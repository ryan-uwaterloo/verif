//******************************************************************************
// Copyright (c) 2017 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package verif.etrace

import chisel3._
import chisel3.util._

import scala.collection.mutable.{ListBuffer}

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._

import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.{RocketCrossingParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

import boom.exu._
import boom.ifu._
import boom.lsu._
import boom.util.{BoomCoreStringPrefix}
import boom.common._
import freechips.rocketchip.prci.ClockSinkParameters

trait CanAccessInterrupts { this: TraceTile =>
  def interruptNode = intXbar.intnode
}

case class TraceTileAttachParams(
  tileParams: TraceTileParams,
  crossingParams: RocketCrossingParams
) extends CanAttachTile {
  type TileType = TraceTile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))
}

class AckBundle extends Bundle{
  val load_n_store = Bool()
  val addr = UInt(32.W)
}

class TraceIO(implicit p: Parameters) extends Bundle{
  val in = Flipped(Decoupled(new BoomDCacheReq))
  val out = Valid(new AckBundle)
}


/**
 * BOOM tile parameter class used in configurations
 *
 */
case class TraceTileParams(
  core: BoomCoreParams = BoomCoreParams(),
  icache: Option[ICacheParams] = Some(ICacheParams(blockBytes = 32)),
  dcache: Option[DCacheParams] = Some(DCacheParams(blockBytes = 32)),
  btb: Option[BTBParams] = Some(BTBParams()),
  name: Option[String] = Some("trace_tile"),
  tileId: Int = 0
) extends InstantiableTileParams[TraceTile]
{
  require(icache.isDefined)
  require(dcache.isDefined)
  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): TraceTile = {
    new TraceTile(this, crossing, lookup)
  }
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
  val clockSinkParams: ClockSinkParameters = ClockSinkParameters()
  val baseName = name.getOrElse("trace_tile")
  val uniqueName = s"${baseName}_$tileId"
}

/**
 * Trace tile
 *
 */
class TraceTile private(
  val boomParams: TraceTileParams,
  crossing: ClockCrossingType,
  lookup: LookupByHartIdImpl,
  q: Parameters)
  extends BaseTile(boomParams, crossing, lookup, q)
  with SinksExternalInterrupts
  with SourcesExternalNotifications
{

  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: TraceTileParams, crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = None
  lazy val masterNode = TLIdentityNode()
  lazy val slaveNode = TLIdentityNode()

  // val dcacheFifoNode = BundleBridgeSink[DecoupledIO[BoomDCacheReq]]()
  // val icacheFifoNode = BundleBridgeSink[DecoupledIO[BoomDCacheReq]]()
  // val dcacheFifoNode = BundleBridgeSource(() => Flipped(DecoupledIO(new BoomDCacheReq)))
  val dcacheFifoNode = BundleBridgeSource(() => new TraceIO)
  val icacheFifoNode = BundleBridgeSource(() => new TraceIO)

  // type DCacheReqType = BoomDCacheReq

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("uw-caesr,etrace", "etrace")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++
                        cpuProperties ++
                        nextLevelCacheProperty ++
                        tileProperties)
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(tileId))
  }

  override lazy val module = new TraceTileModuleImp(this)

  // DCache
  lazy val dcache: BoomNonBlockingDCache = LazyModule(new BoomNonBlockingDCache(tileId))
  val dCacheTap = TLIdentityNode()
  tlMasterXbar.node := dCacheTap := TLWidthWidget(tileParams.dcache.get.rowBits/8) := visibilityNode := dcache.node

  // Frontend/ICache
  lazy val icache: BoomNonBlockingDCache = LazyModule(new BoomNonBlockingDCache(tileId))
  val iCacheTap = TLIdentityNode()
  tlMasterXbar.node := iCacheTap := TLWidthWidget(tileParams.dcache.get.rowBits/8) := visibilityNode := icache.node

//   val frontend = LazyModule(new BoomFrontend(tileParams.icache.get, tileId))
//   frontend.resetVectorSinkNode := resetVectorNexusNode
//   tlMasterXbar.node := TLWidthWidget(tileParams.icache.get.rowBits/8) := frontend.masterNode

  println(s"dcache params: ${tileParams.dcache}\n")
  // println(s"dcache params: ${tileParams.icache}\n")
  // require(tileParams.dcache.get.rowBits == tileParams.icache.get.rowBits)
}

/**
 * Trace tile implementation
 *
 * @param outer top level BOOM tile
 */
class TraceTileModuleImp(outer: TraceTile) extends BaseTileModuleImp(outer){

  Annotated.params(this, outer.boomParams)

  //val core = Module(new BoomCore()(outer.p))
  val lsu  = Module(new TraceLSU()(outer.p, outer.dcache.module.edge))
  val i_lsu = Module(new TraceLSU()(outer.p, outer.icache.module.edge))

  // lsu.io.ptw := 0.U.asTypeOf(TLBPTWIO()(outer.p))
  // lsu.io.core := 0.U.asTypeOf(LSUCoreIO()(outer.p))
  // lsu.io.dmem := 0.U.asTypeOf(LSUDMemIO()(outer.p))

  //trace fifos
  val dcache_fifo = Module(new TraceFifo(new BoomDCacheReq, 1))
  val icache_fifo = Module(new TraceFifo(new BoomDCacheReq, 1))

  // Connect the fifo output to their modules :)
  dcache_fifo.io.deq <> lsu.io.fifo
  icache_fifo.io.deq <> i_lsu.io.fifo

  // Connect the formatted requests to the caches
  outer.dcache.module.io.lsu <> lsu.io.dmem
  outer.icache.module.io.lsu <> i_lsu.io.dmem

  // val dcacheFifoBundle = outer.dcacheFifoNode.bundle
  // dontTouch(dcacheFifoBundle)
  // val icacheFifoBundle = outer.icacheFifoNode.bundle

  // Connect to the FIFO inputs
  dcache_fifo.io.enq <> outer.dcacheFifoNode.bundle.in
  icache_fifo.io.enq <> outer.icacheFifoNode.bundle.in
  dontTouch(dcache_fifo.io.enq)

  outer.dcacheFifoNode.bundle.out <> lsu.io.ack
  outer.icacheFifoNode.bundle.out <> i_lsu.io.ack
}

// object TraceTile{
//   private implicit val localP = (new WithoutTLMonitors).alterMap(Map(
//     TileKey -> TraceTileParams(),
//     TileVisibilityNodeKey -> TLEphemeralNode()(ValName("tile_master")),
//     LookupByHartId -> 0,
//     XLen -> 64
//   ))
//   def makeDCacheFifoBundle()(implicit p: Parameters): DecoupledIO[BoomDCacheReq] =
//     DecoupledIO(new BoomDCacheReq()(localP))
//   def makeICacheFifoBundle()(implicit p: Parameters): DecoupledIO[BoomDCacheReq] =
//     DecoupledIO(new BoomDCacheReq()(localP))
// }


