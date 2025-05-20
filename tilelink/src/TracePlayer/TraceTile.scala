//******************************************************************************
// Copyright (c) 2017 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package verif.etrace

import chisel3._
import chisel3.util.{RRArbiter, Queue}

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


case class TraceTileAttachParams(
  tileParams: TraceTileParams,
  crossingParams: RocketCrossingParams
) extends CanAttachTile {
  type TileType = TraceTile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))
}


/**
 * BOOM tile parameter class used in configurations
 *
 */
case class TraceTileParams(
  core: BoomCoreParams = BoomCoreParams(),
  icache: Option[ICacheParams] = Some(ICacheParams()),
  dcache: Option[DCacheParams] = Some(DCacheParams()),
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
 * BOOM tile
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
  val masterNode = TLIdentityNode()
  val slaveNode = TLIdentityNode()

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
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
  tlMasterXbar.node := iCacheTap := TLWidthWidget(tileParams.icache.get.rowBits/8) := visibilityNode := icache.node

//   val frontend = LazyModule(new BoomFrontend(tileParams.icache.get, tileId))
//   frontend.resetVectorSinkNode := resetVectorNexusNode
//   tlMasterXbar.node := TLWidthWidget(tileParams.icache.get.rowBits/8) := frontend.masterNode

  require(tileParams.dcache.get.rowBits == tileParams.icache.get.rowBits)
}

/**
 * BOOM tile implementation
 *
 * @param outer top level BOOM tile
 */
class TraceTileModuleImp(outer: TraceTile) extends BaseTileModuleImp(outer){

  Annotated.params(this, outer.boomParams)

  //val core = Module(new BoomCore()(outer.p))
  val lsu  = Module(new TraceLSU()(outer.p, outer.dcache.module.edge))
  val i_lsu = Module(new TraceLSU()(outer.p, outer.icache.module.edge))

  //trace fifos
  val dcache_fifo = Module(new TraceFifo(new BoomDCacheReq, 2))
  val icache_fifo = Module(new TraceFifo(new BoomDCacheReq, 2))

  //val ptwPorts         = ListBuffer(lsu.io.ptw, outer.frontend.module.io.ptw, core.io.ptw_tlb)

  //val hellaCachePorts  = ListBuffer[HellaCacheIO]()

//   outer.reportWFI(None) // TODO: actually report this?

//   outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector

  // Pass through various external constants and reports
//   outer.traceSourceNode.bundle <> core.io.trace
//   outer.bpwatchSourceNode.bundle <> DontCare // core.io.bpwatch
  //core.io.hartid := outer.hartIdSinkNode.bundle

  // Connect the fifos to their modules :)
  icache_fifo.io <> i_lsu.io.fifo
  dcache_fifo.io <> lsu.io.fifo

  // PTW
//   val ptw  = Module(new PTW(ptwPorts.length)(outer.dcache.node.edges.out(0), outer.p))
//   core.io.ptw <> ptw.io.dpath
//   ptw.io.requestor <> ptwPorts.toSeq
//   ptw.io.mem +=: hellaCachePorts

   // LSU IO
//   val hellaCacheArb = Module(new HellaCacheArbiter(hellaCachePorts.length)(outer.p))
//   hellaCacheArb.io.requestor <> hellaCachePorts.toSeq
//   lsu.io.hellacache <> hellaCacheArb.io.mem
outer.dcache.module.io.lsu <> lsu.io.dmem
outer.icache.module.io.lsu <> i_lsu.io.dmem

  // Generate a descriptive string
//   val frontendStr = outer.frontend.module.toString
//   val coreStr = core.toString
//   val boomTileStr =
//     (BoomCoreStringPrefix(s"======TRACE Tile ${outer.tileId} Params======") + "\n"
//     + frontendStr
//     + coreStr + "\n")

//   override def toString: String = boomTileStr

//   print(boomTileStr)
}
