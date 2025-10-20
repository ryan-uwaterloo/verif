package verif

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.subsystem.RocketCrossingParams
import freechips.rocketchip.tilelink.TLRegisterNode
import parrp_chisel.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import boom.lsu._
import verif.etrace._
import freechips.rocketchip.tile._
import boom.common._


object DefaultTLParams {
  def slave: TLSlavePortParameters = TLSlavePortParameters.v1(
    Seq(
      TLSlaveParameters.v1( // TL-UH master
        address = Seq(AddressSet(0x0, 0xfff)),
        supportsGet = TransferSizes(1, 32),
        supportsPutFull = TransferSizes(1, 32),
        supportsPutPartial = TransferSizes(1, 32),
        supportsLogical = TransferSizes(1, 32),
        supportsArithmetic = TransferSizes(1, 32),
        supportsHint = TransferSizes(1, 32),
        regionType = RegionType.UNCACHED)
    ),
    beatBytes = 8)

  def master(name: String = "TLMasterPort", idRange: IdRange = IdRange(0,3)): TLMasterPortParameters = TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1(name = name, sourceId = idRange)
    ))

  // Temporary cache parameters
  def slaveCache: TLSlavePortParameters = TLSlavePortParameters.v1(
    Seq(
      TLSlaveParameters.v1(
        address = Seq(AddressSet(0x0, 0xfff)),
        supportsGet = TransferSizes(1, 32),
        supportsPutFull = TransferSizes(1, 32),
        supportsPutPartial = TransferSizes(1, 32),
        supportsLogical = TransferSizes(1, 32),
        supportsArithmetic = TransferSizes(1, 32),
        supportsHint = TransferSizes(1, 32),
        supportsAcquireB = TransferSizes(1, 32),
        supportsAcquireT = TransferSizes(1, 32),
        regionType = RegionType.UNCACHED
      )
    ),
    endSinkId = 1, beatBytes = 8)

  def masterCache: TLMasterPortParameters = TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1(
        name = "Core 0 Test Bundle",
        supportsProbe = TransferSizes(1, 32),
        supportsGet = TransferSizes(1, 32),
        supportsPutFull = TransferSizes(1, 32),
        supportsPutPartial = TransferSizes(1, 32),
        supportsLogical = TransferSizes(1, 32),
        supportsArithmetic = TransferSizes(1, 32),
        supportsHint = TransferSizes(1, 32),
        sourceId = IdRange(0, 5)//this is the sourceID range used by our test harness.
      ),
      TLMasterParameters.v1(
        name = "Core 1 Test Bundle",
        supportsProbe = TransferSizes(1, 32),
        supportsGet = TransferSizes(1, 32),
        supportsPutFull = TransferSizes(1, 32),
        supportsPutPartial = TransferSizes(1, 32),
        supportsLogical = TransferSizes(1, 32),
        supportsArithmetic = TransferSizes(1, 32),
        supportsHint = TransferSizes(1, 32),
        sourceId = IdRange(6, 10)//this is the sourceID range used by our test harness.
      )
    ))
}
// Object ParRPTLParams {
//   def slaveCache: TLSlavePortParameters = TLSlavePortParameters.v1(
//     Seq(
//       TLSlaveParameters.v1(
//         address = Seq(AddressSet(0x0, 0xffffffff)),
//         supportsGet = TransferSizes(1, 32),
//         supportsPutFull = TransferSizes(1, 32),
//         supportsPutPartial = TransferSizes(1, 32),
//         supportsLogical = TransferSizes(1, 32),
//         supportsArithmetic = TransferSizes(1, 32),
//         supportsHint = TransferSizes(1, 32),
//         supportsAcquireB = TransferSizes(1, 32),
//         supportsAcquireT = TransferSizes(1, 32),
//         regionType = RegionType.UNCACHED
//       )
//     ),
//     endSinkId = 1, beatBytes = 8  
//   )
// }

class TLRegBankStandalone(
  mPortParams: TLMasterPortParameters = DefaultTLParams.master(),
  concurrency: Int = 1
)(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val device = new SimpleDevice("TLRegBankStandalone", Seq("regbank"))

  val regNode = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)), // TODO: infer address set from beatBytes and size
    device = device,
    beatBytes = 8, // TODO: make beatBytes a parameter
    concurrency = concurrency)

  val bridge = BundleBridgeToTL(mPortParams)
  regNode := bridge
  val ioInNode = BundleBridgeSource(() => TLBundle(TLBundleParameters(mPortParams, regNode.edges.in.head.slave)))
  bridge := ioInNode
  val in = InModuleBody { ioInNode.makeIO() }

  lazy val module = new LazyModuleImp(this) {
    val regs = RegInit(VecInit(Seq.fill(64)(0.U(64.W))))

    val tuples = regs.zipWithIndex.map { case (reg, i) =>
      (0x00 + (i * 8)) -> Seq(RegField(64,reg)) // TODO: randomize types of reg fields
    }
    regNode.regmap(tuples :_*)
  }
}

class TLRAMNoModelStandalone (val mPortParams: TLMasterPortParameters = DefaultTLParams.master(),
    address: AddressSet = AddressSet(0x0, 0x1ff),
    beatBytes: Int = 8,
    cacheable: Boolean = false,
    atomics: Boolean = true,
    sramReg: Boolean = false,
    fragmenterMaxBytes: Int = 32
  ) (implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val ram  = LazyModule(new TLRAM(address, cacheable=cacheable, atomics=atomics, beatBytes=beatBytes, sramReg=sramReg))
  val frag = TLFragmenter(beatBytes, fragmenterMaxBytes)
  val buffer = TLBuffer(BufferParams.default)
  ram.node := frag := buffer

  val bridge = BundleBridgeToTL(mPortParams)
  buffer := bridge
  val ioInNode = BundleBridgeSource(() => TLBundle(TLBundleParameters(mPortParams, bridge.edges.out.head.slave)))
  bridge := ioInNode
  val in = InModuleBody { ioInNode.makeIO() }
  val sPortParams = bridge.edges.out.head.slave

  lazy val module = new LazyModuleImp(this) {}
}

class TLRAMStandalone (
  val mPortParams: TLMasterPortParameters = DefaultTLParams.master(),
  address: AddressSet = AddressSet(0x0, 0x1ff),
  beatBytes: Int = 8,
  cacheable: Boolean = false,
  atomics: Boolean = true,
  sramReg: Boolean = false,
  fragmenterMaxBytes: Int = 32
) (implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val model = LazyModule(new TLRAMModel("TLRAMModel")) // TODO: remove when checkers are mature
  val ram  = LazyModule(new TLRAM(address, cacheable=cacheable, atomics=atomics, beatBytes=beatBytes, sramReg=sramReg))
  val frag = TLFragmenter(beatBytes, fragmenterMaxBytes)
  val buffer = TLBuffer(BufferParams.default)
  ram.node := model.node := frag := buffer

  val bridge = BundleBridgeToTL(mPortParams)
  buffer := bridge
  val ioInNode = BundleBridgeSource(() => TLBundle(TLBundleParameters(mPortParams, bridge.edges.out.head.slave)))
  bridge := ioInNode
  val in = InModuleBody { ioInNode.makeIO() }
  val sPortParams = bridge.edges.out.head.slave

  lazy val module = new LazyModuleImp(this) {}
}

class XBarToRAMStandalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams= TLBundleParameters(mPortParams, sPortParams)

  val model = LazyModule(new TLRAMModel("TLRAMModelXbarSimple"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  val xbar = LazyModule(new TLXbar)

  ram.node := model.node := TLBuffer() := xbar.node
  val TLSlave = xbar.node

  // Standalone Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams))
  val in = InModuleBody { ioInNode.makeIO() }

  TLSlave :=
    BundleBridgeToTL(mPortParams) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}

class XBarToMultiRAMStandalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams = TLBundleParameters(mPortParams, sPortParams)

  // Multi RAM
  val model1 = LazyModule(new TLRAMModel("TLRAMModel1"))
  val ram1  = LazyModule(new TLRAM(AddressSet(0x0, 0xff), cacheable = false, atomics = true, beatBytes = 8))
  val model2 = LazyModule(new TLRAMModel("TLRAMModel2"))
  val ram2  = LazyModule(new TLRAM(AddressSet(0x100, 0xff), cacheable = false, atomics = true, beatBytes = 8))
  val xbar = LazyModule(new TLXbar)
  ram1.node := model1.node := TLBuffer() := xbar.node
  ram2.node := model2.node := TLBuffer() := xbar.node
  val TLSlave = xbar.node

  // RAM Reference
  val model = LazyModule(new TLRAMModel("TLRAMModelReference"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  ram.node := model.node
  val TLReference = model.node

  // Connections
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams))
  val ioInNodeRef = BundleBridgeSource(() => TLBundle(bParams))
  val in = InModuleBody { ioInNode.makeIO() }
  val inRef = InModuleBody { ioInNodeRef.makeIO() }

  TLSlave :=
    BundleBridgeToTL(mPortParams) :=
    ioInNode
  TLReference :=
    BundleBridgeToTL(mPortParams) :=
    ioInNodeRef

  lazy val module = new LazyModuleImp(this) {}
}

// TL Multi-Master Xbar RAM Slave Node Standalone
class XbarToRAMMultiMasterStandalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val mPortParams = Seq(
    DefaultTLParams.master("one", IdRange(0, 4)),
    DefaultTLParams.master("two", IdRange(1, 2))
  )
  val sPortParams = Seq(DefaultTLParams.slave, DefaultTLParams.slave)
  val bParams = (mPortParams zip sPortParams).map{ case (m, s) => TLBundleParameters(m, s)}

  // TLRAM
  val model = LazyModule(new TLRAMModel("TLRAMModel"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x1ff), cacheable = false, atomics = true, beatBytes = 8))
  val xbar = LazyModule(new TLXbar)
  ram.node := model.node := TLBuffer() := xbar.node
  val TLReference = model.node

  // Connections
  val ioInNodeOne = BundleBridgeSource(() => TLBundle(bParams(0)))
  val ioInNodeTwo = BundleBridgeSource(() => TLBundle(bParams(1)))
  val inOne = InModuleBody { ioInNodeOne.makeIO() }
  val inTwo = InModuleBody { ioInNodeTwo.makeIO() }

  xbar.node := TLBuffer() :=
    BundleBridgeToTL(mPortParams(0)) :=
    ioInNodeOne
  xbar.node := TLBuffer() :=
    BundleBridgeToTL(mPortParams(1)) :=
    ioInNodeTwo

  lazy val module = new LazyModuleImp(this) {}
}

// L2 Cache Standalone
class L2Standalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  // First set of PortParams are L1, second set are DRAM
  val mPortParams = Seq(DefaultTLParams.masterCache, DefaultTLParams.master())
  val sPortParams = Seq(DefaultTLParams.slaveCache, DefaultTLParams.slave)
  val bParams = (mPortParams zip sPortParams).map{ case (m, s) => TLBundleParameters(m, s)}

  println(s"mPortParams $mPortParams \n")
  println(s"bParams: $bParams \n")

  // Instantiating L2 Cache (Inclusive Cache)
  val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = 8,
      sets = 4,
      blockBytes = 32,
      beatBytes = 8,
      hintsSkipProbe = false),
    InclusiveCacheMicroParameters(writeBytes = 8),
    None
  ))
  val cork = LazyModule(new TLCacheCork)

  // IO Connections (Master and Slave are directly connected)
  //print(bParams(0))
  val ioInNode = BundleBridgeSource[TLBundle](() => TLBundle(bParams(0)))
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

//  val ioCtrlNode = BundleBridgeSource(() => TLBundle(verifTLBundleParamsC))
//  val ctrl = InModuleBody { ioCtrlNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams(1)) :=
    cork.node :=
    l2.node :=
    BundleBridgeToTL(mPortParams(0)) :=
    ioInNode

//  l2.ctlnode := BundleBridgeToTL(standaloneMasterParamsC) := ioCtrlNode

  lazy val module = new LazyModuleImp(this) {}
}

class TLPatternPusherStandalone(txns: Seq[Pattern])(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule  {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams = TLBundleParameters(mPortParams, sPortParams)

  val patternp = LazyModule(new TLPatternPusher("patternpusher", txns))

  // Standalone Connections
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams) :=
    patternp.node

  lazy val module = new LazyModuleImp(this) {
    val start = RegNext(1.B, 0.B)
    patternp.module.io.run := start
  }
}

class TLFuzzerStandalone(nOperations: Int)(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule  {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams = TLBundleParameters(mPortParams, sPortParams)

  val tlfuzzer = LazyModule(new freechips.rocketchip.tilelink.TLFuzzer(nOperations, inFlight=1))

  // Standalone Connections
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams) :=
    tlfuzzer.node

  lazy val module = new LazyModuleImp(this) {}
}

class TLBufferStandalone(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule  {
  val mPortParams = DefaultTLParams.master()
  val sPortParams = DefaultTLParams.slave
  val bParams = TLBundleParameters(mPortParams, sPortParams)

  val ioInNode = BundleBridgeSource(() => TLBundle(bParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

  ioOutNode :=
    TLToBundleBridge(sPortParams) :=
    TLBuffer() :=
    BundleBridgeToTL(mPortParams) :=
    ioInNode

  lazy val module = new LazyModuleImp(this) {}
}

case object NoHartLookup extends LookupByHartIdImpl {
  def apply[T <: Data](f: TileParams => Option[T], hartId: UInt): T = {
    require(false, "NoHartLookup should never be used to lookup any TileParams")
    null.asInstanceOf[T] // Needed to satisfy the return type, but should never be reached
  }
}

class MulticoreTraceTileHarness(
  val numTiles:       Int = 2,
  val useTLRAM:       Boolean = true,
  val L2ways:         Int = 8,
  val L2sets:         Int = 4,
  val L2blockBytes:   Int = 64,
  val L2beatBytes:    Int = 8
)(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule with BindingScope {

  // Shared memory hierarchy
  val tlxbar = LazyModule(new TLXbar)
  val buffer = LazyModule(new TLBuffer)

  val cork = LazyModule(new TLCacheCork)


  val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = L2ways,
      sets = L2sets,
      blockBytes = L2blockBytes,
      beatBytes = L2beatBytes,
      hintsSkipProbe = false
    ),
    InclusiveCacheMicroParameters(writeBytes = L2beatBytes),
    None
  ))

  val ram = if (useTLRAM) {
    LazyModule(new TLRAM(AddressSet(0x80000000L, 0x0fffffffL), beatBytes = L2beatBytes))
  } else {
    // You can plug in an AXI4 memory model or a Verilator-backed DRAM
    throw new NotImplementedError("Non-TLRAM backend not implemented yet.")
  }

  // Connect: RAM <- Fragmenter <- Buffer <- L2 <- Buffer <- Xbar
  ram.node := 
    TLFragmenter(L2beatBytes, maxSize = 64) :=
    cork.node := 
    buffer.node :=
    l2.node :=
    TLBuffer() :=
    tlxbar.node

  val intSource = IntSourceNode(IntSourcePortSimple(num = numTiles, sources = numTiles))//sources for diplomatic connections
  val hartIdSource = Seq.fill(numTiles)(BundleBridgeSource(() => UInt(64.W)))
  val resetVectorSources = Seq.fill(numTiles)(BundleBridgeSource[UInt](() => UInt(32.W)))

  val intXbar = LazyModule(new IntXbar) //xbars for diplomatic connections
  // val resetVectorNexus = BundleBridgeNexusNode[UInt]()

  intXbar.intnode :=* intSource //source :=* xbar to bind multi-output and make diplomacy happy
  // resetVectorNexus := resetVectorSource

  InModuleBody { //drive all the diplomatic things in the module
    val dummyVec = Wire(Vec(16, Bool()))//I'm not sure how to make this happy dynamically without something else blowing up
    dummyVec.foreach(_ := false.B)
    intSource.out.head._1 := dummyVec
    // resetVectorSource.bundle := 0.U
  }

  //fifos for reqs from cosim
  // lazy val dummyTile = LazyModule(new TraceTile(TraceTileParams(tileId = 0), RocketCrossingParams(), NoHartLookup))
  // val dcacheBundleType = chiselTypeOf(dummyTile.dcacheFifoNode.bundle)
  // val icacheBundleType = chiselTypeOf(dummyTile.icacheFifoNode.bundle)

  // val dcacheFifosNode = Seq.fill(numTiles)(BundleBridgeSource(() => TraceTile.makeDCacheFifoBundle()))
  // val icacheFifosNode = Seq.fill(numTiles)(BundleBridgeSource(() => TraceTile.makeICacheFifoBundle()))

  // val dcacheFifosNode = Seq.fill(numTiles)(BundleBridgeSink[DecoupledIO[BoomDCacheReq]]())
  val dcacheFifosNode = Seq.fill(numTiles)(BundleBridgeSink[TraceIO]())
  val icacheFifosNode = Seq.fill(numTiles)(BundleBridgeSink[TraceIO]())

  // Instantiate cores and connect to Xbar
  val tiles: Seq[TraceTile] = Seq.tabulate(numTiles) { id =>
    val tile = LazyModule(new TraceTile(TraceTileParams(tileId = id), RocketCrossingParams(), NoHartLookup) with CanAccessInterrupts)
    tlxbar.node := TLWidthWidget(L2beatBytes) := tile.masterNode
    tile.interruptNode := intXbar.intnode
    tile.hartIdNode := hartIdSource(id)
    tile.resetVectorNode := resetVectorSources(id)
    dcacheFifosNode(id) := tile.dcacheFifoNode
    icacheFifosNode(id) := tile.icacheFifoNode
    tile
  }

  class ModuleImpl(outer: MulticoreTraceTileHarness) extends LazyModuleImp(outer) {
    // val dcachefifos = outer.tiles.zipWithIndex.map{
    //   case (tile, i) => 
    //   outer.dcacheFifosNode(i).makeIO()(ValName(s"dcache_in_$i"))
    // }
    // val icachefifos = outer.tiles.zipWithIndex.map{
    //   case (tile, i) => 
    //   outer.icacheFifosNode(i).makeIO()(ValName(s"icache_in_$i"))
    // }

    val dcache_io = outer.dcacheFifosNode.zipWithIndex.map { case (n, i) =>
      n.makeIO(s"dcache_io_$i")
    }
    val interposer = Wire(Vec(numTiles, chiselTypeOf(dcache_io(0))))
    for (i <- 0 until numTiles){

    }
    outer.dcacheFifosNode.zipWithIndex.map { case (n, i) =>
      dcache_io(i) <> interposer(i) 
      interposer(i) <> n.bundle
    }

    // val dcache_io_0 = outer.dcacheFifosNode(0).makeIO(s"dcache_io_0")
    // val interposer_dcache_io_0 = Wire(chiselTypeOf(dcache_io_0))
    // dontTouch(interposer_dcache_io_0)
    // dcache_io_0 <> interposer_dcache_io_0 
      // val dcacheFifos = outer.dcacheFifosNode.bundle
      // val icacheFifos = Vec(outer.numTiles, Decoupled(new BoomDCacheReq))

    // val icache_io_0 = outer.icacheFifosNode(0).makeIO(s"icache_io_0")
    val icache_io = outer.icacheFifosNode.zipWithIndex.map { case (n, i) =>
      n.makeIO(s"icache_io_$i")
    }
    outer.icacheFifosNode.zipWithIndex.map { case (n, i) =>
      icache_io(i) <> n.bundle
    }

    // for (i <- 0 until numTiles){
    //   icache_io(i) <> outer.icacheFifosNode(i).bundle
    //   dcache_io(i) <> outer.dcacheFifosNode(i).bundle
    // }
    

    for ((tile, i) <- outer.tiles.zipWithIndex) {
      outer.hartIdSource(i).bundle := i.U
      outer.resetVectorSources(i).bundle := 0.U
      // outer.dcacheFifosNode(i).makeIO()(ValName(s"dcache_in_$i"))
      // outer.icacheFifosNode(i).makeIO()(ValName(s"icache_in_$i"))
    }
  }

  // instantiate the concrete ModuleImpl
  override lazy val module = new ModuleImpl(this)
}
