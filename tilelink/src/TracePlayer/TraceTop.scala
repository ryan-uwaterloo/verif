// package verif.etrace

// import chisel3._
// import chisel3.util._
// import org.chipsalliance.cde.config.Parameters
// import freechips.rocketchip.diplomacy._
// import freechips.rocketchip.tilelink._
// import freechips.rocketchip.regmapper._
// import freechips.rocketchip.interrupts._
// import freechips.rocketchip.subsystem.WithoutTLMonitors
// import freechips.rocketchip.subsystem.RocketCrossingParams
// import freechips.rocketchip.tilelink.TLRegisterNode
// import parrp_chisel.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
// import boom.lsu._
// import verif._


// class NTraceTilesTop(numTiles: Int = 2,
//                      useTLRAM: Boolean = true,
//                      L2ways: Int = 8,
//                      L2sets: Int = 4,
//                      L2blockBytes: Int = 32,
//                      L2beatBytes: Int = 8)
//                     (implicit p: Parameters) extends ChipTop {

//   override lazy val desiredName = "ChipTop"

//   val tlxbar = LazyModule(new TLXbar)
//   val buffer = LazyModule(new TLBuffer)
//   val cork   = LazyModule(new TLCacheCork)

//   val l2 = LazyModule(new InclusiveCache(
//     CacheParameters(
//       level = 2,
//       ways = L2ways,
//       sets = L2sets,
//       blockBytes = L2blockBytes,
//       beatBytes = L2beatBytes,
//       hintsSkipProbe = false
//     ),
//     InclusiveCacheMicroParameters(writeBytes = L2beatBytes),
//     None
//   ))

//   val ram = if (useTLRAM) {
//     LazyModule(new TLRAM(AddressSet(0x80000000L, 0x0fffffffL), beatBytes = L2beatBytes))
//   } else {
//     throw new NotImplementedError("Non-TLRAM backend not implemented yet.")
//   }

//   ram.node :=
//     TLFragmenter(L2beatBytes, maxSize = 64) :=
//     cork.node :=
//     buffer.node :=
//     l2.node :=
//     TLBuffer() :=
//     tlxbar.node

//   val intSource = IntSourceNode(IntSourcePortSimple(num = numTiles, sources = numTiles))

//   InModuleBody {
//     val dummyVec = Wire(Vec(numTiles, Bool()))
//     dummyVec.foreach(_ := false.B)
//     intSource.out.head._1 := dummyVec
//   }

//   val tiles = Seq.tabulate(numTiles) { id =>
//     val tile = LazyModule(new TraceTileStandalone(id))
//     tlxbar.node := TLWidthWidget(L2beatBytes) := tile.tile.masterNode
//     tile.tile.interruptNode := intSource
//     tile
//   }

//   lazy val module = new LazyModuleImp(this) {
//     val io = IO(new Bundle {
//       val dcacheFifos = Vec(numTiles, Flipped(Decoupled(new BoomDCacheReq)))
//       val icacheFifos = Vec(numTiles, Flipped(Decoupled(new BoomDCacheReq)))
//     })
//     for ((tile, i) <- tiles.zipWithIndex) {
//       io.dcacheFifos(i) <> tile.dcacheFifoIO
//       io.icacheFifos(i) <> tile.icacheFifoIO
//     }
//   }
// }
