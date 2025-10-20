package verif.etrace

import chisel3._
import chisel3.util.{log2Up}

import org.chipsalliance.cde.config.{Parameters, Config, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink.{BootROMParams}
import freechips.rocketchip.diplomacy.{SynchronousCrossing, AsynchronousCrossing, RationalCrossing}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

import boom.ifu._
import boom.exu._
import boom.lsu._
import boom.common._

// DOC include start: TraceTiles
/**
 * Try to copy as much from a working config as possible so it doesn't blow up >_<
 */
class WithNTraces(n: Int = 1) extends Config(
  new WithTAGELBPD ++ // Default to TAGE-L BPD
  new Config((site, here, up) => {
    case TilesLocated(InSubsystem) => {
      val prev = up(TilesLocated(InSubsystem), site)
      val idOffset = up(NumTiles)
      (0 until n).map { i =>
        TraceTileAttachParams(
          tileParams = TraceTileParams(
            core = BoomCoreParams(
              fetchWidth = 8,
              decodeWidth = 3,
              numRobEntries = 96,
              issueParams = Seq(
                IssueParams(issueWidth=1, numEntries=16, iqType=IQT_MEM.litValue, dispatchWidth=3),
                IssueParams(issueWidth=3, numEntries=32, iqType=IQT_INT.litValue, dispatchWidth=3),
                IssueParams(issueWidth=1, numEntries=24, iqType=IQT_FP.litValue , dispatchWidth=3)),
              numIntPhysRegisters = 100,
              numFpPhysRegisters = 96,
              numLdqEntries = 24,
              numStqEntries = 24,
              maxBrCount = 16,
              numFetchBufferEntries = 24,
              ftq = FtqParameters(nEntries=32),
              fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))
            ),
            dcache = Some(
              DCacheParams(rowBits = 128, nSets=16, nWays=2, nMSHRs=1, nTLBWays=16, replacementPolicy="plru") //1 mshr only for in-order dcache accesses 
              //(crying the hardcoding logic needs a workaround for 1 mshr), and then pLRU replacement.
              //rn I've set nSets to 16 instead of 64 to make testing easier :P
            ),
            icache = Some(
              ICacheParams(rowBits = 128, nSets=64, nWays=2, fetchBytes=4*4)
            ),
            tileId = i + idOffset
          ),
          crossingParams = RocketCrossingParams()
        )
      } ++ prev
    }
    case XLen => 64
    case NumTiles => up(NumTiles) + n
  })
)
// DOC include end: TraceConfig
