package verif

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import collection._
//import chiseltest.experimental.TestOptionBuilder._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors
import TLTransaction._
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleB, TLBundleC, TLBundleD, TLBundleE}
import org.chipsalliance.cde.config.Parameters


class Basic() extends Module {
    val io = IO(new Bundle {
        val in = Input(SInt(8.W))
        val out = Output(SInt(8.W))
    })
    val regs = RegInit((0.U(64.W)))

    io.out := io.in
}


class SanityTest extends AnyFlatSpec with ChiselScalatestTester {
  "Basic" should "not explode" in {
    //val length = 4  // Number of stages in the FIR
    //val width = 8   // Bit-width of data

    test(new Basic()) { dut =>
      // Step 1: Apply input and step through the FIR
    //   dut.io.in.poke(1.U)
      dut.clock.step(1)
    }
  }
}