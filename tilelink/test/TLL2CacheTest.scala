package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
//import chiseltest.experimental.TestOptionBuilder._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.WithoutTLMonitors
import TLTransaction._
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleB, TLBundleC, TLBundleD, TLBundleE}
import freechips.rocketchip.diplomacy._
import chisel3._


class TLL2CacheTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "Elaborate L2" in {
    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      val L1PortParams = TLL2.in.params
      val DRAMPortParams = TLL2.out.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.init(Map(0L -> 0x1234, 1L -> 0x3333), TLL2.out.params.dataBits/8))

      L1Placeholder.push(Seq(AcquireBlock(TLPermission.Grow.NtoT, 0x8, 5)(TLL2.in.params)))

      c.clock.step(200)

      val output1 = L1Monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val output2 = DRAMMonitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}

//      println("INNER (CORE)")
//      for (t <- monitor.getMonitoredTransactions()) {
//        println(t)
//      }
//      println("OUTER (DRAM)")
//      for (t <- monitor1.getMonitoredTransactions()) {
//        println(t)
//      }
    }
  }

  // Ignoring test as new driver is no longer TLC compliance
  it should "Driver TLC Compliance Test" in {
    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      val L1PortParams = TLL2.in.params
      val DRAMPortParams = TLL2.out.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(DRAMPortParams)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val txns = Seq(
        // Two Acquires in a row, must be sequential
        AcquireBlock(TLPermission.Grow.NtoT, 0x0, 3)(L1PortParams),
        AcquireBlock(TLPermission.Grow.NtoB, 0x20, 3)(L1PortParams),
        // Cannot acquire until release completes
        ReleaseData(TLPermission.PruneOrReport.TtoB, 0x20, 0x0, 3, 0)(L1PortParams),
        AcquireBlock(TLPermission.Grow.NtoT, 0x40, 3)(L1PortParams),
        // L2 with sets = 2 will evict a block after third Acquire
      )

      val fuzz = new TLCFuzzer(L1PortParams, None, txns, 3)

      for (_<- 0 until 20) {
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(5)
      }

      val output1 = L1Monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
      val output2 = DRAMMonitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
    }
  }

  it should "L2 SWTLFuzzer" in {

    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
      val fuzz = new TLCFuzzer(params, Some(gen), cacheBlockSize = 5)

      for (_ <- 0 until 50) {
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(5)
        //print(txns.toString())
      }

      for (_ <- 0 until 200){
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

      val output1 = L1Monitor.getMonitoredTransactions().map(_.data).collect{ case t: TLBundleD => t}
    }
  }
  it should "L2 txns from file" in {

    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
    
      val txnFile = getClass.getResourceAsStream("/L2TestFile.csv")

      val tx_list = TLUtils.CSVtoTL(txnFile, params)
      val fuzz = new TLCFuzzer(params, None, tx_list, cacheBlockSize = 5, IdRange(0, 10))

      for (i <- 0 until (tx_list.length * 10)) {
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        for (j <- 0 until 10){ // incr. 10 clock cycles
          c.clock.step(1)
        }
      }
    }
  }

  it should "L2_formal_phy_hit_parrp_miss" in {

    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
    
      val txnFile = getClass.getResourceAsStream("/L2Formal_PhyHitParrpMiss.csv")

      val tx_list = TLUtils.CSVtoTL(txnFile, params)
      val fuzz = new TLCFuzzer(params, None, tx_list, cacheBlockSize = 5, IdRange(0, 10))

      for (i <- 0 until (tx_list.length * 10)) {
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        for (j <- 0 until 10){ // incr. 10 clock cycles
          c.clock.step(1)
        }
      }

      for (i <- 0 until 200){ //just step the clock a bunch to run out pending txns
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

    }
  }

  it should "L2_formal_nest_release_same_core" in {

    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
    
      val txnFile = getClass.getResourceAsStream("/L2Formal_NestRelSameCore.csv")

      val tx_list = TLUtils.CSVtoTL(txnFile, params)
      val fuzz = new TLCFuzzer(params, None, tx_list, cacheBlockSize = 5, IdRange(0, 10))

      for (i <- 0 until (tx_list.length * 10)) {
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        println(s"pushing txn: $txns")
        L1Placeholder.push(txns)
        for (j <- 0 until 5){ // incr. 5 clock cycles to queue more
          c.clock.step(1)
        }
      }

      for (i <- 0 until 200){ //just step the clock a bunch to run out pending txns
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

    }
  }

  it should "L2_formal_capacity_eviction" in {

    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
    
      val txnFile = getClass.getResourceAsStream("/L2Formal_CapacityEvict.csv")

      val tx_list = TLUtils.CSVtoTL(txnFile, params)
      val fuzz = new TLCFuzzer(params, None, tx_list, cacheBlockSize = 5, IdRange(0, 10))

      for (i <- 0 until (tx_list.length * 10)) {
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        for (j <- 0 until 10){ // incr. 5 clock cycles to queue more
          c.clock.step(1)
        }

      }

      for (i <- 0 until 200){ //just step the clock a bunch to run out pending txns
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

    }
  }

  it should "L2_formal_nest_release_diff_core" in {

    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
    
      val txnFile = getClass.getResourceAsStream("/L2Formal_NestRelDiffCore.csv")

      val tx_list = TLUtils.CSVtoTL(txnFile, params)
      val tx_list_1 = tx_list.slice(0, 7)
      val tx_list_2 = tx_list.slice(8, 17)
      val fuzz_1 = new TLCFuzzer(params, None, tx_list_1, cacheBlockSize = 5, IdRange(0, 10))
      val fuzz_2 = new TLCFuzzer(params, None, tx_list_2, cacheBlockSize = 5, IdRange(0, 10))

      for (i <- 0 until (tx_list_1.length * 10)) {
        val txns = fuzz_1.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        for (j <- 0 until 5){ // incr. 5 clock cycles to queue more
          c.clock.step(1)
        }
      }

      for (i <- 0 until 200){ //just step the clock a bunch to run out pending txns
        val txns = fuzz_1.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

      for (i <- 0 until (tx_list_2.length * 20)) { //start these transactions partway through
        val txns = fuzz_2.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        for (j <- 0 until 1){ // incr. 5 clock cycles to queue more
          c.clock.step(1)
        }
      }

      for (i <- 0 until 200){ //just step the clock a bunch to run out pending txns
        val txns = fuzz_2.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

    }
  }

  it should "L2_formal_two_released_ways" in {

    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
    
      val txnFile = getClass.getResourceAsStream("/L2Formal_2ReleasedWays.csv")

      val tx_list = TLUtils.CSVtoTL(txnFile, params)
      val fuzz = new TLCFuzzer(params, None, tx_list, cacheBlockSize = 5, IdRange(0, 10))

      for (i <- 0 until (tx_list.length * 10)) {
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        for (j <- 0 until 5){ // incr. 5 clock cycles to queue more
          c.clock.step(1)
        }

      }

      for (i <- 0 until 200){ //just step the clock a bunch to run out pending txns
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

    }
  }

  it should "L2_formal_shared_way" in {

    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
    
      val txnFile = getClass.getResourceAsStream("/L2Formal_SharedWay.csv")

      val tx_list = TLUtils.CSVtoTL(txnFile, params)
      val tx_list_1 = tx_list.slice(0, 7)
      val tx_list_2 = tx_list.slice(8, 21)
      val fuzz_1 = new TLCFuzzer(params, None, tx_list_1, cacheBlockSize = 5, IdRange(0, 10))
      val fuzz_2 = new TLCFuzzer(params, None, tx_list_2, cacheBlockSize = 5, IdRange(0, 10))

      for (i <- 0 until (tx_list_1.length * 10)) {
        val txns = fuzz_1.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        for (j <- 0 until 5){ // incr. 5 clock cycles to queue more
          c.clock.step(1)
        }
      }

      for (i <- 0 until 200){ //just step the clock a bunch to run out pending txns
        val txns = fuzz_1.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

      for (i <- 0 until (tx_list_2.length * 10)) { //start these transactions partway through
        val txns = fuzz_2.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        for (j <- 0 until 5){ // incr. 5 clock cycles to queue more
          c.clock.step(1)
        }
      }

      for (i <- 0 until 200){ //just step the clock a bunch to run out pending txns
        val txns = fuzz_2.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

    }
  }

  it should "L2_formal_probe" in {

    val TLL2 = LazyModule(new L2Standalone)
    test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      implicit val params = TLL2.in.params

      val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
      val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
      val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
      val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
      val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
      val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

      val slaveFn = new TLMemoryModel(TLL2.out.params)
      val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

      val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
    
      val txnFile = getClass.getResourceAsStream("/L2Formal_Probe.csv")

      val tx_list = TLUtils.CSVtoTL(txnFile, params)
      val fuzz = new TLCFuzzer(params, None, tx_list, cacheBlockSize = 5, IdRange(0, 10))

      for (i <- 0 until (tx_list.length * 10)) {
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        for (j <- 0 until 5){ // incr. 5 clock cycles to queue more
          c.clock.step(1)
        }

      }

      for (i <- 0 until 200){ //just step the clock a bunch to run out pending txns
        val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
        L1Placeholder.push(txns)
        c.clock.step(1)
      }

    }
  }

  // FYI: non-coherent processing is kinda scuffed and requires for the requester to be MMIO which is a big hassle in this verification (requires a new traffic generator I don't want to make)....
  // it should "L2_formal_non_coherent_request" in {

  //   val TLL2 = LazyModule(new L2Standalone)
  //   test(TLL2.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
  //     implicit val params = TLL2.in.params

  //     val L1Placeholder = new TLDriverMaster(c.clock, TLL2.in)
  //     val FuzzMonitor = new TLMonitor(c.clock, TLL2.in)
  //     val L1ProtocolChecker = new TLProtocolChecker(TLL2.mPortParams.head, TLL2.sPortParams.head)
  //     val L1Monitor = new TLMonitor(c.clock, TLL2.in, Some(L1ProtocolChecker))
  //     val DRAMProtocolChecker = new TLProtocolChecker(TLL2.mPortParams(1), TLL2.sPortParams(1))
  //     val DRAMMonitor = new TLMonitor(c.clock, TLL2.out, Some(DRAMProtocolChecker))

  //     val slaveFn = new TLMemoryModel(TLL2.out.params)
  //     val DRAMPlaceholder = new TLDriverSlave(c.clock, TLL2.out, slaveFn, TLMemoryModel.State.empty())

  //     val gen = new TLTransactionGenerator(TLL2.sPortParams.head, TLL2.in.params, overrideAddr = Some(AddressSet(0x00, 0x1ff)), get = false, putFull = false, putPartial = false, burst = true, arith = false, logic = false, hints = false, tlc = true, cacheBlockSize = 5, acquire = true)
    
  //     val txnFile = getClass.getResourceAsStream("/L2Formal_NonCoherentRequest.csv")

  //     val tx_list = TLUtils.CSVtoTL(txnFile, params)
  //     val fuzz = new TLCFuzzer(params, None, tx_list, cacheBlockSize = 5, IdRange(0, 10))

  //     for (i <- 0 until (tx_list.length * 10)) {
  //       val txns = fuzz.next(FuzzMonitor.getMonitoredTransactions().map({_.data}))
  //       L1Placeholder.push(txns)
  //       for (j <- 0 until 5){ // incr. 5 clock cycles to queue more
  //         c.clock.step(1)
  //       }

  //     }

  //     for (i <- 0 until 100){ //just step the clock a bunch to run out pending txns
  //       c.clock.step(1)
  //     }

  //   }
  // }

}

