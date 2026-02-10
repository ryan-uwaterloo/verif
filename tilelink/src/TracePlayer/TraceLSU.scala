package verif.etrace

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.Str
import freechips.rocketchip.util.MuxT

import boom.common._
import boom.exu.{BrUpdateInfo, Exception, FuncUnitResp, CommitSignals, ExeUnitResp}
import boom.util.{BoolToChar, AgePriorityEncoder, IsKilledByBranch, GetNewBrMask, WrapInc, IsOlder, UpdateBrMask}
import boom.lsu._

//all these classes are declared in BOOM hierarchy
// class LSUExeIO(implicit p: Parameters) extends BoomBundle()(p)
// {
//   // The "resp" of the maddrcalc is really a "req" to the LSU
//   val req       = Flipped(new ValidIO(new FuncUnitResp(xLen)))
//   // Send load data to regfiles
//   val iresp    = new DecoupledIO(new boom.exu.ExeUnitResp(xLen))
//   val fresp    = new DecoupledIO(new boom.exu.ExeUnitResp(xLen+1)) // TODO: Should this be fLen?
// }

// class BoomDCacheReq(implicit p: Parameters) extends BoomBundle()(p)
//   with HasBoomUOP//does this add a uop field? traiting is so very cringe and hard to follow if you didn't write the traits
// {
//   val addr  = UInt(coreMaxAddrBits.W)
//   val data  = Bits(coreDataBits.W)
//   val is_hella = Bool() // Is this the hellacache req? If so this is not tracked in LDQ or STQ
//   //hijack is_hella to store load/store status of request ^_^
// }

// class BoomDCacheResp(implicit p: Parameters) extends BoomBundle()(p)
//   with HasBoomUOP
// {
//   val data = Bits(coreDataBits.W)
//   val is_hella = Bool()
// }

// class LSUDMemIO(implicit p: Parameters, edge: TLEdgeOut) extends BoomBundle()(p)
// {
//   // In LSU's dmem stage, send the request
//   val req         = new DecoupledIO(Vec(memWidth, Valid(new BoomDCacheReq)))
//   // In LSU's LCAM search stage, kill if order fail (or forwarding possible)
//   val s1_kill     = Output(Vec(memWidth, Bool()))
//   // Get a request any cycle
//   val resp        = Flipped(Vec(memWidth, new ValidIO(new BoomDCacheResp)))
//   // In our response stage, if we get a nack, we need to reexecute
//   val nack        = Flipped(Vec(memWidth, new ValidIO(new BoomDCacheReq)))

//   val brupdate       = Output(new BrUpdateInfo)
//   val exception    = Output(Bool())
//   val rob_pnr_idx  = Output(UInt(robAddrSz.W))
//   val rob_head_idx = Output(UInt(robAddrSz.W))

//   val release = Flipped(new DecoupledIO(new TLBundleC(edge.bundle)))

//   // Clears prefetching MSHRs
//   val force_order  = Output(Bool())
//   val ordered     = Input(Bool())

//   val perf = Input(new Bundle {
//     val acquire = Bool()
//     val release = Bool()
//   })

// }

class TLSUIO(implicit p: Parameters, edge: TLEdgeOut) extends BoomBundle()(p)
{
  //val ptw   = new rocket.TLBPTWIO
  val dmem  = new LSUDMemIO
  val fifo = Flipped(Decoupled(new BoomDCacheReq))
  val ack = Output(Valid(new AckBundle))
  //val hellacache = Flipped(new freechips.rocketchip.rocket.HellaCacheIO)
}

class TraceLSU(implicit p: Parameters, edge: TLEdgeOut) extends BoomModule()(p)
  with rocket.HasL1HellaCacheParameters
{
  val io = IO(new TLSUIO)
  // io.hellacache := DontCare
  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))
  // val h_ready :: h_s1 :: h_s2 :: h_s2_nack :: h_wait :: h_replay :: h_dead :: Nil = Enum(7)
  // s1 : do TLB, if success and not killed, fire request go to h_s2
  //      store s1_data to register
  //      if tlb miss, go to s2_nack
  //      if don't get TLB, go to s2_nack
  //      store tlb xcpt
  // s2 : If kill, go to dead
  //      If tlb xcpt, send tlb xcpt, go to dead
  // s2_nack : send nack, go to dead
  // wait : wait for response, if nack, go to replay
  // replay : refire request, use already translated address
  // dead : wait for response, ignore it

  io.dmem.force_order   := false.B

  // defaults
  io.dmem.brupdate       := DontCare 
  io.dmem.brupdate.b2.valid := false.B //this should remove all branching from dcache
  io.dmem.brupdate.b2.target_offset := 0.S
  io.dmem.exception      := false.B
  io.dmem.rob_head_idx   := DontCare //only for implementing a fence
  io.dmem.rob_pnr_idx    := DontCare //dontCare?
  io.dmem.release.ready  := true.B

  io.ack.valid := false.B
  io.ack.bits.addr := DontCare
  io.ack.bits.load_n_store := false.B

  val dmem_req = Wire(Vec(memWidth, Valid(new BoomDCacheReq)))
  val dmem_req_valid = dmem_req.map(_.valid).reduce(_||_) //have req ready in fifo
  io.dmem.req.valid := dmem_req_valid
  io.dmem.req.bits  := dmem_req //fifo

  val dmem_resp_fired_reg = RegNext(io.dmem.resp(0).valid)

  for (w <- 0 until memWidth) {
    dmem_req(w).valid := false.B
    dmem_req(w).bits.uop   := NullMicroOp()
    dmem_req(w).bits.addr  := 0.U
    dmem_req(w).bits.data  := 0.U
    dmem_req(w).bits.is_hella := false.B

    io.dmem.s1_kill(w) := false.B
  }

  val dmem_resp_fired = WireInit(widthMap(w => false.B))

  //crush this state machine into a fifo
  val wait_nack_ctr = RegInit(1.U(1.W)) // have to re-issue a req if it gets nack'd (yes this will stall and idgaf)
  val issued_this_req = RegInit(false.B)

  //this is the bits we need to pass in
  dmem_req(0).valid := io.fifo.valid & ~io.dmem.resp.map(_.valid).reduce(_||_) & ~dmem_resp_fired_reg(0) && wait_nack_ctr === 0.U && !issued_this_req
  dontTouch(dmem_req(0).valid)
  dmem_req(0).bits.uop := io.fifo.bits.uop
  dmem_req(0).bits.addr := io.fifo.bits.addr
  dmem_req(0).bits.data := io.fifo.bits.data

  when(io.dmem.req.fire){ //when we issue a req, note that
    issued_this_req := true.B
  }
  when(io.fifo.fire || io.dmem.nack(0).valid){ //when it nacks or we get a new req, reset
    issued_this_req := false.B
  }

  val nmshrs = 8

  val last_req_idx = RegInit(0.U(log2Ceil(nmshrs).W))
  val last_req_l_n_s = RegInit(false.B)

  //address storage for multiple reqs
  val ldq_ptr = Wire(UInt(log2Ceil(nmshrs).W))
  val stq_ptr = Wire(UInt(log2Ceil(nmshrs).W))

  dmem_req(0).bits.uop.ldq_idx := ldq_ptr
  dmem_req(0).bits.uop.stq_idx := stq_ptr

  val ldq_addr_array = RegInit(VecInit(Seq.fill(nmshrs)(0.U(coreMaxAddrBits.W))))
  val ldq_valid_array = RegInit(VecInit(Seq.fill(nmshrs)(false.B)))

  val stq_addr_array = RegInit(VecInit(Seq.fill(nmshrs)(0.U(coreMaxAddrBits.W))))
  val stq_valid_array = RegInit(VecInit(Seq.fill(nmshrs)(false.B)))

  ldq_ptr := ldq_valid_array.zipWithIndex.map{case (data, idx) => 
    (data, idx.U)}.reduce { (a, b) => MuxT((~a._1), a, b)}._2

  stq_ptr := stq_valid_array.zipWithIndex.map{case (data, idx) => 
    (data, idx.U)}.reduce { (a, b) => MuxT((~a._1), a, b)}._2
    
  // Handle Memory Responses and nacks
  //----------------------------------
  val reqs_in_flight = RegInit(0.U(log2Ceil(nmshrs+1).W))

  // io.fifo.ready := (~dmem_resp_fired(0) && dmem_resp_fired_reg(0)) || ~io.fifo.valid || reqs_in_flight =/= (nmshrs-1).U //when we complete a txn, load next txn... up to num MSHRs right? also step though fifo when there is not a valid txn at head :)
  io.fifo.ready := (~io.fifo.valid || reqs_in_flight =/= nmshrs.U) && io.dmem.req.ready && wait_nack_ctr === 0.U && ~io.dmem.nack(0).valid && issued_this_req //when we complete a txn, load next txn... up to num MSHRs right? also step though fifo when there is not a valid txn at head :)
  // io.fifo.ready := io.dmem.req.ready //adjust this to allow multiple inflight reqs at once
  when(io.dmem.req.fire && !(~dmem_resp_fired(0) && dmem_resp_fired_reg(0))) { //when we issue a request without getting one back...
    assert(reqs_in_flight =/= nmshrs.U)
    reqs_in_flight := reqs_in_flight + 1.U
  }.elsewhen((~dmem_resp_fired(0) && dmem_resp_fired_reg(0)) || io.dmem.nack(0).valid) { //if we get a req back without issuing one
    assert(reqs_in_flight =/= 0.U) //assert no overflow plz
    reqs_in_flight := reqs_in_flight - 1.U
  }

  when(io.dmem.nack(0).valid){ //clear pending req on nack
    when(last_req_l_n_s){
      ldq_valid_array(last_req_idx) := false.B      
    }.otherwise{
      stq_valid_array(last_req_idx) := false.B
    }
  }

  when(io.dmem.req.fire){
    wait_nack_ctr := 1.U
    when(dmem_req(0).bits.uop.uses_ldq){
      last_req_idx := ldq_ptr
      last_req_l_n_s := true.B
      ldq_valid_array(ldq_ptr) := true.B
      ldq_addr_array(ldq_ptr) := io.fifo.bits.addr
    }.elsewhen(dmem_req(0).bits.uop.uses_stq){
      last_req_idx := stq_ptr
      last_req_l_n_s := false.B
      stq_valid_array(stq_ptr) := true.B
      stq_addr_array(stq_ptr) := io.fifo.bits.addr
    }
    assert(!(io.fifo.bits.uop.uses_ldq && io.fifo.bits.uop.uses_stq))
  }.elsewhen(wait_nack_ctr > 0.U){
    wait_nack_ctr := wait_nack_ctr - 1.U
  }

  for (w <- 0 until memWidth) {
    // Handle the response... I think this is sufficient.
    when (io.dmem.resp(w).valid)
    {
      io.ack.valid := true.B
      when (io.dmem.resp(w).bits.uop.uses_ldq)
      {
        io.ack.bits.addr := ldq_addr_array(io.dmem.resp(w).bits.uop.ldq_idx) //we need an actual way to get the resp'd address T_T
        ldq_valid_array(io.dmem.resp(w).bits.uop.ldq_idx) := false.B
        assert(ldq_valid_array(io.dmem.resp(w).bits.uop.ldq_idx) === true.B || dmem_resp_fired(w))
        io.ack.bits.load_n_store := true.B
        assert(!io.dmem.resp(w).bits.is_hella)
        dmem_resp_fired(w) := true.B
        // pending_req := false.B
      }
        .elsewhen (io.dmem.resp(w).bits.uop.uses_stq)
      {
        io.ack.bits.addr := stq_addr_array(io.dmem.resp(w).bits.uop.stq_idx) //we need an actual way to get the resp'd address T_T
        stq_valid_array(io.dmem.resp(w).bits.uop.stq_idx) := false.B
        assert(stq_valid_array(io.dmem.resp(w).bits.uop.stq_idx) === true.B || dmem_resp_fired(w))
        io.ack.bits.load_n_store := false.B
        assert(!io.dmem.resp(w).bits.is_hella)
        dmem_resp_fired(w) := true.B
        // pending_req := false.B
      }
      assert(io.dmem.resp(w).bits.uop.uses_stq | io.dmem.resp(w).bits.uop.uses_ldq)
    }
  }
}
