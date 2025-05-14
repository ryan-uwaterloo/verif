package verif.etrace

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.Str

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
  io.dmem.exception      := false.B
  io.dmem.rob_head_idx   := DontCare //only for implementing a fence
  io.dmem.rob_pnr_idx    := DontCare //dontCare?

  val dmem_req = Wire(Vec(memWidth, Valid(new BoomDCacheReq)))
  io.dmem.req.valid := dmem_req.map(_.valid).reduce(_||_) //have req ready in fifo
  io.dmem.req.bits  := dmem_req //fifo

  for (w <- 0 until memWidth) {
    dmem_req(w).valid := false.B
    dmem_req(w).bits.uop   := NullMicroOp
    dmem_req(w).bits.addr  := 0.U
    dmem_req(w).bits.data  := 0.U
    dmem_req(w).bits.is_hella := false.B

    io.dmem.s1_kill(w) := false.B
  }

    //crush this state machine into a fifo

    //this is the bits we need to pass in
    dmem_req(0).valid := io.fifo.valid
    dmem_req(0).bits.uop := io.fifo.bits.uop
    dmem_req(0).bits.addr := io.fifo.bits.addr
    dmem_req(0).bits.data := io.fifo.bits.data
    
  // Handle Memory Responses and nacks
  //----------------------------------
  
  val dmem_resp_fired = WireInit(widthMap(w => false.B))
  io.fifo.ready := dmem_resp_fired(0) || !io.fifo.valid //when we complete a txn, load next txn... right? also step though fifo when there is not a valid txn at head :)
  //I'm kinda theorizing that we'll end up re-issuing the request and all that due to how often things are with the delay and stuff...
  io.dmem.s1_kill := dmem_resp_fired(0) //I think this fixes things but idk

  for (w <- 0 until memWidth) {
    // Handle nacks, or can we just hold it valid..? let's ignore everything because I'm lazy lol
    // when (io.dmem.nack(w).valid)
    // {
    //   // We have to re-execute this!
    //   when (io.dmem.nack(w).bits.is_hella)//what is this nack valid :skull:
    //   {
    //     assert(hella_state === h_wait || hella_state === h_dead)
    //   }
    //     .elsewhen (io.dmem.nack(w).bits.uop.uses_ldq)
    //   {
    //     assert(ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.executed)
    //     ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.executed  := false.B
    //     nacking_loads(io.dmem.nack(w).bits.uop.ldq_idx) := true.B
    //   }
    //     .otherwise
    //   {
    //     assert(io.dmem.nack(w).bits.uop.uses_stq)
    //     when (IsOlder(io.dmem.nack(w).bits.uop.stq_idx, stq_execute_head, stq_head)) {
    //       stq_execute_head := io.dmem.nack(w).bits.uop.stq_idx
    //     }
    //   }
    // }
    // Handle the response... I think this is sufficient.
    when (io.dmem.resp(w).valid)
    {
      when (io.dmem.resp(w).bits.uop.uses_ldq)
      {
        assert(!io.dmem.resp(w).bits.is_hella)
        dmem_resp_fired(w) := true.B
      }
        .elsewhen (io.dmem.resp(w).bits.uop.uses_stq)
      {
        assert(!io.dmem.resp(w).bits.is_hella)
        dmem_resp_fired(w) := true.B
      }
    }
  }
}
