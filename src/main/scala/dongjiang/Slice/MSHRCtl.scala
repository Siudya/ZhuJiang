package DONGJIANG.SLICE

import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.DECODE._
import Utils.Encoder._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._



class MSHREntry()(implicit p: Parameters) extends DJBundle {
  val valid           = Bool()
  val alreadySend     = Bool()
  val tag             = UInt(mshrTagBits.W)
  val bank            = UInt(bankBits.W)
  val waitSnpResp     = Bool()
  val waitRDResp      = Bool()
  val snpWaitNodeId   = Vec(nrRnSlv, Bool())
  val reqMes          = new ReqBaseMesBundle()
  val respMes         = new Bundle {
    val snpRespVal    = Bool()
    val snpResp       = UInt(ChiResp.width.W)
    val fwdState      = Valid(UInt(ChiResp.width.W))

    val rdRespVal     = Bool()
    val rdResp        = UInt(ChiResp.width.W)

    val hasData       = Bool()
    val dbid          = UInt(dbIdBits.W)
  }

  def free    = !valid
  def beSend  = valid & !waitSnpResp & !waitRDResp & !alreadySend
}


class MSHRCtl()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val sliceId       = Input(UInt(bankBits.W))
    // Req and Resp To Slice
    val req2Slice     = Flipped(Decoupled(new Req2SliceBundle()))
    val resp2Slice    = Flipped(Decoupled(new Resp2SliceBundle()))
    // Task To MainPipe
    val mpTask        = Decoupled(new MpTaskBundle())
    // Update Task From MainPipe
    val udpMSHR       = Flipped(Decoupled(new UpdateMSHRBundle()))
    // Directory Read Req
    val dirRead       = Decoupled(new DirReadBundle())
    // Req Retry: Resp To Rn Node
    val retry2RnNode  = Decoupled(new Resp2NodeBundle())
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare



// --------------------- Reg / Wire declaration ------------------------//
  // mshrTable
  val mshrTableReg    = RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { VecInit(Seq.fill(djparam.nrMSHRWays) { 0.U.asTypeOf(new MSHREntry()) }) }))
  val mshrLockVecReg  = RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { false.B }))
  // Transfer Req From Node To MSHREntry
  val mshrAlloc_s0    = WireInit(0.U.asTypeOf(new MSHREntry()))
  // retry mes
  val retryTo_s0_g    = RegInit(0.U.asTypeOf(Valid(new IDBundle())))
  // task s0
  val task_s0         = Wire(Valid(new MpTaskBundle()))
  val canGo_s0        = Wire(Bool())
  // task s1
  val task_s1_g       = RegInit(0.U.asTypeOf(Valid(new MpTaskBundle())))
  val canGo_s1        = Wire(Bool())
  val alreadyReadDirReg = RegInit(false.B)




// ------------------------ S0: Receive Req From Node or Let It Retry-------------------------- //
  /*
   * Get MSHR Mes of Req From Node Set
   */
  val (blockTag, blockSet, blockBank) = parseMSHRAddress(io.req2Slice.bits.addr, djparam.mpBlockBySet); dontTouch(blockTag); dontTouch(blockSet); dontTouch(blockBank)
  val mshrSetMatchVec = mshrTableReg(blockSet).map { case m => if(djparam.mpBlockBySet) m.valid & m.tag === blockTag else m.valid & m.tag === blockTag & m.bank === blockBank }
  val mshrSetInvVec   = mshrTableReg(blockSet).map(!_.valid)
  val mshrInvWay      = PriorityEncoder(mshrSetInvVec)
  val canReceiveReq   = !mshrSetMatchVec.reduce(_ | _) & PopCount(mshrSetInvVec) >= 2.U // A entry must be reserved for MainPipe Req.

  /*
   * Transfer Req From Node To MSHREntry
   */
  val (reqTag, reqSet, reqBank) = parseMSHRAddress(io.req2Slice.bits.addr); dontTouch(reqTag); dontTouch(reqSet); dontTouch(reqBank)
  mshrAlloc_s0          := DontCare
  mshrAlloc_s0.tag      := reqTag
  mshrAlloc_s0.bank     := reqSet
  mshrAlloc_s0.reqMes   := io.req2Slice.bits


  /*
   * Receive Req From Node
   */
  io.req2Slice.ready    := !retryTo_s0_g.valid
  retryTo_s0_g.valid    := Mux(retryTo_s0_g.valid, !io.retry2RnNode.fire, io.req2Slice.fire & !canReceiveReq)
  retryTo_s0_g.bits     := Mux(io.req2Slice.fire, io.req2Slice.bits.from, retryTo_s0_g.bits)


  /*
   * Retry Resp To Rn Node
   * TODO: Completer Retry Logic In Rn ReqBuf
   */
  io.retry2RnNode.valid         := retryTo_s0_g.valid
  io.retry2RnNode.bits          := DontCare
  io.retry2RnNode.bits.reqRetry := true.B
  io.retry2RnNode.bits.to       := retryTo_s0_g.bits

// ----------------------------------- S0: Update MHSR Table ----------------------------------- //
  /*
   * Req Update mshrTable value
   */
  val receiveReqMshr    = mshrTableReg(reqSet)(mshrInvWay)
  receiveReqMshr        := Mux(mshrAlloc_s0.valid & canReceiveReq, mshrAlloc_s0, receiveReqMshr)

  /*
   * Resp Update mshrTable value
   */
  val resp              = io.resp2Slice.bits
  val respMshr          = mshrTableReg(resp.mshrSet)(resp.to.mshrWay)
  when(io.resp2Slice.valid) {
    // Receive Snp Resp
    when(resp.isSnpResp) {
      // TODO
    // Receive Read Down Resp
    }.otherwise {
      respMshr.waitRDResp         := false.B
      respMshr.respMes.rdRespVal  := true.B
      respMshr.respMes.rdResp     := resp.resp
    }
    // Receive DataBuffer ID
    when(resp.hasData) {
      respMshr.respMes.hasData    := true.B
      respMshr.respMes.dbid       := resp.dbid
    }
  }


  /*
   * Receive update mshr mes from MainPipe
   */
  // TODO



// --------------------------------- S0: Get task_s0 from MSHR -------------------------------- //
  /*
   * Get task_s0 mshrSet and mshrWay from MSHR
   */
  val mshrCanSendSetVec   = mshrTableReg.zip(mshrLockVecReg).map { case(t, l) => t.map(_.beSend).reduce(_ | _) & !l }
  val mshrCanSendSet      = RREncoder(mshrCanSendSetVec)
  val mshrCanSendWayVec   = mshrTableReg(mshrCanSendSet).map(_.beSend)
  val mshrCanSendWay      = RREncoder(mshrCanSendWayVec)

  /*
   * send task_s0
   */
  val beSendMshr          = mshrTableReg(mshrCanSendSet)(mshrCanSendWay)
  val canSend_s0          = beSendMshr.beSend & !mshrLockVecReg(mshrCanSendSet)
  task_s0.valid           := canSend_s0
  task_s0.bits.addr       := Cat(beSendMshr.tag, mshrCanSendSet, beSendMshr.bank, 0.U(offsetBits.W))
  // TODO


  /*
   * Set AlreadySend and Lock Value
   */
  beSendMshr.alreadySend          := Mux(canSend_s0, canGo_s0, false.B) // TODO
  mshrLockVecReg(mshrCanSendSet)  := Mux(canSend_s0, canGo_s0, mshrLockVecReg(mshrCanSendSet)) // TODO
  canGo_s0                        := canGo_s1 | !task_s1_g.valid


// ------------------------ S1: Read Dir and send task to MainPipe --------------------------//
  /*
   * Set Task S1 Value
   */
  task_s1_g.valid := Mux(task_s0.valid, true.B, task_s1_g.valid & !canGo_s1)
  task_s1_g.bits  := Mux(task_s0.valid & canGo_s0, task_s0.bits, task_s1_g.bits)
  canGo_s1        := io.mpTask.ready & (io.dirRead.ready | alreadyReadDirReg)

  /*
   * Send mpTask to mainpipe
   */
  io.mpTask.valid := task_s1_g.valid & canGo_s1
  io.mpTask.bits  := task_s1_g.bits

  /*
   * Read Directory
   */
  alreadyReadDirReg     := Mux(alreadyReadDirReg, !io.mpTask.fire, io.dirRead.fire & !canGo_s1)
  io.dirRead.valid      := task_s1_g.valid & !alreadyReadDirReg
  io.dirRead.bits.addr  := task_s1_g.bits.addr


// ------------------------------------- Assertion ---------------------------------------//
  // TODO





}