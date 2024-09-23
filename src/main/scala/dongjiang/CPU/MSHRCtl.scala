package DONGJIANG.CPU

import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.DECODE._
import Utils.Encoder._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._


object MSHRState {
  // [free] ---> [beSend] ---> [alreadySend] ---> [waitResp] ---> [beSend] ---> [free]
  // [free] ---> [beSend] ---> [alreadySend] ---> [free]
  val width       = 2
  val Free        = "b00".U
  val BeSend      = "b01".U
  val AlreadySend = "b10".U
  val WaitResp    = "b11".U
}


class MSHREntry(useAddr: Boolean = false)(implicit p: Parameters) extends DJBundle {
  // state
  val state           = UInt(MSHRState.width.W)
  // addr
  val tag             = UInt(mshrTagBits.W)
  val bank            = UInt(bankBits.W)
  val setOpt          = if(useAddr) Some(UInt(mshrSetBits.W)) else None
  def set             = setOpt.get
  // req mes
  val chiMes          = new MSHRCHIMesBundle()
  // resp mes
  val waitIntfVec     = Vec(nrIntf, Bool()) // Wait Snoop Resp or Req Resp
  val respMes         = new RespMesBundle()

  def isValid         = state =/= MSHRState.Free
  def isFree          = state === MSHRState.Free
  def isBeSend        = state === MSHRState.BeSend
  def isAlreadySend   = state === MSHRState.AlreadySend
  def isWaitResp      = state === MSHRState.WaitResp
  def isResp          = respMes.slvResp.valid | respMes.masResp.valid | respMes.fwdState.valid
  def isReq           = !isResp
  def respBeSend      = isBeSend & isResp
  def reqBeSend       = isBeSend & isReq

  def addr(x: UInt = setOpt.getOrElse(0.U)): UInt = Cat(tag, x, bank, 0.U(offsetBits.W))
}


class MSHRCtl()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val sliceId       = Input(UInt(bankBits.W))
    // Req To Slice
    val req2Slice     = Flipped(Decoupled(new Req2SliceBundle()))
    // Ack To Node
    val reqAck2node   = Decoupled(new ReqAck2NodeBundle())
    // Resp To Slice
    val resp2Slice    = Flipped(Decoupled(new Resp2SliceBundle()))
    // Task To MainPipe
    val pipeTask      = Vec(2, Decoupled(new PipeTaskBundle()))
    // Update Task From MainPipe
    val udpMSHR       = Flipped(Decoupled(new UpdateMSHRReqBundle()))
    val updLockMSHR   = Flipped(Decoupled(new MSHRSetBundle))
    // Directory Read Req
    val earlyRReqVec  = Vec(djparam.nrDirBank, Decoupled())
    val dirRead       = Vec(2, Valid(new DirReadBundle()))
    // Directory Read MSHR Set Mes
    val dirReadMshr   = Vec(2, Flipped(Valid(new DirReadMSHRBundle())))
    val mshrResp2Dir  = Vec(2, Valid(new MSHRRespDirBundle()))
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare
  dontTouch(io)

// ------------------------ Module declaration ------------------------- //
  val reqAck_s0_q       = Module(new Queue(new ReqAck2NodeBundle(), entries = 4, flow = false, pipe = true))

// --------------------- Reg / Wire declaration ------------------------ //
  // mshrTable
  val mshrTableReg      = RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { VecInit(Seq.fill(djparam.nrMSHRWays) { 0.U.asTypeOf(new MSHREntry()) }) }))
  val evictTableReg     = RegInit(VecInit(Seq.fill(djparam.nrEvictWays) { 0.U.asTypeOf(new MSHREntry(useAddr = true)) })) // Use to save snp evict // TODO: Setting the right number of entries
  val mshrLockVecReg    = RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { false.B }))
  // Transfer Req From Node To MSHREntry
  val mshrAlloc_s0      = WireInit(0.U.asTypeOf(new MSHREntry()))
  // task s0
  val canSendVec        = Wire(Vec(djparam.nrMSHRSets, Bool()))
  val taskReq_s0        = Wire(Valid(new PipeTaskBundle()))
  val canGoReq_s0       = Wire(Bool())
  val taskResp_s0       = Wire(Valid(new PipeTaskBundle()))
  val canGoResp_s0      = Wire(Bool())
  // task s1
  val taskReq_s1_g      = RegInit(0.U.asTypeOf(Valid(new PipeTaskBundle())))
  val canGoReq_s1       = Wire(Bool())
  val taskResp_s1_g     = RegInit(0.U.asTypeOf(Valid(new PipeTaskBundle())))
  val canGoResp_s1      = Wire(Bool())
  val reqAlreadyReadDirReg  = RegInit(false.B)
  val respAlreadyReadDirReg = RegInit(false.B)
  // dir read mshr
  val resp2dirRegVec    = RegInit(0.U.asTypeOf(io.mshrResp2Dir))


// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------- S0: Receive Req From Node or Let It Retry ------------------------------------ //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Get MSHR Mes
   */
  // mshrTableReg
  val nodeReqMatchVec = mshrTableReg(io.req2Slice.bits.mSet).map { case m => m.isValid & m.tag === io.req2Slice.bits.mTag & m.bank === io.req2Slice.bits.mBank }
  val nodeReqInvVec   = mshrTableReg(io.req2Slice.bits.mSet).map(_.isFree)
  val nodeReqInvWay   = PriorityEncoder(nodeReqInvVec)
  // evictTableReg
  val evictMatchVec   = evictTableReg.map { case m => m.isValid & m.addr() === io.req2Slice.bits.addr }
  // mshrLockVecReg
  val lockMatch       = mshrLockVecReg(io.req2Slice.bits.mSet)


  /*
   * Get Block Message
   */
  val blockByMHSR     = nodeReqMatchVec.reduce(_ | _) | PopCount(nodeReqInvVec) <= 1.U | !evictMatchVec.reduce(_ | _) | lockMatch
  val canReceiveNode  = blockByMHSR


  /*
   * Transfer Req From Node To MSHREntry
   */
  mshrAlloc_s0.tag      := io.req2Slice.bits.mTag
  mshrAlloc_s0.bank     := io.req2Slice.bits.mBank
  mshrAlloc_s0.chiMes   := io.req2Slice.bits


  /*
   * Receive Req From Node and Determine if it needs retry
   * TODO: Setting blocking timer
   */
  io.req2Slice.ready            := reqAck_s0_q.io.enq.ready
  reqAck_s0_q.io.enq.valid      := io.req2Slice.valid
  reqAck_s0_q.io.enq.bits.retry := !canReceiveNode
  reqAck_s0_q.io.enq.bits.to    := io.req2Slice.bits.from
  reqAck_s0_q.io.enq.bits.pcuId := io.req2Slice.bits.pcuId
  io.reqAck2node                <> reqAck_s0_q.io.deq


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------- S0: Update MSHR Table Value  ------------------------------------------ //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Update MSHRTable
   */
  mshrTableReg.zipWithIndex.foreach {
    case(m, i) =>
      m.zipWithIndex.foreach {
        case(m, j) =>
          /*
           * Pipe Update mshrTable value
           */
          when(io.udpMSHR.valid & !io.udpMSHR.bits.isRetry & io.udpMSHR.bits.useMSHR & io.udpMSHR.bits.mSet === i.U & io.udpMSHR.bits.mshrWay === j.U) {
            m.waitIntfVec     := io.udpMSHR.bits.waitIntfVec
            when(io.udpMSHR.bits.isReq) {
              m.chiMes.opcode := DontCare; assert(false.B, "TODO")
              m.tag           := io.udpMSHR.bits.mTag
              m.bank          := io.udpMSHR.bits.mBank
            }
            assert(PopCount(m.waitIntfVec) === 0.U, s"MSHR[0x%x][0x%x] ADDR[0x%x] CHANNEL[0x%x] OP[0x%x]", i.U, j.U, m.addr(i.U), m.chiMes.channel, m.chiMes.opcode)
            assert(m.isAlreadySend, s"MSHR[0x%x][0x%x] ADDR[0x%x] CHANNEL[0x%x] OP[0x%x]", i.U, j.U, m.addr(i.U), m.chiMes.channel, m.chiMes.opcode)
          /*
           * Resp Update mshrTable value
           */
          }.elsewhen(io.resp2Slice.valid & io.resp2Slice.bits.useMSHR & io.resp2Slice.bits.mshrMatch(i.U, j.U)) {
            assert(!io.resp2Slice.bits.isUpdate, "TODO")
            // Recovery of pending intf identifiers
            m.waitIntfVec(io.resp2Slice.bits.from.intfId) := false.B
            // Record Resp Mes
            when(io.resp2Slice.bits.isSnpResp) {
              m.respMes.slvResp.valid   := true.B
              m.respMes.slvResp.bits    := io.resp2Slice.bits.resp
              m.respMes.slvDBID.valid   := io.resp2Slice.bits.hasData
              m.respMes.slvDBID.bits    := io.resp2Slice.bits.dbid
              m.respMes.fwdState.valid  := io.resp2Slice.bits.fwdState.valid | m.respMes.fwdState.valid
              m.respMes.fwdState.bits   := Mux(io.resp2Slice.bits.fwdState.valid, io.resp2Slice.bits.fwdState.bits, m.respMes.fwdState.bits)
            }.elsewhen(io.resp2Slice.bits.isReqResp) {
              m.respMes.masResp.valid   := true.B
              m.respMes.masResp.bits    := io.resp2Slice.bits.resp
              m.respMes.masDBID.valid   := io.resp2Slice.bits.hasData
              m.respMes.masDBID.bits    := io.resp2Slice.bits.dbid
            }
            assert(io.resp2Slice.bits.isSnpResp ^ io.resp2Slice.bits.isReqResp)
            assert(m.waitIntfVec(io.resp2Slice.bits.from.intfId), s"MSHR[0x%x][0x%x] ADDR[0x%x] CHANNEL[0x%x] OP[0x%x]", i.U, j.U, m.addr(i.U), m.chiMes.channel, m.chiMes.opcode)
            assert(m.isWaitResp, s"MSHR[0x%x][0x%x] ADDR[0x%x] CHANNEL[0x%x] OP[0x%x]", i.U, j.U, m.addr(i.U), m.chiMes.channel, m.chiMes.opcode)
          /*
           * Receive Node Req
           */
          }.elsewhen(io.req2Slice.fire & canReceiveNode & i.U === io.req2Slice.bits.mSet & j.U === nodeReqInvWay) {
            m := mshrAlloc_s0
            assert(m.isFree, s"MSHR[0x%x][0x%x] ADDR[0x%x] CHANNEL[0x%x] OP[0x%x]", i.U, j.U, m.addr(i.U), m.chiMes.channel, m.chiMes.opcode)
          /*
           * Clean MSHR Entry When Its Free
           */
          }.elsewhen(m.isFree) {
            m := 0.U.asTypeOf(m)
          }
      }
  }

  /*
   * Update EVICTTable
   */
  evictTableReg.zipWithIndex.foreach {
    case(e, i) =>
      /*
       * Pipe Update evictTable value
       */
      when(io.udpMSHR.valid & !io.udpMSHR.bits.isRetry & io.udpMSHR.bits.useEvict & io.udpMSHR.bits.mSet === i.U & io.udpMSHR.bits.mshrWay === i.U) {
        // TODO
        assert(false.B)
      /*
       * Resp Update evictTable value
       */
      }.elsewhen(io.resp2Slice.valid & io.resp2Slice.bits.useEvict & io.resp2Slice.bits.mshrWay === i.U) {
        // TODO
        assert(false.B)
      }
  }


  /*
   * Set ready value
   */
  io.udpMSHR.ready := true.B
  io.resp2Slice.ready := true.B


  /*
   * Update Lock Vec
   */
  mshrLockVecReg.zipWithIndex.foreach {
    case(lock, i) =>
      when(io.updLockMSHR.valid & io.updLockMSHR.bits.mshrSet === i.U) {
        lock := false.B
      }.elsewhen((taskReq_s0.valid & canGoReq_s0 & taskReq_s0.bits.readDir & taskReq_s0.bits.mSet === i.U) | // Req Fire
                 (taskResp_s0.valid & canGoResp_s0 & taskResp_s0.bits.readDir & taskResp_s0.bits.mSet === i.U)) { // Resp Fire
        lock := true.B
      }
  }
  io.updLockMSHR.ready := true.B



// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------- S0: Update MHSR Table State -------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Update MSHR Table State
   */
  mshrTableReg.zipWithIndex.foreach {
    case (m, i) =>
      m.zipWithIndex.foreach {
        case (m, j) =>
          switch(m.state) {
            // Free
            is(MSHRState.Free) {
              val nodeHit = io.req2Slice.valid & canReceiveNode & i.U === io.req2Slice.bits.mSet & j.U === nodeReqInvWay
              m.state     := Mux(nodeHit, MSHRState.BeSend, MSHRState.Free)
            }
            // BeSend
            is(MSHRState.BeSend) {
              val reqhit  = taskReq_s0.valid & canGoReq_s0 & taskReq_s0.bits.mSet === i.U & taskReq_s0.bits.mshrWay === j.U & taskReq_s0.bits.useMSHR
              val resphit = taskResp_s0.valid & canGoResp_s0 & taskResp_s0.bits.mSet === i.U & taskResp_s0.bits.mshrWay === j.U & taskResp_s0.bits.useMSHR
              m.state     := Mux(reqhit | resphit, MSHRState.AlreadySend, MSHRState.BeSend)
            }
            // AlreadySend
            is(MSHRState.AlreadySend) {
              val hit     = io.udpMSHR.valid & io.udpMSHR.bits.mSet === i.U & io.udpMSHR.bits.mshrWay === j.U
              val retry   = hit & io.udpMSHR.bits.isRetry
              val update  = hit & io.udpMSHR.bits.isUpdate
              val clean   = hit & io.udpMSHR.bits.isClean
              val req     = hit & io.udpMSHR.bits.isReq
              m.state     := Mux(retry, MSHRState.BeSend,
                                Mux(update, MSHRState.WaitResp,
                                  Mux(req, MSHRState.WaitResp,
                                    Mux(clean, MSHRState.Free, MSHRState.AlreadySend))))
            }
            // WaitResp
            is(MSHRState.WaitResp) {
              val hit     = !m.waitIntfVec.reduce(_ | _)
              m.state     := Mux(hit, MSHRState.BeSend, MSHRState.WaitResp)
            }
          }
      }
  }


  /*
   * Update EVICT Table State
   */
  evictTableReg.zipWithIndex.foreach {
    case(e, i) =>
      // TODO
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------- S0: Get task_s0 from MSHR ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Get Can Send Set From Dir Read Ready and mshrLockVecReg
   */
  require(djparam.nrMSHRSets >= djparam.nrDirBank)
  canSendVec.zipWithIndex.foreach { case(can, i) => can := io.earlyRReqVec(i.U(dirBankBits-1, 0)).ready & !mshrLockVecReg(i) } // TODO: if dont need to read Dir, it should not be ctrl by mshrLockVecReg

  /*
   * Get task_s0(req) from MSHRTable
   */
  val reqBeSendSetVec     = mshrTableReg.map(_.map(_.reqBeSend).reduce(_ | _))
  val reqBeSendSet        = RREncoder(reqBeSendSetVec.zip(canSendVec).map{ case(a, b) => a & b })
  val reqBeSendWay        = RREncoder(mshrTableReg(reqBeSendSet).map(_.reqBeSend))
  val taskReq             = mshrTableReg(reqBeSendSet)(reqBeSendWay)

  /*
   * Get task_s0(resp) from MSHRTable
   */
  val respBeSendSetVec    = mshrTableReg.map(_.map(_.respBeSend).reduce(_ | _))
  val respBeSendSet       = RREncoder(respBeSendSetVec.zip(canSendVec).map{ case(a, b) => a & b })
  val respBeSendWay       = RREncoder(mshrTableReg(respBeSendSet).map(_.respBeSend))
  val taskResp            = mshrTableReg(respBeSendSet)(respBeSendWay)

  /*
   * Get task_s0(evict) from MSHRTable
   */
  val evictBeSendId       = RREncoder(evictTableReg.map {case e => e.isBeSend & canSendVec(e.set) })
  val taskEvict           = evictTableReg(evictBeSendId)


  /*
   * Select resp task_s0
   */
  taskResp_s0.valid         := taskResp.respBeSend | taskEvict.respBeSend
  taskResp_s0.bits.readDir  := true.B // TODO
  //                                                 EvictTask            RespTask
  taskResp_s0.bits.addr     := Mux(taskEvict.isBeSend, taskEvict.addr(),  taskResp.addr(respBeSendSet))
  taskResp_s0.bits.pipeId   := Mux(taskEvict.isBeSend, PipeID.RESP,       PipeID.RESP)
  taskResp_s0.bits.mshrWay  := Mux(taskEvict.isBeSend, evictBeSendId,     respBeSendWay)
  taskResp_s0.bits.reqMes   := Mux(taskEvict.isBeSend, taskEvict.chiMes,  taskResp.chiMes)
  taskResp_s0.bits.respMes  := Mux(taskEvict.isBeSend, taskEvict.respMes, taskResp.respMes)
  taskResp_s0.bits.useEvict := taskEvict.isBeSend
  canGoResp_s0              := canGoResp_s1 | !taskResp_s1_g.valid


  /*
   * Select req task_s0
   */
  taskReq_s0.valid          := taskReq.reqBeSend & !(taskResp_s0.valid & taskResp_s0.bits.dirBank === taskReq_s0.bits.dirBank)
  taskReq_s0.bits.readDir   := true.B // TODO
  //                           ReqTask
  taskReq_s0.bits.addr      := taskReq.addr(reqBeSendSet)
  taskReq_s0.bits.pipeId    := PipeID.REQ
  taskReq_s0.bits.mshrWay   := reqBeSendWay
  taskReq_s0.bits.reqMes    := taskReq.chiMes
  taskReq_s0.bits.respMes   := taskReq.respMes
  taskReq_s0.bits.useEvict  := false.B
  canGoReq_s0               := canGoReq_s1 | !taskReq_s1_g.valid

  /*
   * Read Directory Early Req
   */
  io.earlyRReqVec.map(_.valid).zipWithIndex.foreach {
    case(v, i) =>
      v := taskResp_s0.valid & taskResp_s0.bits.dirBank === i.U | taskReq_s0.valid & taskReq_s0.bits.dirBank === i.U
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------- S0: Read Dir and send task to MainPipe -------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Set Task S1 Value
   */
  // resp
  taskResp_s1_g.valid       := Mux(taskResp_s0.valid, true.B, taskResp_s1_g.valid & !canGoResp_s1)
  taskResp_s1_g.bits        := Mux(taskResp_s0.valid & canGoResp_s0, taskResp_s0.bits, taskResp_s1_g.bits)
  canGoResp_s1              := io.pipeTask(PipeID.RESP).ready
  //req
  taskReq_s1_g.valid        := Mux(taskReq_s0.valid, true.B, taskReq_s1_g.valid & !canGoReq_s1)
  taskReq_s1_g.bits         := Mux(taskReq_s0.valid & canGoReq_s0, taskReq_s0.bits, taskReq_s1_g.bits)
  canGoReq_s1               := io.pipeTask(PipeID.REQ).ready

  /*
   * Send Task to Pipe
   */
  // resp
  respAlreadyReadDirReg           := Mux(respAlreadyReadDirReg, taskResp_s0.valid & canGoResp_s0, io.pipeTask(0).fire & !canGoResp_s1)
  io.pipeTask(PipeID.RESP).valid  := taskResp_s1_g.valid
  io.pipeTask(PipeID.RESP).bits   := taskResp_s1_g.bits
  // req
  reqAlreadyReadDirReg            := Mux(reqAlreadyReadDirReg, taskReq_s0.valid & canGoReq_s0, io.pipeTask(1).fire & !canGoReq_s1)
  io.pipeTask(PipeID.REQ).valid   := taskReq_s1_g.valid
  io.pipeTask(PipeID.REQ).bits    := taskReq_s1_g.bits

  /*
   * Read Directory
   */
  // resp
  io.dirRead(PipeID.RESP).valid       := taskResp_s1_g.valid & !respAlreadyReadDirReg
  io.dirRead(PipeID.RESP).bits.addr   := taskResp_s1_g.bits.addr
  io.dirRead(PipeID.RESP).bits.pipeId := taskResp_s1_g.bits.pipeId
  // req
  io.dirRead(PipeID.REQ).valid        := taskReq_s1_g.valid & !reqAlreadyReadDirReg
  io.dirRead(PipeID.REQ).bits.addr    := taskReq_s1_g.bits.addr
  io.dirRead(PipeID.REQ).bits.pipeId  := taskReq_s1_g.bits.pipeId



// ------------------------ S2: Dir Read MSHR and MSHR Resp to Dir --------------------------//
  io.dirReadMshr.zip(resp2dirRegVec).foreach {
    case (read, resp) =>
      when(read.valid) {
        resp.valid          := true.B
        resp.bits.pipeId    := read.bits.pipeId
        resp.bits.dirBankId := read.bits.dirBankId
        resp.bits.addrs.zipWithIndex.foreach {
          case (r, i) =>
            if (i < djparam.nrMSHRWays) {
              r.valid := mshrTableReg(read.bits.mshrSet)(i).isValid
              r.bits  := mshrTableReg(read.bits.mshrSet)(i).addr(read.bits.mshrSet)
            } else {
              val id = i - djparam.nrMSHRWays
              r.valid := evictTableReg(id).isValid & evictTableReg(id).set === read.bits.mshrSet
              r.bits  := evictTableReg(id).addr()
            }
        }
      }.otherwise {
        resp.valid    := false.B
        resp.bits     := 0.U.asTypeOf(resp.bits)
      }
  }
  io.mshrResp2Dir     := resp2dirRegVec


// ---------------------------  Assertion  -------------------------------- //
  // MSHR Timeout Check
  val cntMSHRReg = RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { VecInit(Seq.fill(djparam.nrMSHRWays) { 0.U(64.W) }) }))
  cntMSHRReg.zipWithIndex.foreach { case (c0, i) => c0.zipWithIndex.foreach { case(c1, j) => c1 := Mux(mshrTableReg(i)(j).isFree, 0.U, c1 + 1.U)  } }
  cntMSHRReg.zipWithIndex.foreach { case (c0, i) => c0.zipWithIndex.foreach { case(c1, j) => assert(c1 < TIMEOUT_MSHR.U, "MSHR[0x%x][0x%x] ADDR[0x%x] CHANNEL[0x%x] OP[0x%x] TIMEOUT", i.U, j.U, mshrTableReg(i)(j).addr(i.U), mshrTableReg(i)(j).chiMes.channel, mshrTableReg(i)(j).chiMes.opcode) } }

  // MSHR Timeout Check
  val cntEvictReg = RegInit(VecInit(Seq.fill(djparam.nrEvictWays) { 0.U(64.W) }))
  cntEvictReg.zipWithIndex.foreach { case (c, i) => c := Mux(evictTableReg(i).isFree, 0.U, c + 1.U) }
  cntEvictReg.zipWithIndex.foreach { case (c, i) => assert(c < TIMEOUT_MSHR.U, "EVICT[0x%x] ADDR[0x%x] CHANNEL[0x%x] OP[0x%x] TIMEOUT", i.U, evictTableReg(i).addr(i.U), evictTableReg(i).chiMes.channel, evictTableReg(i).chiMes.opcode) }


}