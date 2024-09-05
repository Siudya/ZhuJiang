package DONGJIANG.SLICE

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
  val reqMes          = new ReqBaseMesBundle()
  // resp mes
  val waitSlvVec      = Vec(nrSlvIntf, Bool()) // Wait Snoop Resp
  val waitMasVec      = Vec(nrMasIntf, Bool()) // Wait Req Resp
  val respMes         = new Bundle {
    val slvResp       = Valid(UInt(ChiResp.width.W))
    val masResp       = Valid(UInt(ChiResp.width.W))
    val fwdState      = Valid(UInt(ChiResp.width.W))
    val slvDBID       = Valid(UInt(dbIdBits.W))
    val masDBID       = Valid(UInt(dbIdBits.W))
  }

  def isValid       = state =/= MSHRState.Free
  def isFree        = state === MSHRState.Free
  def isBeSend      = state === MSHRState.BeSend
  def isAlreadySend = state === MSHRState.AlreadySend
  def isWaitResp    = state === MSHRState.WaitResp
  def isResp        = respMes.slvResp.valid | respMes.masResp.valid | respMes.fwdState.valid
  def isReq         = !isResp
  def respBeSend    = isBeSend & isResp
  def reqBeSend     = isBeSend & isReq

  def addr(x: UInt = setOpt.getOrElse(0.U)): UInt = Cat(tag, x, bank, 0.U(offsetBits.W))
}


class MSHRCtl()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val sliceId       = Input(UInt(bankBits.W))
    // Req and Resp To Slice
    val req2Slice     = Flipped(Decoupled(new Req2SliceBundle()))
    val resp2Slice    = Flipped(Decoupled(new Resp2SliceBundle()))
    // Task To MainPipe
    val pipeTask      = Decoupled(new PipeTaskBundle())
    // Update Task From MainPipe
    val udpMSHR       = Flipped(Decoupled(new UpdateMSHRReqBundle()))
    val udpResp       = Valid(new UpdateMSHRRespBundle())
    val updLockMSHR   = Flipped(Decoupled(new MSHRSetBundle))
    // Directory Read Req
    val dirRead       = Decoupled(new DirReadBundle())
    // Directory Read MSHR Set Mes
    val dirReadMshr   = Flipped(Valid(UInt(mshrSetBits.W)))
    val mshrResp2Dir  = Output(Vec(djparam.nrMSHRWays + djparam.nrEvictWays, Valid(UInt(addressBits.W))))
    // Req Retry: Resp To Rn Node
    val retry2Node    = Decoupled(new Resp2NodeBundle())
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare
  dontTouch(io)



// --------------------- Reg / Wire declaration ------------------------//
  // mshrTable
  val mshrTableReg    = RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { VecInit(Seq.fill(djparam.nrMSHRWays) { 0.U.asTypeOf(new MSHREntry()) }) }))
  val evictTableReg   = RegInit(VecInit(Seq.fill(djparam.nrEvictWays) { 0.U.asTypeOf(new MSHREntry(useAddr = true)) })) // Use to save snp evict / evict req from resp process // TODO: Setting the right number of entries
  val mshrLockVecReg  = RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { false.B }))
  // Transfer Req From Node To MSHREntry
  val mshrAlloc_s0    = WireInit(0.U.asTypeOf(new MSHREntry()))
  // retry mes
  val retryTo_s0_g    = RegInit(0.U.asTypeOf(Valid(new IDBundle())))
  val udpResp_s0_g    = RegInit(0.U.asTypeOf(Valid(new UpdateMSHRRespBundle())))
  // will receive pipe req number
  val mshrWillUseVecReg =  RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { 0.U(mshrWayBits.W) }))
  val evictWillUseReg   =  RegInit(0.U(mshrWayBits.W))
  // task s0
  val task_s0         = Wire(Valid(new PipeTaskBundle()))
  val canGo_s0        = Wire(Bool())
  // task s1
  val task_s1_g       = RegInit(0.U.asTypeOf(Valid(new PipeTaskBundle())))
  val canGo_s1        = Wire(Bool())
  val alreadyReadDirReg   = RegInit(false.B)
  val alreadySendTaskReg  = RegInit(false.B)




// ------------------------ S0: Receive Req From Node or Let It Retry-------------------------- //
  /*
   * Get MSHR Mes
   */
  // mshrTableReg
  val nodeReqMatchVec = mshrTableReg(io.req2Slice.bits.mSet).map { case m => m.isValid & m.tag === io.req2Slice.bits.mTag & m.bank === io.req2Slice.bits.mBank }
  val nodeReqInvVec   = mshrTableReg(io.req2Slice.bits.mSet).map(_.isFree)
  val nodeReqInvWay   = PriorityEncoder(nodeReqInvVec)
  // evictTableReg
  val evictMatchVec   = evictTableReg.map { case m => m.isValid & m.addr() === io.req2Slice.bits.addr }


  /*
   * Get Block Message
   */
  val blockByMHSR     = nodeReqMatchVec.reduce(_ | _) | PopCount(nodeReqInvVec) <= 1.U | !evictMatchVec.reduce(_ | _)
  val blockByPipeReq  = io.udpMSHR.valid & io.udpMSHR.bits.isReq
  val canReceiveNode  = blockByMHSR | blockByPipeReq


  /*
   * Transfer Req From Node To MSHREntry
   */
  mshrAlloc_s0.tag      := io.req2Slice.bits.mTag
  mshrAlloc_s0.bank     := io.req2Slice.bits.mBank
  mshrAlloc_s0.reqMes   := io.req2Slice.bits


  /*
   * Receive Req From Node and Determine if it needs retry
   * TODO: Setting blocking timer
   */
  io.req2Slice.ready    := !retryTo_s0_g.valid
  retryTo_s0_g.valid    := Mux(retryTo_s0_g.valid, !io.retry2Node.fire, io.req2Slice.fire & !canReceiveNode)
  retryTo_s0_g.bits     := Mux(io.req2Slice.fire, io.req2Slice.bits.from, retryTo_s0_g.bits)


  /*
   * Retry Resp To Rn Node
   * TODO: Complete Retry Logic In ReqBuf
   */
  assert(!io.retry2Node.valid)
  io.retry2Node.valid         := retryTo_s0_g.valid
  io.retry2Node.bits          := DontCare
  io.retry2Node.bits.reqRetry := true.B
  io.retry2Node.bits.to       := retryTo_s0_g.bits


// ------------------------ S0: Receive Req From Pipe or Let It Retry-------------------------- //
  // mshrTableReg
  val pipeReqInvVec           = mshrTableReg(io.udpMSHR.bits.mSet).map(_.isFree)
  val pipeReqInvWay           = PriorityEncoder(pipeReqInvVec)
  // evictTableReg
  val evictInvVec             = evictTableReg.map(_.isFree)
  val evictInvId              = PriorityEncoder(evictInvVec)

  // judgment logic
  val canSavePipeReq          = PopCount(pipeReqInvVec) + PopCount(evictInvVec) - mshrWillUseVecReg(io.udpMSHR.bits.mSet) - evictWillUseReg > io.udpMSHR.bits.willUseWay
  val mshrCanReceivePipe      = PopCount(pipeReqInvVec) > 0.U
  val evictCanReceivePipe     = PopCount(evictInvVec) > 0.U
  val pipeReqSaveInEvict      = !mshrCanReceivePipe & evictCanReceivePipe

  // count will use way
  when(io.udpMSHR.fire & io.udpMSHR.bits.isUpdate) {
    when(mshrWillUseVecReg(io.udpMSHR.bits.mSet) <= io.udpMSHR.bits.willUseWay) {
      mshrWillUseVecReg(io.udpMSHR.bits.mSet) := mshrWillUseVecReg(io.udpMSHR.bits.mSet) + io.udpMSHR.bits.willUseWay
    }.otherwise {
      mshrWillUseVecReg(io.udpMSHR.bits.mSet) := djparam.nrMSHRWays.U
      evictWillUseReg                         := evictWillUseReg + io.udpMSHR.bits.willUseWay - PopCount(pipeReqInvVec)
    }
  }.elsewhen(io.udpMSHR.fire & io.udpMSHR.bits.isReq) {
    mshrWillUseVecReg(io.udpMSHR.bits.mSet)   := mshrWillUseVecReg(io.udpMSHR.bits.mSet) - mshrCanReceivePipe.asUInt
    evictWillUseReg                           := evictWillUseReg - (!mshrCanReceivePipe & evictCanReceivePipe).asUInt
  }
  // resp to pipe
  udpResp_s0_g.valid          := io.udpMSHR.fire
  udpResp_s0_g.bits.retry     := Mux(io.udpMSHR.bits.isUpdate, canSavePipeReq, false.B)
  udpResp_s0_g.bits.updType   := io.udpMSHR.bits.updType
  udpResp_s0_g.bits.pipeId    := io.udpMSHR.bits.pipeId
  udpResp_s0_g.bits.mshrWay   := Mux(pipeReqSaveInEvict, evictInvId, pipeReqInvWay)
  udpResp_s0_g.bits.useEvict  := pipeReqSaveInEvict

  io.udpMSHR.ready            := true.B
  io.udpResp                  := udpResp_s0_g

// ----------------------------------- S0: Update MHSR Table Value ----------------------------------- //
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
          when(io.udpMSHR.valid & io.udpMSHR.bits.isUpdate & io.udpMSHR.bits.useMSHR & io.udpMSHR.bits.mSet === i.U & io.udpMSHR.bits.mshrWay === j.U) {
            m.waitSlvVec := io.udpMSHR.bits.waitSlvVec
            m.waitMasVec := io.udpMSHR.bits.waitMasVec
            assert(PopCount(m.waitSlvVec) === 0.U)
            assert(PopCount(m.waitMasVec) === 0.U)
          /*
           * Receive Pipe Req
           */
          }.elsewhen(io.udpMSHR.valid & io.udpMSHR.bits.isReq & mshrCanReceivePipe & io.udpMSHR.bits.mSet === i.U & pipeReqInvWay === j.U) {
            // TODO
            assert(false.B)
          /*
           * Resp Update mshrTable value
           */
          }.elsewhen(io.resp2Slice.valid & io.resp2Slice.bits.useMSHR & io.resp2Slice.bits.mshrMatch(i.U, j.U)) {
            // TODO
            assert(false.B)
          /*
           * Receive Node Req
           */
          }.elsewhen(io.req2Slice.fire & canReceiveNode & i.U === io.req2Slice.bits.mSet & j.U === nodeReqInvWay) {
            m := mshrAlloc_s0
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
      when(io.udpMSHR.valid & io.udpMSHR.bits.isUpdate & io.udpMSHR.bits.useEvict & io.udpMSHR.bits.mSet === i.U & io.udpMSHR.bits.mshrWay === i.U) {
        // TODO
        assert(false.B)
        /*
       * Receive Pipe Req
       */
      }.elsewhen(io.udpMSHR.valid & io.udpMSHR.bits.isReq & pipeReqSaveInEvict & evictInvId === i.U) {
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
      }.elsewhen(task_s0.valid & canGo_s0 & task_s0.bits.mSet === i.U) {
        lock := true.B
      }
  }
  io.updLockMSHR.ready := true.B



// ----------------------------------- S0: Update MHSR Table State ----------------------------------- //
  /*
   * Update MSHR Table State
   */
  mshrTableReg.zipWithIndex.foreach {
    case (m, i) =>
      m.zipWithIndex.foreach {
        case (m, j) =>
          switch(m.state) {
            is(MSHRState.Free) {
              val nodeHit = io.req2Slice.valid & canReceiveNode & i.U === io.req2Slice.bits.mSet & j.U === nodeReqInvWay
              val pipeHit = io.udpMSHR.valid & io.udpMSHR.bits.isReq & mshrCanReceivePipe & io.udpMSHR.bits.mSet === i.U & pipeReqInvWay === j.U
              m.state := Mux(nodeHit | pipeHit, MSHRState.BeSend, MSHRState.Free)
            }
            is(MSHRState.BeSend) {
              val hit = task_s0.valid & canGo_s0 & task_s0.bits.mSet === i.U & task_s0.bits.mshrWay === j.U & task_s0.bits.useMSHR
              m.state := Mux(hit, MSHRState.AlreadySend, MSHRState.BeSend)
            }
            is(MSHRState.AlreadySend) {
              val hit = io.udpMSHR.valid & io.resp2Slice.bits.useMSHR & io.resp2Slice.bits.mshrMatch(i.U, j.U)
              val isClean = !(io.udpMSHR.bits.waitSlvVec.reduce(_ | _) | io.udpMSHR.bits.waitMasVec.reduce(_ | _))
              m.state := Mux(hit, Mux(isClean, MSHRState.Free, MSHRState.WaitResp), MSHRState.AlreadySend)
            }
            is(MSHRState.WaitResp) {
              val hit = !m.waitSlvVec.reduce(_ | _) & !m.waitMasVec.reduce(_ | _)
              m.state := Mux(hit, Mux(m.isResp, MSHRState.BeSend, MSHRState.Free), MSHRState.WaitResp)
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


// --------------------------------- S0: Get task_s0 from MSHR -------------------------------- //
  /*
   * Get task_s0(req) from MSHRTable
   */
  val reqBeSendSetVec     = mshrTableReg.map(_.map(_.reqBeSend).reduce(_ | _))
  val reqBeSendSet        = RREncoder(reqBeSendSetVec.zip(mshrLockVecReg).map{ case(a, b) => a & !b }) // TODO: if dont need to read Dir, it should not be ctrl by mshrLockVecReg
  val reqBeSendWay        = RREncoder(mshrTableReg(reqBeSendSet).map(_.reqBeSend))
  val taskReq             = mshrTableReg(reqBeSendSet)(reqBeSendWay)

  /*
   * Get task_s0(resp) from MSHRTable
   */
  val respBeSendSetVec    = mshrTableReg.map(_.map(_.respBeSend).reduce(_ | _))
  val respBeSendSet       = RREncoder(respBeSendSetVec.zip(mshrLockVecReg).map{ case(a, b) => a & !b }) // TODO: if dont need to read Dir, it should not be ctrl by mshrLockVecReg
  val respBeSendWay       = RREncoder(mshrTableReg(respBeSendSet).map(_.respBeSend))
  val taskResp            = mshrTableReg(respBeSendSet)(respBeSendWay)

  /*
   * Get task_s0(evict) from MSHRTable
   */
  val evictBeSendId       = RREncoder(evictTableReg.map {case e => e.isBeSend & !mshrLockVecReg(e.set) }) // TODO: if dont need to read Dir, it should not be ctrl by mshrLockVecReg
  val taskEvict           = evictTableReg(evictBeSendId)


  /*
   * Select task_s0
   */
  task_s0.valid           := taskResp.isBeSend | taskReq.isBeSend
  task_s0.bits.readDir    := true.B // TODO
  //                                                 EvictTask                                 RespTask                       ReqTask
  task_s0.bits.addr       := Mux(taskEvict.isBeSend, taskEvict.addr(),  Mux(taskResp.isBeSend, taskResp.addr(respBeSendSet),  taskReq.addr(reqBeSendSet)))
  task_s0.bits.pipeId     := Mux(taskEvict.isBeSend, PipeID.RESP,       Mux(taskResp.isBeSend, PipeID.RESP,                   PipeID.REQ))
  task_s0.bits.mshrWay    := Mux(taskEvict.isBeSend, evictBeSendId,     Mux(taskResp.isBeSend, respBeSendWay,                 reqBeSendWay))
  task_s0.bits.reqMes     := Mux(taskEvict.isBeSend, taskEvict.reqMes,  Mux(taskResp.isBeSend, taskResp.reqMes,               taskReq.reqMes))
  task_s0.bits.respMes    := Mux(taskEvict.isBeSend, taskEvict.respMes, Mux(taskResp.isBeSend, taskResp.respMes,              taskReq.respMes))
  task_s0.bits.useEvict   := taskEvict.isBeSend
  canGo_s0                := canGo_s1 | !task_s1_g.valid


// ------------------------ S1: Read Dir and send task to MainPipe --------------------------//
  /*
   * Set Task S1 Value
   */
  task_s1_g.valid       := Mux(task_s0.valid, true.B, task_s1_g.valid & !canGo_s1)
  task_s1_g.bits        := Mux(task_s0.valid & canGo_s0, task_s0.bits, task_s1_g.bits)
  canGo_s1              := (io.pipeTask.ready | alreadySendTaskReg) & (io.dirRead.ready | alreadyReadDirReg | !task_s1_g.bits.readDir)

  /*
   * Send mpTask to Pipe
   */
  alreadySendTaskReg    := Mux(alreadySendTaskReg, !io.dirRead.fire, io.pipeTask.fire & !canGo_s1)
  io.pipeTask.valid     := task_s1_g.valid & !alreadySendTaskReg & task_s1_g.bits.readDir
  io.pipeTask.bits      := task_s1_g.bits

  /*
   * Read Directory
   */
  alreadyReadDirReg         := Mux(alreadyReadDirReg, !io.pipeTask.fire, io.dirRead.fire & !canGo_s1)
  io.dirRead.valid          := task_s1_g.valid & !alreadyReadDirReg
  io.dirRead.bits.addr      := task_s1_g.bits.addr
  io.dirRead.bits.pipeId    := task_s1_g.bits.pipeId



// ------------------------ S2: Dir Read MSHR and MSHR Resp to Dir --------------------------//
  when(io.dirReadMshr.valid) {
    io.mshrResp2Dir.zipWithIndex.foreach {
      case(r, i) =>
        if(i < djparam.nrMSHRWays) {
          r.valid := mshrTableReg(io.dirReadMshr.bits)(i).isValid
          r.bits  := mshrTableReg(io.dirReadMshr.bits)(i).addr(io.dirReadMshr.bits)
        } else {
          val id = i - djparam.nrMSHRWays
          r.valid := evictTableReg(id).isValid & evictTableReg(id).set === io.dirReadMshr.bits
          r.bits  := evictTableReg(id).addr()
        }
    }
  }.otherwise {
    io.mshrResp2Dir.foreach(_.valid := false.B)
    io.mshrResp2Dir.foreach(_.bits  := DontCare)
  }



// ------------------------------------- Assertion ---------------------------------------//
  // TODO





}