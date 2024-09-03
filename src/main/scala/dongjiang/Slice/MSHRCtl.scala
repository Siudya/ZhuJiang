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
  val tagOpt          = if(!useAddr) Some(UInt(mshrTagBits.W)) else None
  val bankOpt         = if(!useAddr) Some(UInt(bankBits.W)) else None
  val addrOpt         = if(useAddr) Some(UInt(djparam.addressBits.W)) else None
  def tag             = tagOpt.get
  def bank            = bankOpt.get
  def addr            = addrOpt.get
  // req mes
  val reqMes          = new ReqBaseMesBundle()
  // resp mes
  val waitSlvVec      = Vec(nrSlvIntf, Bool()) // Wait Snoop Resp
  val waitMasVec      = Vec(nrMasIntf, Bool()) // Wait Req Resp
  val respMes         = new Bundle {
    val slvResp       = Valid(UInt(ChiResp.width.W))
    val masResp       = Valid(UInt(ChiResp.width.W))
    val fwdState      = Valid(UInt(ChiResp.width.W))
    val dbid          = Valid(UInt(dbIdBits.W))
  }

  def isValid       = state =/= MSHRState.Free
  def isFree        = state === MSHRState.Free
  def isBeSend      = state === MSHRState.BeSend
  def isAlreadySend = state === MSHRState.AlreadySend
  def isWaitResp    = state === MSHRState.WaitResp
  def isResp        = respMes.slvResp.valid | respMes.masResp.valid | respMes.fwdState.valid | respMes.dbid.valid
  def isReq         = !isResp
  def respBeSend    = isBeSend & isResp
  def reqBeSend     = isBeSend & isReq
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
    val updLockVec    = Flipped(Decoupled(new MSHRSetBundle))
    // Directory Read Req
    val dirRead       = Decoupled(new DirReadBundle())
    // Directory Read MSHR Set Mes
    val dirReadMshr   = Flipped(Valid(new MSHRIndexBundle()))
    val mshrResp2Dir  = Output(Vec(djparam.nrMSHRWays, Valid(UInt(addressBits.W))))
    // Req Retry: Resp To Rn Node
    val retry2Node    = Decoupled(new Resp2NodeBundle())
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare
  dontTouch(io)



// --------------------- Reg / Wire declaration ------------------------//
  // mshrTable
  val mshrTableReg    = RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { VecInit(Seq.fill(djparam.nrMSHRWays) { 0.U.asTypeOf(new MSHREntry()) }) }))
  val evictTableReg   = RegInit(VecInit(Seq.fill(2) { 0.U.asTypeOf(new MSHREntry(useAddr = true)) })) // Use to save snp evict / evict req from resp process // TODO: Setting the right number of entries
  val mshrLockVecReg  = RegInit(VecInit(Seq.fill(djparam.nrMSHRSets) { false.B }))
  require(evictTableReg.length <= djparam.nrMSHRWays)
  // Transfer Req From Node To MSHREntry
  val mshrAlloc_s0    = WireInit(0.U.asTypeOf(new MSHREntry()))
  // retry mes
  val retryTo_s0_g    = RegInit(0.U.asTypeOf(Valid(new IDBundle())))
  val udpResp_s0_g    = RegInit(0.U.asTypeOf(Valid(new UpdateMSHRRespBundle())))
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
  val (blockTag, blockSet, blockBank) = parseMSHRAddress(io.req2Slice.bits.addr); dontTouch(blockTag); dontTouch(blockSet); dontTouch(blockBank)
  val nodeReqMatchVec = mshrTableReg(blockSet).map { case m => m.isValid & m.tag === blockTag & m.bank === blockBank }
  val nodeReqInvVec   = mshrTableReg(blockSet).map(_.isFree)
  val nodeReqInvWay   = PriorityEncoder(nodeReqInvVec)
  // evictTableReg
  val evictMatchVec   = evictTableReg.map { case m => m.isValid & m.addr === io.req2Slice.bits.addr }


  /*
   * Get Block Message
   */
  val blockByMHSR     = nodeReqMatchVec.reduce(_ | _) | PopCount(nodeReqInvVec) <= 1.U | !evictMatchVec.reduce(_ | _)
  val blockByPipeReq  = io.udpMSHR.valid & io.udpMSHR.bits.isReq
  val canReceiveNode  = blockByMHSR | blockByPipeReq


  /*
   * Transfer Req From Node To MSHREntry
   */
  val (reqTag, reqSet, reqBank) = parseMSHRAddress(io.req2Slice.bits.addr); dontTouch(reqTag); dontTouch(reqSet); dontTouch(reqBank)
  mshrAlloc_s0          := DontCare
  mshrAlloc_s0.tag      := reqTag
  mshrAlloc_s0.bank     := reqSet
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
   * TODO: Completer Retry Logic In ReqBuf
   */
  assert(!io.retry2Node.valid)
  io.retry2Node.valid         := retryTo_s0_g.valid
  io.retry2Node.bits          := DontCare
  io.retry2Node.bits.reqRetry := true.B
  io.retry2Node.bits.to       := retryTo_s0_g.bits


// ------------------------ S0: Receive Req From Pipe or Let It Retry-------------------------- //
  // mshrTableReg
  val pipeReqInvVec       = mshrTableReg(io.udpMSHR.bits.mshrSet).map(_.isFree)
  val pipeReqInvWay       = PriorityEncoder(pipeReqInvVec)
  // evictTableReg
  val evictInvVec         = evictTableReg.map(_.isFree)
  val evictInvId          = PriorityEncoder(evictInvVec)

  val mshrCanReceivePipe  = PopCount(pipeReqInvVec) > 0.U
  val evictCanReceivePipe = PopCount(evictInvVec) > 0.U
  val pipeReqSaveInEvict  = !mshrCanReceivePipe & evictCanReceivePipe

  io.udpMSHR.ready          := true.B

  udpResp_s0_g.valid        := io.udpMSHR.fire & io.udpMSHR.bits.isReq
  udpResp_s0_g.bits.pipeId  := io.udpMSHR.bits.pipeId
  udpResp_s0_g.bits.retry   := !(mshrCanReceivePipe | evictCanReceivePipe)
  udpResp_s0_g.bits.mshrWay := Mux(pipeReqSaveInEvict, evictInvId, pipeReqInvWay)
  udpResp_s0_g.bits.useEvict := pipeReqSaveInEvict

  io.udpResp                := udpResp_s0_g

// ----------------------------------- S0: Update MHSR Table Value ----------------------------------- //
  /*
   * Update MSHRTable
   */
  mshrTableReg.zipWithIndex.foreach {
    case(m, i) =>
      m.zipWithIndex.foreach {
        case(m, j) =>
          when(task_s0.valid & canGo_s0 & parseMSHRAddress(task_s0.bits.addr)._2 === i.U & task_s0.bits.mshrWay === j.U & task_s0.bits.useMSHR) {
            m.respMes := 0.U.asTypeOf(m.respMes)
          /*
           * Pipe(isReq) Update mshrTable value
           */
          }.elsewhen(io.udpMSHR.valid & io.udpMSHR.bits.isReq & mshrCanReceivePipe & io.udpMSHR.bits.mshrSet === i.U & pipeReqInvWay === j.U) {
            // TODO
          /*
           * Pipe(isUpdate) Update mshrTable value
           */
          }.elsewhen(io.udpMSHR.valid & io.udpMSHR.bits.isUpdate & io.udpMSHR.bits.mshrMatch(i.U, j.U)) {
            // TODO
          /*
           * Resp Update mshrTable value
           */
          }.elsewhen(io.resp2Slice.valid & io.resp2Slice.bits.useMSHR & io.resp2Slice.bits.mshrMatch(i.U, j.U)) {
            // TODO
          /*
           * Req Update mshrTable value
           */
          }.elsewhen(io.req2Slice.fire & canReceiveNode & i.U === reqSet & j.U === nodeReqInvWay) {
            m := mshrAlloc_s0
          }
      }
  }
  io.udpMSHR.ready    := true.B
  io.resp2Slice.ready := true.B


  /*
   * Update EVICTTable
   */
  evictTableReg.zipWithIndex.foreach {
    case(e, i) =>
      when(io.udpMSHR.valid & io.udpMSHR.bits.isReq & pipeReqSaveInEvict & evictInvId === i.U) {
        // TODO
      }
  }


  /*
   * Update Lock Vec
   */
  mshrLockVecReg.zipWithIndex.foreach {
    case(lock, i) =>
      when(io.updLockVec.valid & io.updLockVec.bits.mshrSet === i.U) {
        lock := false.B
      }.elsewhen(task_s0.valid & canGo_s0 & parseMSHRAddress(task_s0.bits.addr)._2 === i.U) {
        lock := true.B
      }
  }
  io.updLockVec.ready := true.B



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
              val nodeHit = io.req2Slice.valid & canReceiveNode & i.U === reqSet & j.U === nodeReqInvWay
              val pipeHit = io.udpMSHR.valid & io.udpMSHR.bits.isReq & mshrCanReceivePipe & io.udpMSHR.bits.mshrSet === i.U & pipeReqInvWay === j.U
              m.state := Mux(nodeHit | pipeHit, MSHRState.BeSend, MSHRState.Free)
            }
            is(MSHRState.BeSend) {
              val hit = task_s0.valid & canGo_s0 & parseMSHRAddress(task_s0.bits.addr)._2 === i.U & task_s0.bits.mshrWay === j.U & task_s0.bits.useMSHR
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
  val reqBeSendSet        = RREncoder(reqBeSendSetVec.zip(mshrLockVecReg).map{ case(a, b) => a & !b })
  val reqBeSendWay        = RREncoder(mshrTableReg(reqBeSendSet).map(_.reqBeSend))
  val taskReq             = mshrTableReg(reqBeSendSet)(reqBeSendWay)

  /*
   * Get task_s0(resp) from MSHRTable
   */
  val respBeSendSetVec    = mshrTableReg.map(_.map(_.respBeSend).reduce(_ | _))
  val respBeSendSet       = RREncoder(respBeSendSetVec.zip(mshrLockVecReg).map{ case(a, b) => a & !b })
  val respBeSendWay       = RREncoder(mshrTableReg(respBeSendSet).map(_.respBeSend))
  val taskResp            = mshrTableReg(respBeSendSet)(respBeSendWay)

  /*
   * Get task_s0(evict) from MSHRTable
   */
  val evictBeSendId       = RREncoder(evictTableReg.map {case e => e.isBeSend & !mshrLockVecReg(parseMSHRAddress(e.addr)._2) })
  val taskEvict           = evictTableReg(evictBeSendId)


  /*
   * Select task_s0
   */
  val taskReqMes          = PriorityMux(Seq(
    taskEvict.isBeSend    -> taskEvict.reqMes,
    taskResp.isBeSend     -> taskResp.reqMes,
    taskReq.isBeSend      -> taskReq.reqMes
  ))
  val taskRespMes         = PriorityMux(Seq(
    taskEvict.isBeSend    -> taskEvict.respMes,
    taskResp.isBeSend     -> taskResp.respMes,
    taskReq.isBeSend      -> taskReq.respMes
  ))
  val taskAddr            = PriorityMux(Seq(
    taskEvict.isBeSend    -> taskEvict.addr,
    taskResp.isBeSend     -> Cat(taskResp.tag, respBeSendSet, 0.U(offsetBits.W)),
    taskReq.isBeSend      -> Cat(taskReq.tag, reqBeSendSet, 0.U(offsetBits.W)),
  ))
  val taskMshrWay         = PriorityMux(Seq(
    taskEvict.isBeSend    -> evictBeSendId,
    taskResp.isBeSend     -> respBeSendWay,
    taskReq.isBeSend      -> reqBeSendWay,
  ))
  val taskPipeId          = PriorityMux(Seq(
    taskEvict.isBeSend    -> PipeID.RESP,
    taskResp.isBeSend     -> PipeID.RESP,
    taskReq.isBeSend      -> PipeID.REQ,
  ))
  task_s0.valid           := taskResp.isBeSend | taskReq.isBeSend | taskEvict.isBeSend
  task_s0.bits.addr       := taskAddr
  task_s0.bits.pipeId     := taskPipeId
  task_s0.bits.mshrWay    := taskMshrWay
  task_s0.bits.useEvict   := taskEvict.isBeSend
  task_s0.bits.reqMes     := taskReqMes
  task_s0.bits.respMes    := taskRespMes
  canGo_s0                := canGo_s1 | !task_s1_g.valid


// ------------------------ S1: Read Dir and send task to MainPipe --------------------------//
  /*
   * Set Task S1 Value
   */
  task_s1_g.valid       := Mux(task_s0.valid, true.B, task_s1_g.valid & !canGo_s1)
  task_s1_g.bits        := Mux(task_s0.valid & canGo_s0, task_s0.bits, task_s1_g.bits)
  canGo_s1              := (io.pipeTask.ready | alreadySendTaskReg) & (io.dirRead.ready | alreadyReadDirReg)

  /*
   * Send mpTask to Pipe
   */
  alreadySendTaskReg    := Mux(alreadySendTaskReg, !io.dirRead.fire, io.pipeTask.fire & !canGo_s1)
  io.pipeTask.valid     := task_s1_g.valid & !alreadySendTaskReg
  io.pipeTask.bits      := task_s1_g.bits

  /*
   * Read Directory
   */
  alreadyReadDirReg     := Mux(alreadyReadDirReg, !io.pipeTask.fire, io.dirRead.fire & !canGo_s1)
  io.dirRead.valid      := task_s1_g.valid & !alreadyReadDirReg
  io.dirRead.bits.addr  := task_s1_g.bits.addr
  io.dirRead.bits.mshrWay   := task_s1_g.bits.mshrWay
  io.dirRead.bits.useEvict  := task_s1_g.bits.useEvict
  io.dirRead.bits.pipeId    := task_s1_g.bits.pipeId



// ------------------------ S2: Dir Read MSHR and MSHR Resp to Dir --------------------------//
  when(io.dirReadMshr.valid) {
    when(!io.dirReadMshr.bits.useEvict) {
      io.mshrResp2Dir.zip(mshrTableReg(io.dirReadMshr.bits.mshrSet)).foreach {
        case(a, b) =>
          a.valid := b.isValid
          a.bits  := Cat(b.tag, io.dirReadMshr.bits.mshrSet, b.bank, 0.U(offsetBits.W))
      }
    }.otherwise {
      io.mshrResp2Dir.zipWithIndex.foreach {
        case(a, i) =>
          if(i < evictTableReg.length) {
            a.valid := evictTableReg(i).isValid & parseMSHRAddress(evictTableReg(i).addr)._2 === io.dirReadMshr.bits.mshrSet
            a.bits  := evictTableReg(i).addr
          } else {
            a.valid := false.B
            a.bits  := DontCare
          }
      }
    }
  }.otherwise {
    io.mshrResp2Dir.foreach(_.valid := false.B)
    io.mshrResp2Dir.foreach(_.bits  := DontCare)
  }



// ------------------------------------- Assertion ---------------------------------------//
  // TODO





}