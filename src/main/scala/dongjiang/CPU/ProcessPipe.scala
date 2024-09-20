package DONGJIANG.CPU

import DONGJIANG._
import DONGJIANG.DECODE._
import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.ParallelLookUp

class ProcessPipe()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val sliceId     = Input(UInt(bankBits.W))
    // Req To DataBuffer
    val dbRCReq   = Decoupled(new DBRCReq())
    // Resp From Directory
    val dirResp     = Flipped(Valid(new DirRespBundle()))
    // Write Req To Directory
    val dirWrite    = new DirWriteBundle()
    // Task From MSHR
    val task        = Flipped(Decoupled(new PipeTaskBundle()))
    // Update Task To MSHR
    val udpMSHR     = Decoupled(new UpdateMSHRReqBundle())
    val mshrResp    = Flipped(Valid(new UpdateMSHRRespBundle()))
    val updLockMSHR = Decoupled(new MSHRSetBundle)
    // Req To Node
    val req2Node    = Decoupled(new Req2NodeBundle())
    // Resp To Node
    val resp2Node   = Decoupled(new Resp2NodeBundle())
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare

// --------------------- Modules declaration ------------------------//
  val taskQ   = Module(new Queue(new PipeTaskBundle(), entries = djparam.nrMpTaskQueue, pipe = false, flow = false))
  val dirResQ = Module(new Queue(new DirRespBundle(), entries = djparam.nrMpTaskQueue + 2, pipe = true, flow = true)) // one for mp_s1 read Dir before send task to mp_2, one for mp_s3

// --------------------- Reg/Wire declaration ------------------------//
  // s2 signals
  val canGo_s2            = Wire(Bool())
  val task_s2             = WireInit(0.U.asTypeOf(Valid(new PipeTaskBundle())))
  // s3 basic signals
  val valid_s3            = Wire(Bool())
  val canGo_s3            = Wire(Bool());
  val dirCanGo_s3         = Wire(Bool())
  val taskNext_s3         = WireInit(0.U.asTypeOf(new PipeTaskBundle()))
  val task_s3_g           = RegInit(0.U.asTypeOf(Valid(new PipeTaskBundle())))
  val dirRes_s3           = WireInit(0.U.asTypeOf(Valid(new DirRespBundle())))
  val taskChipType        = Wire(UInt(ChipType.width.W))
  // s3 decode signals
  val inst_s3             = Wire(new InstBundle());  dontTouch(inst_s3)
  val decode_s3           = Wire(new DecodeBundle());  dontTouch(decode_s3)
  // s3 execute signals 1
  val taskSnp_s3          = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val taskRD_s3           = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val taskWD_s3           = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val rcDBReq_s3          = WireInit(0.U.asTypeOf(new DBRCReq()))
  val readDCU_s3          = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val writeDCU_s3         = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val wSDir_s3            = WireInit(0.U.asTypeOf(io.dirWrite.s.bits))
  val wSFDir_s3           = WireInit(0.U.asTypeOf(io.dirWrite.sf.bits))
  val commit_s3           = WireInit(0.U.asTypeOf(new Resp2NodeBundle()))
  // s3 execute signals: Set specific tasks value
  val todo_s3             = WireInit(0.U.asTypeOf(new OperationsBundle()))
  val replace_s3          = Wire(Bool()) // replace self Directory
  val evictSF_s3          = Wire(Bool()) // replace snoop filter
  // s3 execute signals: Update mshr or do some req
  val retry_s3            = Wire(Bool())
  val retry2MSHR_s3_g     = RegInit(false.B)
  val upd2MSHR_s3_g       = RegInit(false.B)
  val rep2MSHRl_s3_g      = RegInit(false.B)
  val evict2MSHR_s3_g     = RegInit(false.B)
  val mshrLetRetry_s3     = Wire(Bool())
  // s3 execute signals: Execute specific tasks
  val done_s3_g           = RegInit(0.U.asTypeOf(new OperationsBundle()))
  val doneRepl_s3_g       = RegInit(false.B)
  val doneEvict_s3_g      = RegInit(false.B)
  val reqBeSend_s3        = Wire(Vec(7, new Req2NodeBundle()))






// ---------------------------------------------------------------------------------------------------------------------- //
// ----------------------------------------------- S2: Buffer input task/dirRes ----------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  // task queue
  taskQ.io.enq          <> io.task
  task_s2.valid         := taskQ.io.deq.valid
  task_s2.bits          := taskQ.io.deq.bits
  taskQ.io.deq.ready    := canGo_s2

  canGo_s2              := canGo_s3 | !task_s3_g.valid

  // dir result queue
  dirResQ.io.enq.valid  := io.dirResp.valid
  dirResQ.io.enq.bits   := io.dirResp.bits
  assert(Mux(io.dirResp.valid, dirResQ.io.enq.ready, true.B))


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------- S3_Receive: Receive task and dirRes from s2 -------------------------------------//
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Recieve task_s2
   */
  task_s3_g.valid       := Mux(task_s2.valid, true.B, task_s3_g.valid & !canGo_s3)
  taskNext_s3           := Mux(task_s2.valid & canGo_s2, task_s2.bits, task_s3_g.bits)
  task_s3_g.bits        := taskNext_s3

  /*
   * Recieve dirRes
   */
  dirRes_s3.valid       := dirResQ.io.deq.valid
  dirRes_s3.bits        := dirResQ.io.deq.bits
  dirResQ.io.deq.ready  := dirCanGo_s3

  /*
   * S3 base ctrl logic
   */
  dirCanGo_s3           := canGo_s3 & task_s3_g.valid & taskNext_s3.readDir
  valid_s3              := Mux(task_s3_g.bits.readDir, task_s3_g.valid & dirRes_s3.valid, task_s3_g.valid)



// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------- S3_Decode: Decode by task Message and Dir Result ------------------------------------//
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Parse Dir Result
   */
  val metaId    = getMetaIdBySrcID(task_s3_g.bits.reqMes.srcID)
  val srcHit    = dirRes_s3.bits.sf.hit & !dirRes_s3.bits.sf.metaVec(metaId).isInvalid
  val srcState  = Mux(srcHit, dirRes_s3.bits.sf.metaVec(metaId).state, ChiState.I)

  val othHit    = dirRes_s3.bits.sf.hit & (PopCount(dirRes_s3.bits.sf.metaVec.map(!_.isInvalid)) > srcHit.asUInt)
  val sfHitId   = PriorityEncoder(dirRes_s3.bits.sf.metaVec.map(!_.isInvalid))
  val othState  = Mux(othHit, dirRes_s3.bits.sf.metaVec(sfHitId).state, ChiState.I)

  val hnHit     = dirRes_s3.bits.s.hit
  val hnState   = Mux(hnHit, dirRes_s3.bits.s.metaVec(0).state, ChiState.I)

  /*
   * Set Inst value
   */
  taskChipType      := getChipTypeByAddr(task_s3_g.bits.addr)
  inst_s3           := DontCare
  inst_s3.chipType  := taskChipType
  inst_s3.channel   := task_s3_g.bits.reqMes.channel
  inst_s3.opcode    := task_s3_g.bits.reqMes.opcode
  inst_s3.srcState  := Mux(task_s3_g.bits.readDir, srcState, ChiState.I)
  inst_s3.othState  := Mux(task_s3_g.bits.readDir, othState, ChiState.I)
  inst_s3.hnState   := Mux(task_s3_g.bits.readDir, hnState, ChiState.I)
  inst_s3.respType  := Cat(task_s3_g.bits.respMes.masResp.valid,  // Read Down Resp
                           task_s3_g.bits.respMes.fwdState.valid, // Snoop Fwd Resp
                           task_s3_g.bits.respMes.slvResp.valid)  // Snoop Resp
  inst_s3.snpResp   := task_s3_g.bits.respMes.slvResp.bits
  inst_s3.fwdState  := task_s3_g.bits.respMes.fwdState.bits
  inst_s3.rdResp    := task_s3_g.bits.respMes.masResp.bits
  inst_s3.respHasData := task_s3_g.bits.respMes.masDBID.valid | task_s3_g.bits.respMes.slvDBID.valid
  assert(Mux(valid_s3, !inst_s3.chipType === ChipType.CSN, true.B), "TODO")

  /*
   * Get Decode Result
   */
  decode_s3.decode(inst_s3, table = LocalReadDecode.table)
  when(valid_s3) { assert(decode_s3.asUInt =/= 0.U, "DECODE ERROR: No inst match in decode table") }


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------- S3_Execute: Set specific tasks value -----------------------------------------//
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Send Snoop to RN-F
   */
  // taskSnp_s3
  assert(Mux(valid_s3, !decode_s3.snoop, true.B), "TODO")

  /*
   * Send Read to SN(DDRC) / HN-F(CSN)
   */
  taskRD_s3             := DontCare
  taskRD_s3.addr        := task_s3_g.bits.addr
  taskRD_s3.mshrWay     := task_s3_g.bits.mshrWay
  taskRD_s3.useEvict    := task_s3_g.bits.useEvict
  taskRD_s3.tgtID       := Mux(taskChipType === ChipType.Local, ddrcNodeId.U, DontCare); assert(Mux(valid_s3, !inst_s3.chipType === ChipType.CSN, true.B), "TODO")
  taskRD_s3.srcID       := Mux(taskChipType === ChipType.Local, hnfNodeId.U, csnHnfNodeId.U)
  taskRD_s3.opcode      := decode_s3.rdOp
  taskRD_s3.expCompAck  := Mux(taskChipType === ChipType.Local, false.B, true.B)
  taskRD_s3.from.IncoId := io.sliceId
  taskRD_s3.to.IncoId   := Mux(taskChipType === ChipType.Local, IncoID.LOCALMAS.U, IncoID.CSNMAS.U)
  taskRD_s3.replace     := false.B
  taskRD_s3.ReadDCU     := false.B

  /*
   * Send Write / Dataless to SN(DDRC) / HN-F(CSN)
   */
  // taskWD_s3
  assert(Mux(valid_s3, !decode_s3.writeDown, true.B), "TODO")

  /*
   * Send Read / Clean to DataBuffer
   */
  val rcDBID            = Mux(task_s3_g.bits.respMes.slvDBID.valid, task_s3_g.bits.respMes.slvDBID.bits, task_s3_g.bits.respMes.masDBID.bits)
  assert(!(task_s3_g.bits.respMes.slvDBID.valid & task_s3_g.bits.respMes.masDBID.valid))
  rcDBReq_s3.to         := task_s3_g.bits.reqMes.from
  rcDBReq_s3.isRead     := decode_s3.rDB2Src
  rcDBReq_s3.isClean    := decode_s3.cleanDB
  rcDBReq_s3.dbid       := rcDBID


  /*
   * Send Read to SN(DCU)
   */
  // readDCU_s3
  assert(Mux(valid_s3, !decode_s3.readDCU, true.B), "TODO")

  /*
   * Send Write to SN(DCU)
   */
  // writeDCU_s3
  assert(Mux(valid_s3, !decode_s3.writeDCU, true.B), "TODO")

  /*
   * Send Write to Self Directory
   */
  wSDir_s3.addr             := task_s3_g.bits.addr
  wSDir_s3.wayOH            := dirRes_s3.bits.s.wayOH
  wSDir_s3.metaVec(0).state := decode_s3.hnState
  wSDir_s3.replMes          := dirRes_s3.bits.s.replMes

  /*
   * Send Write to Snoop Filter Directory
   */
  wSFDir_s3.addr            := task_s3_g.bits.addr
  wSFDir_s3.wayOH           := dirRes_s3.bits.sf.wayOH
  wSFDir_s3.replMes         := dirRes_s3.bits.sf.replMes
  wSFDir_s3.metaVec.zip(dirRes_s3.bits.sf.metaVec).zipWithIndex.foreach {
    case((a, b), i) =>
      when(!b.isInvalid & i.U =/= metaId) { a.state := decode_s3.othState }
      .elsewhen(i.U === metaId)           { a.state := decode_s3.srcState }
      .otherwise                          { a.state := ChiState.I }
  }

  /*
   * Send Commit to S4
   */
  commit_s3.channel         := decode_s3.respChnl
  commit_s3.opcode          := decode_s3.respOp
  commit_s3.mshrWay         := task_s3_g.bits.mshrWay
  commit_s3.useEvict        := task_s3_g.bits.useEvict
  commit_s3.srcID           := task_s3_g.bits.reqMes.srcID
  commit_s3.txnID           := task_s3_g.bits.reqMes.txnID
  commit_s3.resp            := decode_s3.resp
  commit_s3.dbid            := rcDBID
  commit_s3.needReadDB      := !decode_s3.rDB2Src & decode_s3.respChnl === CHIChannel.DAT
  commit_s3.fwdState        := DontCare
  commit_s3.expCompAck      := task_s3_g.bits.reqMes.expCompAck



// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------- S3_Execute: Update MSHR ------------------------------------------------//
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Set todo_s3 value
   */
  when(valid_s3) { todo_s3 := decode_s3 }

  /*
   * Send Retry to MSHR When need write Dir but cant do it
   *
   */
  retry_s3    := todo_s3.wSDir & dirRes_s3.bits.s.replRetry | todo_s3.wSFDir & dirRes_s3.bits.sf.replRetry
  replace_s3  := todo_s3.wSDir & !hnHit & !dirRes_s3.bits.s.metaVec(0).isInvalid
  evictSF_s3  := todo_s3.wSFDir & !srcHit & !othHit & dirRes_s3.bits.s.metaVec.map(!_.isInvalid).reduce(_ | _)

  /*
   * Update MSHR Mes or let task retry
   */
  val needUpdMSHR             = todo_s3.reqToMas | todo_s3.reqToSlv | replace_s3 | evictSF_s3
  val doRetry                 = retry_s3 & !retry2MSHR_s3_g
  val doUpdate                = needUpdMSHR & !upd2MSHR_s3_g
  val doRepl                  = replace_s3 & !rep2MSHRl_s3_g
  val doEvict                 = evictSF_s3 & !evict2MSHR_s3_g
  val mshrTaskVec             = Seq(doRetry, doUpdate, doRepl, doEvict)
  //                                          Retry                                   Update                                Replace                 EvictSF
  io.udpMSHR.bits.updType     := Mux(doRetry, UpdMSHRType.RETRY,        Mux(doUpdate, UpdMSHRType.UPD,          Mux(doRepl, UpdMSHRType.REPL,       UpdMSHRType.EVICT)))
  io.udpMSHR.bits.addr        := Mux(doRetry, DontCare,                 Mux(doUpdate, DontCare,                 Mux(doRepl, dirRes_s3.bits.s.addr,  dirRes_s3.bits.sf.addr)))
  // Use In Retry or Update
  io.udpMSHR.bits.mshrWay     := task_s3_g.bits.mshrWay
  io.udpMSHR.bits.useEvict    := task_s3_g.bits.useEvict
  // Use In Update // TODO: Complete hasCSNIntf
  io.udpMSHR.bits.willUseWay  := replace_s3.asUInt + evictSF_s3.asUInt
  io.udpMSHR.bits.waitIntfVec := (Mux(todo_s3.reqToSlv | evictSF_s3, UIntToOH(IncoID.LOCALSLV.U), 0.U) |
                                  Mux(todo_s3.reqToMas | replace_s3, UIntToOH(IncoID.LOCALMAS.U), 0.U)).asBools
  require(!hasCSNIntf)
  // Common
  io.udpMSHR.bits.pipeId      := task_s3_g.bits.pipeId
  io.udpMSHR.valid            := valid_s3 & mshrTaskVec.reduce(_ | _)

  /*
   * Receive mshrResp
   */
  retry2MSHR_s3_g             := Mux(retry2MSHR_s3_g, !canGo_s3, io.udpMSHR.fire & io.udpMSHR.bits.isRetry)
  upd2MSHR_s3_g               := Mux(upd2MSHR_s3_g,   !canGo_s3, (io.mshrResp.valid & io.mshrResp.bits.isUpdate) |
                                                                 (io.udpMSHR.fire & io.udpMSHR.bits.isUpdate & io.udpMSHR.bits.willUseWay === 0.U))
  rep2MSHRl_s3_g              := Mux(rep2MSHRl_s3_g,  !canGo_s3, io.mshrResp.valid & io.mshrResp.bits.isRepl)
  evict2MSHR_s3_g             := Mux(evict2MSHR_s3_g, !canGo_s3, io.mshrResp.valid & io.mshrResp.bits.isEvict)

  mshrLetRetry_s3             := io.mshrResp.fire & io.mshrResp.bits.retry



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------ S3_Execute: Execute specific tasks value based on decode results -----------------------------//
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Reset Done Reg When Get New Task Form S2
   */
  val s2Fire        = task_s2.valid & canGo_s2
  val rstDone       = s2Fire

  /*
   * Send Req to Node
   */
  val canSendReq    = valid_s3 & !retry_s3 & (upd2MSHR_s3_g | !(replace_s3 | evictSF_s3)) & !mshrLetRetry_s3
  val reqDoneList   = Seq(done_s3_g.snoop, done_s3_g.readDown, done_s3_g.writeDown, done_s3_g.readDCU, done_s3_g.writeDCU, doneRepl_s3_g, doneEvict_s3_g)
  val reqTodoList   = Seq(todo_s3.snoop     & !done_s3_g.snoop,
                          todo_s3.readDown  & !done_s3_g.readDown,
                          todo_s3.writeDown & !done_s3_g.writeDown,
                          todo_s3.readDCU   & !done_s3_g.readDCU,
                          todo_s3.writeDCU  & !done_s3_g.writeDCU,
                          replace_s3        & !doneRepl_s3_g,
                          evictSF_s3        & !doneEvict_s3_g)
  val toBeSendId    = PriorityEncoder(reqTodoList)
  reqBeSend_s3(0)   := taskSnp_s3
  reqBeSend_s3(1)   := taskRD_s3
  reqBeSend_s3(2)   := taskWD_s3
  reqBeSend_s3(3)   := readDCU_s3
  reqBeSend_s3(4)   := writeDCU_s3
  reqBeSend_s3(5)   := 0.U.asTypeOf(new Req2NodeBundle()) // TODO: replace_s3
  reqBeSend_s3(6)   := 0.U.asTypeOf(new Req2NodeBundle()) // TODO: evictSF_s3
  io.req2Node.valid := canSendReq & reqTodoList.reduce(_ | _)
  io.req2Node.bits  := reqBeSend_s3(toBeSendId)
  reqDoneList.zipWithIndex.foreach { case(d, i) => d := Mux(rstDone, false.B, Mux(d, !canGo_s3, io.req2Node.fire & toBeSendId === i.U)) }
  // req
  val reqDone       = PopCount(reqTodoList) === 0.U | (PopCount(reqTodoList) === 1.U & io.req2Node.fire)


  /*
   * Send Write Req to Directory
   */
  // self
  io.dirWrite.s.valid   := valid_s3 & todo_s3.wSDir & !done_s3_g.wSDir
  io.dirWrite.s.bits    := wSDir_s3
  done_s3_g.wSDir       := Mux(rstDone, false.B, Mux(done_s3_g.wSDir, !canGo_s3, io.dirWrite.s.fire))
  // sf
  io.dirWrite.sf.valid  := valid_s3 & todo_s3.wSFDir & !done_s3_g.wSFDir
  io.dirWrite.sf.bits   := wSFDir_s3
  done_s3_g.wSFDir      := Mux(rstDone, false.B, Mux(done_s3_g.wSFDir, !canGo_s3, io.dirWrite.sf.fire))
  // dir
  val dirTodoList       = Seq(todo_s3.wSDir  & !done_s3_g.wSDir  & !io.dirWrite.s.fire,
                              todo_s3.wSFDir & !done_s3_g.wSFDir & !io.dirWrite.sf.fire)
  val dirDone           = PopCount(dirTodoList) === 0.U


  /*
   * Send Read or Clean Req to DataBuffer
   */
  io.dbRCReq.valid      := valid_s3 & ((todo_s3.rDB2Src & !done_s3_g.rDB2Src) | (todo_s3.cleanDB & !done_s3_g.cleanDB))
  io.dbRCReq.bits       := rcDBReq_s3
  done_s3_g.rDB2Src     := Mux(rstDone, false.B, Mux(done_s3_g.rDB2Src, !canGo_s3, io.dbRCReq.fire))
  done_s3_g.cleanDB     := Mux(rstDone, false.B, Mux(done_s3_g.cleanDB, !canGo_s3, io.dbRCReq.fire))
  val rcDBTodoList      = Seq(todo_s3.rDB2Src & !done_s3_g.rDB2Src & !io.dbRCReq.fire,
                              todo_s3.cleanDB & !done_s3_g.cleanDB & !io.dbRCReq.fire)
  val rcDBDone          = PopCount(rcDBTodoList) === 0.U

  /*
   * Send Commit to S4
   */
  io.resp2Node.valid    := valid_s3 & todo_s3.commit & !done_s3_g.commit
  io.resp2Node.bits     := commit_s3
  done_s3_g.commit      := Mux(rstDone, false.B, Mux(done_s3_g.commit, !canGo_s3, io.resp2Node.fire))
  val comDone           = !(todo_s3.commit & !done_s3_g.commit & !io.resp2Node.fire)

  /*
   * Set Can Go S3 Value
   */
  canGo_s3              := valid_s3 & reqDone & dirDone & rcDBDone & comDone




// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------- S3_Execute: UnLock MshrLockVec ------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Update MshrLockVec:
   * 1. S3 done but task dont need to commit
   */
  io.updLockMSHR.valid        := valid_s3 & canGo_s3 & task_s3_g.bits.readDir
  io.updLockMSHR.bits.mshrSet := task_s3_g.bits.mSet


// ----------------------------------------------------- Assertion ------------------------------------------------------ //
  // S3
  val cnt_s3_g  = RegInit(0.U(64.W))
  cnt_s3_g      := Mux(!valid_s3 | canGo_s3, 0.U, cnt_s3_g + 1.U)
  assert(cnt_s3_g < TIMEOUT_EXU.U, "ProcessPipe[0x%x] EXECUTE ADDR[0x%x] OP[0x%x] TIMEOUT", task_s3_g.bits.pipeId, task_s3_g.bits.addr, task_s3_g.bits.reqMes.opcode)

  // Other
  assert(!valid_s3 | !todo_s3.asUInt.asBools.zip(done_s3_g.asUInt.asBools).map { case(todo, done) => !todo & done }.reduce(_ | _))
}