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
  val canGo_s2      = Wire(Bool())
  val task_s2       = WireInit(0.U.asTypeOf(Valid(new PipeTaskBundle())))
  // s3 basic signals
  val valid_s3      = Wire(Bool())
  val canGo_s3      = Wire(Bool()); canGo_s3 := DontCare // TODO: Del DontCare
  val dirCanGo_s3   = Wire(Bool())
  val taskNext_s3   = WireInit(0.U.asTypeOf(new PipeTaskBundle()))
  val task_s3_g     = RegInit(0.U.asTypeOf(Valid(new PipeTaskBundle())))
  val dirRes_s3     = WireInit(0.U.asTypeOf(Valid(new DirRespBundle())))
  val taskChipType  = Wire(UInt(ChipType.width.W))
  // s3 decode signals
  val inst_s3       = Wire(new InstBundle())
  val decode_s3     = Wire(new DecodeBundle())
  // s3 execute signals 1
  val taskSnp_s3    = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val taskRD_s3     = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val taskWD_s3     = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val rcDBReq_s3    = WireInit(0.U.asTypeOf(new DBRCReq()))
  val readDCU_s3    = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val writeDCU_s3   = WireInit(0.U.asTypeOf(new Req2NodeBundle()))
  val wSDir_s3      = WireInit(0.U.asTypeOf(io.dirWrite.s))
  val wSFDir_s3     = WireInit(0.U.asTypeOf(io.dirWrite.sf))
  // s3 execute signals: Set specific tasks value
  val todo_s3           = Wire(new OperationsBundle())
  val replace_s3        = Wire(Bool()) // replace self Directory
  val evictSF_s3        = Wire(Bool()) // replace snoop filter
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
  taskQ.io.enq        <> io.task
  task_s2.valid       := taskQ.io.deq.valid
  task_s2.bits        := taskQ.io.deq.bits
  taskQ.io.deq.ready  := canGo_s2

  canGo_s2            := canGo_s3 | !task_s3_g.valid

  // dir result queue
  dirResQ.io.enq.valid := io.dirResp.valid
  dirResQ.io.enq.bits  := io.dirResp.bits
  assert(Mux(io.dirResp.valid, dirResQ.io.enq.ready, true.B))

// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------- S3_Receive: Receive task and dirRes from s2 -------------------------------------//
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Recieve task_s2
   */
  task_s3_g.valid := Mux(task_s2.valid, true.B, task_s3_g.valid & !canGo_s3)
  taskNext_s3     := Mux(task_s2.valid & canGo_s2, task_s2.bits, task_s3_g.bits)
  task_s3_g.bits  := taskNext_s3

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
  canGo_s3              := DontCare // TODO



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
  inst_s3.opcode    := task_s3_g.bits.reqMes.opcode
  inst_s3.srcState  := Mux(task_s3_g.bits.readDir, srcState, ChiState.I)
  inst_s3.othState  := Mux(task_s3_g.bits.readDir, othState, ChiState.I)
  inst_s3.hnState   := Mux(task_s3_g.bits.readDir, hnState, ChiState.I)

  /*
   * Get Decode Result
   */
  decode_s3.decode(inst_s3, table = LocalReadDecode.table)
  dontTouch(decode_s3)

// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------- S3_Execute: Set specific tasks value -----------------------------------------//
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Send Snoop to RN-F
   */
  // taskSnp_s3

  /*
   * Send Read to SN(DDRC) / HN-F(CSN)
   */
  taskRD_s3             := DontCare
  taskRD_s3.addr        := Mux(decode_s3.readDCU, DontCare, task_s3_g.bits.addr) // TODO: Read DCU Addr
  taskRD_s3.mshrWay     := task_s3_g.bits.mshrWay
  taskRD_s3.useEvict    := task_s3_g.bits.useEvict
  taskRD_s3.tgtId       := Mux(taskChipType === ChipType.Local, ddrcNodeId.U, DontCare)
  taskRD_s3.srcId       := Mux(taskChipType === ChipType.Local, hnfNodeId.U, csnHnfNodeId.U)
  taskRD_s3.opcode      := decode_s3.rdOp
  taskRD_s3.expCompAck  := Mux(taskChipType === ChipType.Local, false.B, true.B)
  taskRD_s3.from.idL1   := io.sliceId
  taskRD_s3.from.idL2   := DontCare
  taskRD_s3.to.idL1     := Mux(taskChipType === ChipType.Local, IdL1.LOCALMAS.U, IdL1.CSNMAS.U)
  taskRD_s3.to.idL2     := DontCare
  taskRD_s3.replace     := false.B
  taskRD_s3.ReadDCU     := decode_s3.readDCU


  /*
   * Send Write / Dataless to SN(DDRC) / HN-F(CSN)
   */
  // taskWD_s3

  /*
   * Send Read / Clean to DataBuffer
   */
  // rcDBReq_s3

  /*
   * Send Read to SN(DCU)
   */
  // readDCU_s3

  /*
   * Send Write to SN(DCU)
   */
  // writeDCU_s3

  /*
   * Send Write to Self Directory
   */
  // wSDir_s3

  /*
   * Send Write to Snoop Filter Directory
   */
  // wSFDir_s3


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------- S3_Execute: Update MSHR ------------------------------------------------//
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Set todo_s3 value
   */
  todo_s3 := decode_s3

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
  val doRetry                 = retry_s3 & !retry2MSHR_s3_g
  val doUpdate                = !upd2MSHR_s3_g
  val doRepl                  = replace_s3 & !rep2MSHRl_s3_g
  val doEvict                 = evictSF_s3 & !evict2MSHR_s3_g
  val mshrTaskVec             = Seq(doRetry, doUpdate, doRepl, doEvict)

  //                                          Retry                                   Update                                Replace                 EvictSF
  io.udpMSHR.bits.updType     := Mux(doRetry, UpdMSHRType.RETRY,        Mux(doUpdate, UpdMSHRType.UPD,          Mux(doRepl, UpdMSHRType.REPL,       UpdMSHRType.EVICT)))
  io.udpMSHR.bits.addr        := Mux(doRetry, DontCare,                 Mux(doUpdate, DontCare,                 Mux(doRepl, dirRes_s3.bits.s.addr,  dirRes_s3.bits.sf.addr)))
  // Use In Retry or Update
  io.udpMSHR.bits.mshrWay     := task_s3_g.bits.mshrWay
  io.udpMSHR.bits.useEvict    := task_s3_g.bits.useEvict
  // Use In Update
  io.udpMSHR.bits.willUseWay  := replace_s3.asUInt + evictSF_s3.asUInt
  io.udpMSHR.bits.waitSlvVec.foreach(_ := todo_s3.reqToSlv) // TODO: Complete hasCSNIntf
  io.udpMSHR.bits.waitMasVec.foreach(_ := todo_s3.reqToMas) // TODO: Complete hasCSNIntf
//  require(!hasCSNIntf)
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
// ---------------------------------------------------------------------------------------------------------------------- /
  /*
   * Send Req to Node
   */
  val canSendReq    = valid_s3 & !retry_s3 & (upd2MSHR_s3_g | !(replace_s3 | evictSF_s3)) & !mshrLetRetry_s3
  val doneList      = Seq(done_s3_g.snoop, done_s3_g.readDown, done_s3_g.writeDown, done_s3_g.readDCU, done_s3_g.writeDCU, doneRepl_s3_g, doneEvict_s3_g)
  val todoList      = Seq(todo_s3.snoop     & !done_s3_g.snoop,
                          todo_s3.readDown  & !done_s3_g.readDown,
                          todo_s3.writeDown & !done_s3_g.writeDown,
                          todo_s3.readDCU   & !done_s3_g.readDCU,
                          todo_s3.writeDCU  & !done_s3_g.writeDCU,
                          replace_s3        & !doneRepl_s3_g,
                          evictSF_s3        & !doneEvict_s3_g)
  val toBeSendId    = PriorityEncoder(todoList)
  reqBeSend_s3(0)   := taskSnp_s3
  reqBeSend_s3(1)   := taskRD_s3
  reqBeSend_s3(2)   := taskWD_s3
  reqBeSend_s3(3)   := readDCU_s3
  reqBeSend_s3(4)   := writeDCU_s3
  reqBeSend_s3(5)   := 0.U.asTypeOf(new Req2NodeBundle()) // TODO: replace_s3
  reqBeSend_s3(6)   := 0.U.asTypeOf(new Req2NodeBundle()) // TODO: evictSF_s3
  io.req2Node.valid := canSendReq & todoList.reduce(_ | _)
  io.req2Node.bits  := reqBeSend_s3(toBeSendId)
  doneList.zipWithIndex.foreach { case(d, i) => d := Mux(d, !canGo_s3, io.req2Node.fire & toBeSendId === i.U) }
  












}