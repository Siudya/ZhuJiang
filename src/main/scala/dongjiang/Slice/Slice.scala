package DONGJIANG.SLICE

import DONGJIANG._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils._
import Utils.FastArb._

class Slice()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val valid           = Input(Bool())
    val sliceId         = Input(UInt(bankBits.W))
    // slice ctrl signals: RnNode <> Slice
    val req2Slice       = Flipped(Decoupled(new Req2SliceBundle()))
    val resp2Node       = Decoupled(new Resp2NodeBundle())
    val req2Node        = Decoupled(new Req2NodeBundle())
    val resp2Slice      = Flipped(Decoupled(new Resp2SliceBundle()))
    // slice DataBuffer signals: RnNode <> Slice
    val nodeDBSigs      = Flipped(new DBBundle(hasDBRCReq = true))
  })

// --------------------- Modules declaration ------------------------//
  val dataBuffer    = Module(new DataBuffer())
  val directory     = Module(new DirectoryWrapper())
  val reqPipe       = Module(new SlicePipe())
  val respPipe      = Module(new SlicePipe())
  val mshrCtl       = Module(new MSHRCtl())
  val mpReqQueue    = Module(new Queue(gen = new Req2NodeBundle(), entries = djparam.nrMpReqQueue, pipe = true, flow = true))
  val mpRespQueue   = Module(new Queue(gen = new Resp2NodeBundle(),entries = djparam.nrMpRespQueue, pipe = true, flow = true))

// --------------------------- Connection ---------------------------//
  dataBuffer.io.sliceId     := io.sliceId
  dataBuffer.io.node2db     <> io.nodeDBSigs
  dataBuffer.io.mpDBRCReq   <> fastPriorityArbDec(Seq(respPipe.io.mpDBRCReq, reqPipe.io.mpDBRCReq))


  directory.io.sliceId      := io.sliceId
  directory.io.dirRead      <> mshrCtl.io.dirRead
  directory.io.dirWrite.s   <> fastPriorityArbDec(Seq(respPipe.io.dirWrite.s, reqPipe.io.dirWrite.s))
  directory.io.dirWrite.sf  <> fastPriorityArbDec(Seq(respPipe.io.dirWrite.sf, reqPipe.io.dirWrite.sf))
  directory.io.readMshr     <> mshrCtl.io.dirReadMshr
  directory.io.mshrResp     := mshrCtl.io.mshrResp2Dir


  mshrCtl.io.sliceId        := io.sliceId
  mshrCtl.io.req2Slice      <> io.req2Slice
  mshrCtl.io.resp2Slice     <> io.resp2Slice
  mshrCtl.io.udpMSHR        <> fastPriorityArbDec(Seq(respPipe.io.udpMSHR, reqPipe.io.udpMSHR))
  mshrCtl.io.updLockMSHR    <> fastPriorityArbDec(Seq(respPipe.io.updLockMSHR, reqPipe.io.updLockMSHR))

  reqPipe.io.sliceId        := io.sliceId
  reqPipe.io.dirResp        := directory.io.dirResp
  reqPipe.io.mshrResp       := mshrCtl.io.udpResp


  respPipe.io.sliceId       := io.sliceId
  respPipe.io.dirResp       := directory.io.dirResp
  respPipe.io.mshrResp      := mshrCtl.io.udpResp


  reqPipe.io.task.valid     := mshrCtl.io.pipeTask.valid & mshrCtl.io.pipeTask.bits.toReqPipe
  respPipe.io.task.valid    := mshrCtl.io.pipeTask.valid & mshrCtl.io.pipeTask.bits.toRespPipe
  reqPipe.io.task.bits      := mshrCtl.io.pipeTask.bits
  respPipe.io.task.bits     := mshrCtl.io.pipeTask.bits
  mshrCtl.io.pipeTask.ready := reqPipe.io.task.ready & mshrCtl.io.pipeTask.bits.toReqPipe |
                               respPipe.io.task.ready & mshrCtl.io.pipeTask.bits.toRespPipe


  mpReqQueue.io.enq         <> fastPriorityArbDec(Seq(respPipe.io.req2Node, reqPipe.io.req2Node))
  mpRespQueue.io.enq        <> fastPriorityArbDec(Seq(respPipe.io.resp2Node, reqPipe.io.resp2Node))
  io.req2Node               <> mpReqQueue.io.deq
  io.resp2Node              <> fastPriorityArbDec(Seq(mshrCtl.io.retry2Node, mpRespQueue.io.deq))
}