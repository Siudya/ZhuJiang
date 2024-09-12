package DONGJIANG.CPU

import DONGJIANG._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils._
import Utils.FastArb._

class SliceWrapper()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val valid           = Input(Bool())
    val sliceId         = Input(UInt(bankBits.W))
    // slice ctrl signals: RnNode <> Slice
    val req2Slice       = Flipped(Decoupled(new Req2SliceBundle()))
    val resp2Node       = Decoupled(new Resp2NodeBundle())
    val req2Node        = Decoupled(new Req2NodeBundle())
    val resp2Slice      = Flipped(Decoupled(new Resp2SliceBundle()))
    // Req To DataBuffer
    val dbRCReq         = Decoupled(new DBRCReq())
  })

// --------------------- Modules declaration ------------------------//
  val directory     = Module(new DirectoryWrapper())
  val reqPipe       = Module(new ProcessPipe())
  val respPipe      = Module(new ProcessPipe())
  val mshrCtl       = Module(new MSHRCtl())
  val mpReqQueue    = Module(new Queue(gen = new Req2NodeBundle(), entries = djparam.nrMpReqQueue, pipe = true, flow = true))
  val mpRespQueue   = Module(new Queue(gen = new Resp2NodeBundle(),entries = djparam.nrMpRespQueue, pipe = true, flow = true))

// --------------------------- Connection ---------------------------//
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
  respPipe.io.sliceId       := io.sliceId

  object connectPipe {
    def apply[T <: Bundle with HasPipeID](in: DecoupledIO[T], out: Seq[DecoupledIO[T]]): Unit = {
      out.head.valid          := in.valid & in.bits.toReqPipe
      out.last.valid          := in.valid & in.bits.toRespPipe
      out.foreach(_.bits      := in.bits)
      in.ready                := out.head.ready & in.bits.toReqPipe | out.last.ready & in.bits.toRespPipe
    }
    def apply[T <: Bundle with HasPipeID](in: ValidIO[T], out: Seq[ValidIO[T]]): Unit = {
      out.head.valid          := in.valid & in.bits.toReqPipe
      out.last.valid          := in.valid & in.bits.toRespPipe
      out.foreach(_.bits      := in.bits)
    }
  }

  connectPipe(mshrCtl.io.pipeTask,  Seq(reqPipe.io.task, respPipe.io.task))
  connectPipe(mshrCtl.io.udpResp,   Seq(reqPipe.io.mshrResp, respPipe.io.mshrResp))
  connectPipe(directory.io.dirResp, Seq(reqPipe.io.dirResp, respPipe.io.dirResp))

  mpReqQueue.io.enq         <> fastPriorityArbDec(Seq(respPipe.io.req2Node, reqPipe.io.req2Node))
  mpRespQueue.io.enq        <> fastPriorityArbDec(Seq(respPipe.io.resp2Node, reqPipe.io.resp2Node))
  io.req2Node               <> mpReqQueue.io.deq
  io.resp2Node              <> fastPriorityArbDec(Seq(mshrCtl.io.retry2Node, mpRespQueue.io.deq))
  io.dbRCReq                <> fastPriorityArbDec(Seq(respPipe.io.dbRCReq, reqPipe.io.dbRCReq))
}