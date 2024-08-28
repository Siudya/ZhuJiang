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
    val rnReq2Slice     = Flipped(Decoupled(new Req2SliceBundle()))
    val resp2RnNode     = Decoupled(new Resp2NodeBundle())
    val req2RnNode      = Decoupled(new Req2NodeBundle())
    val rnResp2Slice    = Flipped(Decoupled(new Resp2SliceBundle()))
    // slice ctrl signals: SnNode <> Slice
    val req2SnNode      = Decoupled(new Req2NodeBundle())
    val snResp2Slice    = Flipped(Decoupled(new Resp2SliceBundle()))
    // slice DataBuffer signals: RnNode <> Slice
    val rnDBSigs        = Flipped(new DBBundle(hasDBRCReq = true))
    val snDBSigs        = Flipped(new DBBundle(hasDBRCReq = true))
  })

// --------------------- Modules declaration ------------------------//
  val dataBuffer    = Module(new DataBuffer())
  val dataStorage   = Module(new DataStorageWrapper())
  val directory     = Module(new DirectoryWrapper())
  val mainPipe      = Module(new MainPipe())
  val mshrCtl       = Module(new MSHRCtl())
  val snpCtl        = Module(new SnoopCtlWrapper())
  val mpReqQueue    = Module(new Queue(gen = new Req2NodeBundle(), entries = djparam.nrMpReqQueue, pipe = true, flow = true))
  val mpRespQueue   = Module(new Queue(gen = new Resp2NodeBundle(),entries = djparam.nrMpRespQueue, pipe = true, flow = true))


// --------------------- Wire declaration ------------------------//
  val mpReq2RnNode  = WireInit(0.U.asTypeOf(Decoupled(new Req2NodeBundle())))
  val mpReq2SnNode  = WireInit(0.U.asTypeOf(Decoupled(new Req2NodeBundle())))


// --------------------------- Connection ---------------------------//
  dataBuffer.io.sliceId   := io.sliceId
  dataBuffer.io.rn2db     <> io.rnDBSigs
  dataBuffer.io.sn2db     <> io.snDBSigs
  dataBuffer.io.ds2db     <> dataStorage.io.ds2db
  dataBuffer.io.mpDBRCReq <> mainPipe.io.mpDBRCReq


  dataStorage.io.sliceId  := io.sliceId
  dataStorage.io.task     <> mainPipe.io.dsTask


  directory.io.sliceId    := io.sliceId
  directory.io.dirRead    <> mshrCtl.io.dirRead
  directory.io.dirResp    <> mainPipe.io.dirResp
  directory.io.dirWrite   <> mainPipe.io.dirWrite


  mshrCtl.io.sliceId      := io.sliceId
  mshrCtl.io.req2Slice    <> io.rnReq2Slice
  mshrCtl.io.resp2Slice   <> fastArbDec(Seq(io.rnResp2Slice, io.snResp2Slice))
  mshrCtl.io.mpTask       <> mainPipe.io.mpTask
  mshrCtl.io.udpMSHR      <> mainPipe.io.udpMSHR


  snpCtl.io.sliceId       := io.sliceId
  snpCtl.io.snpTask       <> mainPipe.io.snpTask


  mainPipe.io.sliceId     := io.sliceId
  mainPipe.io.req2Node    <> mpReqQueue.io.enq
  mainPipe.io.resp2Node   <> mpRespQueue.io.enq


  when(mpReqQueue.io.deq.bits.to.isSNMAS) {
    mpReq2SnNode.valid      := mpReqQueue.io.deq.valid
    mpReqQueue.io.deq.ready := mpReq2SnNode.ready
  }.otherwise {
    mpReq2RnNode.valid      := mpReqQueue.io.deq.valid
    mpReqQueue.io.deq.ready := mpReq2RnNode.ready
  }
  mpReq2RnNode.bits       := mpReqQueue.io.deq.bits
  mpReq2SnNode.bits       := mpReqQueue.io.deq.bits


  io.resp2RnNode          <> fastPriorityArbDec(Seq(mshrCtl.io.retry2RnNode, mpRespQueue.io.deq))
  io.req2RnNode           <> fastPriorityArbDec(Seq(snpCtl.io.req2RnNode, mpReq2RnNode))
  io.req2SnNode           <> mpReq2SnNode

}