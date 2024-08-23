package DONGJIANG

import DONGJIANG._
import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils._
import Utils.FastArb._
import Utils.IDConnector._

class ReqBufSelector(param: InterfaceParam)(implicit p: Parameters) extends DJModule {
  val io = IO(new Bundle() {
    val idle = Input(Vec(param.nrReqBuf, Bool()))
    val idleNum = Output(UInt((param.reqBufIdBits+1).W))
    val out0 = UInt(param.reqBufIdBits.W)
    val out1 = UInt(param.reqBufIdBits.W)
  })
  io.idleNum := PopCount(io.idle)
  io.out0 := PriorityEncoder(io.idle)
  val idle1 = WireInit(io.idle)
  idle1(io.out0) := false.B
  io.out1 := PriorityEncoder(idle1)
}

abstract class NodeBase(isSlv:Boolean, hasFree: Boolean = false, hasReq2Slice: Boolean = false, hasDBRCReq: Boolean = false)(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val freeOpt       = if(hasFree) Some(Output(Bool())) else None
    // CHI
    val chiOpt        = if(isSlv) Some(Flipped(CHIBundleDecoupled(chiParams))) else Some(CHIBundleDecoupled(chiParams))
    // slice ctrl signals
    val req2SliceOpt  = if(hasReq2Slice) Some(Decoupled(new Req2SliceBundle())) else None
    val resp2NodeOpt  = if(hasReq2Slice) Some(Flipped(Decoupled(new Resp2NodeBundle()))) else None
    val req2Node      = Flipped(Decoupled(new Req2NodeBundle()))
    val resp2Slice    = Decoupled(new Resp2SliceBundle())
    // slice DataBuffer signals
    val dbSigs        = new DBBundle(hasDBRCReq)

    def free          = freeOpt.get
    def chi           = chiOpt.get
    def req2Slice     = req2SliceOpt.get
    def resp2Node     = resp2NodeOpt.get
  })
}