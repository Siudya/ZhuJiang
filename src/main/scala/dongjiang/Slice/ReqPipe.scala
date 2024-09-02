package DONGJIANG.SLICE

import DONGJIANG._
import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class ReqPipe()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val sliceId     = Input(UInt(bankBits.W))
    // Req To DataBuffer
    val mpDBRCReq   = Decoupled(new DBRCReq())
    // Resp From Directory
    val dirResp     = Flipped(Valid(new DirRespBundle()))
    // Write Req To Directory
    val dirWrite    = new DirWriteBundle()
    // Task From MSHR
    val task        = Flipped(Decoupled(new PipeTaskBundle()))
    // Update Task To MSHR
    val udpMSHR     = Decoupled(new UpdateMSHRReqBundle())
    val mshrResp    = Flipped(Valid(new UpdateMSHRRespBundle()))
    val updLockVec  = Decoupled(new MSHRSetBundle)
    // Req To Node
    val req2Node    = Decoupled(new Req2NodeBundle())
    // Resp To Node
    val resp2Node   = Decoupled(new Resp2NodeBundle())
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare
}