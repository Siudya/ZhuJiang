package DONGJIANG.SLICE

import DONGJIANG._
import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class MainPipe()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val sliceId     = Input(UInt(bankBits.W))
    // Req To DataBuffer
    val mpDBRCReq   = Decoupled(new DBRCReq())
    // Task To DataStorage
    val dsTask      = Decoupled(new DSTaskBundle())
    // Resp From Directory
    val dirResp     = Flipped(Decoupled(new DirRespBundle()))
    // Write Req To Directory
    val dirWrite    = new DirWriteBundle()
    // Task From MSHR
    val mpTask      = Flipped(Decoupled(new MpTaskBundle()))
    // Update Task To MSHR
    val udpMSHR     = Decoupled(new UpdateMSHRBundle())
    // Task To SnpCtl
    val snpTask     = Decoupled(new SnpTaskBundle())
    // Req To Node
    val req2Node    = Decoupled(new Req2NodeBundle())
    // Resp To Node
    val resp2Node   = Decoupled(new Resp2NodeBundle())
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare
}