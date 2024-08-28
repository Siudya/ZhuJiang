package DONGJIANG.SLICE

import DONGJIANG._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.sram.SRAMTemplate

class DataStorageWrapper()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val sliceId   = Input(UInt(bankBits.W))
    // Req From MainPipe
    val task      = Flipped(Decoupled(new DSTaskBundle()))
    // dataBuffer <-> DataStorage
    val ds2db     = new DBBundle(hasDBRCReq = true) // TODO: Consider data width: 256 -> 512
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare

}