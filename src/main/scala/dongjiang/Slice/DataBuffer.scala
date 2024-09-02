package DONGJIANG.SLICE

import DONGJIANG._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.Encoder.RREncoder

class DataBuffer()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val sliceId   = Input(UInt(bankBits.W))
    // NODE <-> DataBuffer
    val node2db   = Flipped(new DBBundle(hasDBRCReq = true))
    // MainPipe -> DataBuffer
    val mpDBRCReq = Flipped(Decoupled(new DBRCReq()))
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare
}