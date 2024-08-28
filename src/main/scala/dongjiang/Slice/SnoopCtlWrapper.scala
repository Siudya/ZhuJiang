package DONGJIANG.SLICE

import DONGJIANG._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.FastArb._

class SnoopCtlWrapper()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val sliceId       = Input(UInt(bankBits.W))
    // MainPipe -> SnpCtrl
    val snpTask       = Flipped(Decoupled(new SnpTaskBundle()))
    // snpCtrl -> RnNode
    val req2RnNode    = Decoupled(new Req2NodeBundle())
  })

  // TODO: Delete the following code when the coding is complete
  io <> DontCare
}