package DONGJIANG.SLICE

import DONGJIANG._
import _root_.DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.ReplacementPolicy
import xs.utils.ParallelPriorityMux
import xs.utils.sram.SRAMTemplate
import xs.utils.perf.{DebugOptions, DebugOptionsKey}

class DirectoryWrapper()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
val io = IO(new Bundle {
  val sliceId     = Input(UInt(bankBits.W))
  val dirRead     = Flipped(Decoupled(new DirReadBundle()))
  val dirResp     = Decoupled(new DirRespBundle())
  val dirWrite    = Flipped(new DirWriteBundle())
})

  // TODO: Delete the following code when the coding is complete
  io <> DontCare
}