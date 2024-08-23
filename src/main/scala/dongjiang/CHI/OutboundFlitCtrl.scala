package DONGJIANG.CHI

import chisel3._
import chisel3.util.{Decoupled, is, log2Ceil, switch}
import org.chipsalliance.cde.config._

class OutboundFlitCtrl[T <: Bundle with HasChiOpcode](gen: T, lcrdMax: Int = 4, aggregateIO: Boolean = false)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val chi       = CHIChannelIO(gen, aggregateIO)
    val linkState = Input(UInt(LinkStates.width.W))
    val flit      = Flipped(Decoupled(gen))
  })

  val lcrdMaxBits   = log2Ceil(lcrdMax + 1)

// ------------------- Reg/Wire declaration ---------------------- //
  // Count lcrd
  val lcrdFreeNumReg  = RegInit(0.U(lcrdMaxBits.W))
  val flitReg         = RegInit(0.U.asTypeOf(gen))
  val flitvReg        = RegInit(false.B)
  val flit            = WireInit(0.U.asTypeOf(gen))
  val flitv           = WireInit(false.B)
  val flitInReady     = WireInit(false.B)


// ------------------------- Logic ------------------------------- //
  /*
   * Receive task and data
   */
  when(io.linkState =/= LinkStates.DEACTIVATE) {
    flitv := io.flit.fire
    flit  := io.flit.bits
  }.otherwise {
    flitv := lcrdFreeNumReg > 0.U
    flit  := 0.U.asTypeOf(flit)
  }

  /*
   * set reg value
   */
  flitvReg := flitv
  flitReg  := Mux(flitv, flit, flitReg)

  /*
   * FSM: count free lcrd and set task ready value
   */
  switch(io.linkState) {
    is(LinkStates.STOP) {
      // Nothing to do
    }
    is(LinkStates.ACTIVATE) {
      lcrdFreeNumReg  := lcrdFreeNumReg + io.chi.lcrdv.asUInt
    }
    is(LinkStates.RUN) {
      lcrdFreeNumReg  := lcrdFreeNumReg + io.chi.lcrdv.asUInt - flitv
      flitInReady     := lcrdFreeNumReg > 0.U
    }
    is(LinkStates.DEACTIVATE) {
      lcrdFreeNumReg  := lcrdFreeNumReg + io.chi.lcrdv.asUInt - flitv
      flitInReady     := false.B
    }
  }
  io.flit.ready   := flitInReady

  /*
   * Output chi flit
   */
  io.chi.flitpend := flitv
  io.chi.flitv    := flitvReg
  io.chi.flit     := flitReg


// ------------------------- Assert ------------------------------- //
  switch(io.linkState) {
    is(LinkStates.STOP) {
      assert(!io.chi.flitv, "When STOP, It cant send flit")
      assert(!io.chi.lcrdv,  "When DEACTIVATE, It cant receive lcrdv")
    }
    is(LinkStates.ACTIVATE) {
      assert(!io.chi.flitv, "When ACTIVATE, It cant send flit")
    }
    is(LinkStates.RUN) {
      assert(Mux(lcrdFreeNumReg >= lcrdMax.U, !io.chi.lcrdv | flitv, true.B), "Lcrd overflow")
    }
    is(LinkStates.DEACTIVATE) {
      assert(!io.flit.fire, "When DEACTIVATE, It cant receive flit")
    }
  }

  assert(lcrdFreeNumReg <= lcrdMax.U, "Lcrd be send cant over than lcrdMax")

}