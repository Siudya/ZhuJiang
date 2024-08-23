package DONGJIANG.CHI

import chisel3._
import chisel3.util.{Decoupled, Queue, is, log2Ceil, switch}
import org.chipsalliance.cde.config._

class InboundFlitCtrl[T <: Bundle with HasChiOpcode](gen: T, lcrdMax: Int = 4, aggregateIO: Boolean = false)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val chi           = Flipped(CHIChannelIO(gen, aggregateIO))
    val linkState     = Input(UInt(LinkStates.width.W))
    val allLcrdRetrun = Output(Bool()) // Deactive Done
    val flit          = Decoupled(gen)
  })

  val lcrdMaxBits   = log2Ceil(lcrdMax + 1)

// --------------------- Modules declaration --------------------- //
  val queue = Module(new Queue(gen, entries = lcrdMax, pipe = true, flow = false, hasFlush = false))

// ------------------- Reg/Wire declaration ---------------------- //
  val lcrdSendNumReg  = RegInit(0.U(lcrdMaxBits.W))
  val lcrdFreeNum     = Wire(UInt(lcrdMaxBits.W))
  val lcrdv           = WireInit(false.B)
  val enq             = WireInit(0.U.asTypeOf(io.flit))
  dontTouch(lcrdFreeNum)

// --------------------- Logic ----------------------------------- //
  // Count lcrd
  lcrdSendNumReg      := lcrdSendNumReg + io.chi.lcrdv.asUInt - io.chi.flitv.asUInt
  lcrdFreeNum         := lcrdMax.U - queue.io.count - lcrdSendNumReg
  
  /*
   * FSM
   */
  switch(io.linkState) {
    is(LinkStates.STOP) {
      // Nothing to do
    }
    is(LinkStates.ACTIVATE) {
      // Nothing to do
    }
    is(LinkStates.RUN) {
      // Send lcrd
      lcrdv           := lcrdFreeNum > 0.U
      // Receive txReq
      enq.valid       := RegNext(io.chi.flitpend) & io.chi.flitv
      enq.bits        := io.chi.flit
    }
    is(LinkStates.DEACTIVATE) {
      // Nothing to do
    }
  }

  // allLcrdRetrun
  io.allLcrdRetrun := lcrdSendNumReg === 0.U

  /*
   * Connection
   */
  // lcrdv
  io.chi.lcrdv := lcrdv
  // enq
  queue.io.enq <> enq
  // deq
  // In CHI, opcode = 0 means LCrdReturn
  when(queue.io.deq.bits.opcode =/= 0.U) {
    io.flit <> queue.io.deq
  }.otherwise {
    io.flit.valid := false.B
    io.flit.bits := DontCare
    queue.io.deq.ready := true.B
  }



// --------------------- Assertion ------------------------------- //
  switch(io.linkState) {
    is(LinkStates.STOP) {
      assert(!io.chi.flitv, "When STOP, It cant send flit")
    }
    is(LinkStates.ACTIVATE) {
      assert(!io.chi.flitv, "When ACTIVATE, It cant send flit")
    }
    is(LinkStates.RUN) {
      assert(Mux(queue.io.enq.valid, queue.io.enq.ready, true.B), "When flitv is true, queue must be able to receive flit")
    }
    is(LinkStates.DEACTIVATE) {
      assert(!io.chi.lcrdv,  "When DEACTIVATE, It cant send lcrdv")
    }
  }

  assert(lcrdSendNumReg <= lcrdMax.U, "Lcrd be send cant over than lcrdMax")
  assert(queue.io.count <= lcrdMax.U, "queue.io.count cant over than lcrdMax")
  assert(lcrdFreeNum    <= lcrdMax.U, "lcrd free num cant over than lcrdMax")
}