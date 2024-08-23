package DONGJIANG.CHI

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class InboundLinkCtrl()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val chiLinkCtrl     = Flipped(new CHILinkCtrlIO())
    val txState         = Output(UInt(LinkStates.width.W))
    val rxState         = Output(UInt(LinkStates.width.W))
    val rxRun           = Input(Bool())
    val txAllLcrdRetrun = Input(Bool())
  })
  // DontCare txsactive and rxsactive
  io.chiLinkCtrl.rxsactive := true.B

  val txState = LinkStates.getLinkState(io.chiLinkCtrl.txactivereq, io.chiLinkCtrl.txactiveack)
  val rxState = LinkStates.getLinkState(io.chiLinkCtrl.rxactivereq, io.chiLinkCtrl.rxactiveack)

  val txStateReg = RegInit(LinkStates.STOP)
  val rxStateReg = RegInit(LinkStates.STOP)

  val txactiveackReg = RegInit(false.B)
  val rxactivereqReg = RegInit(false.B)

  txStateReg := txState
  rxStateReg := rxState

  /*
   * txState FSM ctrl by io.chiLinkCtrl.txactiveack
   */
  switch(txStateReg) {
    is(LinkStates.STOP) {
      txactiveackReg := false.B
    }
    is(LinkStates.ACTIVATE) {
      txactiveackReg := true.B
    }
    is(LinkStates.RUN) {
      txactiveackReg := true.B
    }
    is(LinkStates.DEACTIVATE) {
      txactiveackReg := !io.txAllLcrdRetrun
    }
  }


  /*
   * rxState FSM ctrl by io.chiLinkCtrl.rxactivereq
   */
  switch(rxStateReg) {
    is(LinkStates.STOP) {
      rxactivereqReg := io.rxRun
    }
    is(LinkStates.ACTIVATE) {
      rxactivereqReg := true.B
    }
    is(LinkStates.RUN) {
      rxactivereqReg := Mux(io.rxRun, true.B, !(txStateReg === LinkStates.DEACTIVATE | txStateReg === LinkStates.STOP))
    }
    is(LinkStates.DEACTIVATE) {
      rxactivereqReg := false.B
    }
  }

  io.txState := txState
  io.rxState := rxState

  io.chiLinkCtrl.txactiveack := txactiveackReg
  io.chiLinkCtrl.rxactivereq := rxactivereqReg

}