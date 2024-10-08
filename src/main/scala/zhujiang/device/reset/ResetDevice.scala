package zhujiang.device.reset

import chisel3._

class ResetDevice extends Module {
  val io = IO(new Bundle {
    val resetInject = Output(UInt(2.W))
    val resetState = Input(UInt(2.W))
    val onReset = Output(Bool())
  })
  private val resetReg = RegInit(3.U(2.W))
  io.resetInject := resetReg
  when(resetReg === 3.U) {
    resetReg := 1.U
  }.elsewhen(resetReg === 1.U && io.resetState === 1.U) {
    resetReg := 0.U
  }
  io.onReset := RegNext(io.resetState =/= 0.U)
}
