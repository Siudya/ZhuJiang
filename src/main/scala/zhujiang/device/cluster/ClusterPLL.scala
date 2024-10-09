package zhujiang.device.cluster

import xs.utils.ClockManagerWrapper
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.device.peripheral.BaseTLULPeripheral
import zhujiang.tilelink.TilelinkParams

class ClusterPLL(tlParams: TilelinkParams)(implicit p: Parameters) extends BaseTLULPeripheral(tlParams) {
  val io = IO(new Bundle {
    val inClock = Input(Clock())
    val cpuClock = Output(Clock())
    val timerClock = Output(Clock())
  })
  private val cfgReg = RegInit(VecInit(Seq.fill(8)(0.U(64.W))))
  val regSeq = Seq.tabulate(8)(i =>
    (s"cfg_$i", cfgReg(i), cfgReg(i), i * 8, None, None)
  )
  private val cm = Module(new ClockManagerWrapper)
  myclock := cm.io.cpu_clock
  for(i <- cm.io.cfg.indices) cm.io.cfg(i) := cfgReg(i)
  io.cpuClock := cm.io.cpu_clock
  io.timerClock := cm.io.timer_clock
  cm.io.in_clock := io.inClock
  private val dummy = genWriteMap()
}
