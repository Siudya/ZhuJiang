package zhujiang.device.uncore

import chisel3._
import freechips.rocketchip.devices.debug.{DebugCustomXbar, DebugIO, DebugModuleKey, DebugModuleParams, DebugTransportModuleJTAG, JtagDTMKeyDefault, ResetCtrlIO, SystemJTAGIO, TLDebugModule}
import freechips.rocketchip.diplomacy.{LazyModule, LazyRawModuleImp}
import org.chipsalliance.cde.config.Parameters

class DebugModule(numCores: Int)(implicit p: Parameters) extends LazyModule {
  val debug = LazyModule(new TLDebugModule(8))
  private val debugCustomXbarOpt = p(DebugModuleKey).map(params => LazyModule(new DebugCustomXbar(outputRequiresInput = false)))
  debug.dmInner.dmInner.customNode := debugCustomXbarOpt.get.node
  lazy val module = new DebugModuleImp(this, numCores)
}

class DebugModuleIO(numCores: Int)(implicit p: Parameters) extends Bundle {
  val resetCtrl = new ResetCtrlIO(numCores)(p)
  val debugIO = new DebugIO()(p)
  val clock = Input(Bool())
  val reset = Input(Reset())
}

class DebugModuleImp(outer: DebugModule, numCores: Int) extends LazyRawModuleImp(outer) {
  val io = IO(new DebugModuleIO(numCores))
  private val debug = outer.debug.module
  debug.io.tl_reset := io.reset // this should be TL reset
  debug.io.tl_clock := io.clock.asClock // this should be TL clock
  withClock(io.clock.asClock) {
    debug.io.hartIsInReset := RegNext(io.resetCtrl.hartIsInReset)
  }
  io.resetCtrl.hartResetReq.foreach { rcio => debug.io.hartResetReq.foreach { rcdm => rcio := rcdm } }

  io.debugIO.clockeddmi.foreach { dbg => debug.io.dmi.get <> dbg } // not connected in current case since we use dtm
  debug.io.debug_reset := io.debugIO.reset
  debug.io.debug_clock := io.debugIO.clock
  io.debugIO.ndreset := debug.io.ctrl.ndreset
  io.debugIO.dmactive := debug.io.ctrl.dmactive
  debug.io.ctrl.dmactiveAck := io.debugIO.dmactiveAck
  io.debugIO.extTrigger.foreach { x => debug.io.extTrigger.foreach { y => x <> y } }
  debug.io.ctrl.debugUnavail.foreach {
    _ := false.B
  }

  private val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleKey).get.nDMIAddrSize, new JtagDTMKeyDefault))
  dtm.io.jtag <> io.debugIO.systemjtag.get.jtag
  io.debugIO.disableDebug.foreach { x => dtm.io.jtag.TMS := io.debugIO.systemjtag.get.jtag.TMS | x } // force TMS high when debug is disabled
  dtm.io.jtag_clock := io.debugIO.systemjtag.get.jtag.TCK
  dtm.io.jtag_reset := io.debugIO.systemjtag.get.reset
  dtm.io.jtag_mfr_id := io.debugIO.systemjtag.get.mfr_id
  dtm.io.jtag_part_number := io.debugIO.systemjtag.get.part_number
  dtm.io.jtag_version := io.debugIO.systemjtag.get.version
  dtm.rf_reset := io.debugIO.systemjtag.get.reset
  debug.io.dmi.get.dmi <> dtm.io.dmi
  debug.io.dmi.get.dmiClock := io.debugIO.systemjtag.get.jtag.TCK
  debug.io.dmi.get.dmiReset := io.debugIO.systemjtag.get.reset
}

object ZJDebugModuleParams {
  val debugParams = DebugModuleParams(
    nAbstractDataWords = 2,
    maxSupportedSBAccess = 64,
    hasBusMaster = true,
    baseAddress = BigInt(0x38020000),
    nScratch = 2,
    crossingHasSafeReset = false
  )
}
