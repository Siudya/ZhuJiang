package zhujiang.device.uncore

import chisel3._
import chisel3.util._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import zhujiang.ZJParametersKey

class TLDeviceBlockIO(coreNum: Int, extIntrNum: Int)(implicit p: Parameters) extends Bundle {
  val extIntr = Input(UInt(extIntrNum.W))
  val msip = Output(UInt(coreNum.W))
  val mtip = Output(UInt(coreNum.W))
  val meip = Output(UInt(coreNum.W))
  val seip = Output(UInt(coreNum.W))
  val dbip = Output(UInt(coreNum.W))
  val timerTick = Input(Bool())
  val resetCtrl = new ResetCtrlIO(coreNum)(p)
  val debug = new DebugIO()(p)
}

class TLDeviceBlock(coreNum: Int, extIntrNum: Int, idBits: Int, cfgDataBits: Int, sbaDataBits: Int)(implicit p: Parameters) extends LazyModule with BindingScope {
  private val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "riscv-device-block",
      sourceId = IdRange(0, 1 << idBits),
      supportsProbe = TransferSizes(1, cfgDataBits / 8),
      supportsGet = TransferSizes(1, cfgDataBits / 8),
      supportsPutFull = TransferSizes(1, cfgDataBits / 8),
      supportsPutPartial = TransferSizes(1, cfgDataBits / 8)
    ))
  )
  private val clientNode = TLClientNode(Seq(clientParameters))

  private val sbaParameters = TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0L, (0x1L << p(ZJParametersKey).requestAddrBits) - 1)),
      supportsGet = TransferSizes(1, sbaDataBits / 8),
      supportsPutFull = TransferSizes(1, sbaDataBits / 8),
      supportsPutPartial = TransferSizes(1, sbaDataBits / 8),
    )),
    beatBytes = sbaDataBits / 8
  )
  private val sbaNode = TLManagerNode(Seq(sbaParameters))

  private val xbar = LazyModule(new TLXbar)
  private val plic = LazyModule(new TLPLIC(PLICParams(baseAddress = 0x3c000000L), 8))
  private val clint = LazyModule(new CLINT(CLINTParams(0x38000000L), 8))
  private val debug = LazyModule(new DebugModule(coreNum)(p.alterPartial({
    case DebugModuleKey => Some(ZJDebugModuleParams.debugParams)
    case MaxHartIdBits => log2Ceil(coreNum)
    case ExportDebug => DebugAttachParams(protocols = Set(JTAG))
    case JtagDTMKey => JtagDTMKey
  })))

  private val intSourceNode = IntSourceNode(IntSourcePortSimple(extIntrNum, ports = 1, sources = 1))
  private val clintIntSink = IntSinkNode(IntSinkPortSimple(coreNum, 2))
  private val debugIntSink = IntSinkNode(IntSinkPortSimple(coreNum, 1))
  private val plicIntSink = IntSinkNode(IntSinkPortSimple(2 * coreNum, 1))

  xbar.node :=* clientNode
  plic.node :*= xbar.node
  clint.node :*= xbar.node
  debug.debug.node :*= xbar.node
  plic.intnode := intSourceNode

  clintIntSink :*= clint.intnode
  debugIntSink :*= debug.debug.dmOuter.dmOuter.intnode
  plicIntSink :*= plic.intnode

  sbaNode :=* TLBuffer() :=* TLWidthWidget(1) :=* debug.debug.dmInner.dmInner.sb2tlOpt.get.node

  lazy val module = new Impl

  class Impl extends LazyModuleImp(this) {
    val tlm = clientNode.makeIOs()
    val sba = sbaNode.makeIOs()
    val io = IO(new TLDeviceBlockIO(coreNum, extIntrNum)(p.alterPartial({
      case DebugModuleKey => Some(ZJDebugModuleParams.debugParams)
      case MaxHartIdBits => log2Ceil(coreNum)
      case ExportDebug => DebugAttachParams(protocols = Set(JTAG))
      case JtagDTMKey => JtagDTMKey
    })))

    require(intSourceNode.out.head._1.length == io.extIntr.getWidth)
    for(idx <- 0 until extIntrNum) {
      val intrSyncReg = RegInit(0.U(3.W))
      intrSyncReg := Cat(io.extIntr(idx), intrSyncReg)(intrSyncReg.getWidth, 1)
      intSourceNode.out.head._1(idx) := intrSyncReg(0)
      intrSyncReg.suggestName(s"intrSyncReg${idx}")
    }
    clint.module.io.rtcTick := io.timerTick
    private val meip = Wire(Vec(coreNum, Bool()))
    private val seip = Wire(Vec(coreNum, Bool()))
    private val msip = Wire(Vec(coreNum, Bool()))
    private val mtip = Wire(Vec(coreNum, Bool()))
    private val dbip = Wire(Vec(coreNum, Bool()))
    io.meip := meip.asUInt
    io.seip := seip.asUInt
    io.msip := msip.asUInt
    io.mtip := mtip.asUInt
    io.dbip := dbip.asUInt
    for(idx <- 0 until coreNum) {
      meip(idx) := plicIntSink.in.map(_._1)(2 * idx).head
      seip(idx) := plicIntSink.in.map(_._1)(2 * idx + 1).head
      msip(idx) := clintIntSink.in.map(_._1)(idx)(0)
      mtip(idx) := clintIntSink.in.map(_._1)(idx)(1)
      dbip(idx) := debugIntSink.in.map(_._1)(idx).head
    }
    debug.module.io.clock := clock.asBool
    debug.module.io.reset := reset
    debug.module.io.resetCtrl <> io.resetCtrl
    debug.module.io.debugIO <> io.debug
    debug.module.io.debugIO.clock := clock
    debug.module.io.debugIO.reset := reset
  }
}
