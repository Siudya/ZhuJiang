package zhujiang.device.uncore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.AXI4Xbar
import freechips.rocketchip.devices.debug.{DebugAttachParams, DebugIO, DebugModuleKey, ExportDebug, JTAG, JtagDTMKey, ResetCtrlIO}
import freechips.rocketchip.devices.tilelink.{CLINT, CLINTParams, PLICParams, TLPLIC}
import freechips.rocketchip.diplomacy.RegionType.TRACKED
import zhujiang.axi._
import zhujiang.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple, IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import zhujiang.{HasZJParams, ZJModule}

class UncoreComplex(coreNum: Int, extIntrNum: Int, axiCfgParams: AxiParams, axiDmaParams: AxiParams)(implicit p: Parameters) extends ZJModule {
  private val cfgXBar = Module(new AxiCfgXBar(axiCfgParams))
  private val dmaXBar = Module(new AxiDmaXBar(Seq(axiDmaParams, axiDmaParams)))
  private val axi2tl = Module(new AxiLite2TLUL(cfgXBar.io.downstream.head.params))
  private val tl2axi = Module(new TLUL2AxiLite(dmaXBar.io.upstream.head.params))
  private val lpb = LazyModule(new TLDeviceBlock(
    coreNum,
    extIntrNum,
    axi2tl.io.tl.params.sourceBits,
    axi2tl.io.tl.params.dataBits,
    axiDmaParams.dataBits
  )(p.alterPartial {
    case MonitorsEnabled => false
  }))
  private val pb = Module(lpb.module)


  val io = IO(new Bundle {
    val icn = new Bundle {
      val cfg = Flipped(new AxiBundle(axiCfgParams))
      val dma = new AxiBundle(dmaXBar.io.upstream.last.params)
    }
    val ext = new Bundle {
      val cfg = new AxiBundle(cfgXBar.io.downstream.last.params)
      val dma = Flipped(new AxiBundle(axiDmaParams))
    }
    val chip = Input(UInt(nodeAidBits.W))
    val debug = pb.io.cloneType
  })

  dontTouch(io)

  cfgXBar.misc.chip := io.chip
  cfgXBar.io.upstream.head <> io.icn.cfg
  axi2tl.io.axi <> cfgXBar.io.downstream.head
  io.ext.cfg <> cfgXBar.io.downstream.last

  dmaXBar.io.upstream.head <> tl2axi.io.axi
  dmaXBar.io.upstream.last <> io.ext.dma
  io.icn.dma <> dmaXBar.io.downstream.head

  pb.io <> io.debug
  pb.tlm.foreach(tlm => {
    tlm.a.valid := axi2tl.io.tl.a.valid
    tlm.a.bits.opcode := axi2tl.io.tl.a.bits.opcode
    tlm.a.bits.param := axi2tl.io.tl.a.bits.param
    tlm.a.bits.size := axi2tl.io.tl.a.bits.size
    tlm.a.bits.source := axi2tl.io.tl.a.bits.source
    tlm.a.bits.address := axi2tl.io.tl.a.bits.address
    tlm.a.bits.mask := axi2tl.io.tl.a.bits.mask
    tlm.a.bits.data := axi2tl.io.tl.a.bits.data
    tlm.a.bits.corrupt := axi2tl.io.tl.a.bits.corrupt
    axi2tl.io.tl.a.ready := tlm.a.ready

    axi2tl.io.tl.d.valid := tlm.d.valid
    axi2tl.io.tl.d.bits.opcode := tlm.d.bits.opcode
    axi2tl.io.tl.d.bits.param := tlm.d.bits.param
    axi2tl.io.tl.d.bits.size := tlm.d.bits.size
    axi2tl.io.tl.d.bits.source := tlm.d.bits.source
    axi2tl.io.tl.d.bits.sink := tlm.d.bits.sink
    axi2tl.io.tl.d.bits.denied := tlm.d.bits.denied
    axi2tl.io.tl.d.bits.data := tlm.d.bits.data
    axi2tl.io.tl.d.bits.corrupt := tlm.d.bits.corrupt
    tlm.d.ready := axi2tl.io.tl.d.ready
  })

  pb.sba.foreach(sba => {
    tl2axi.io.tl.a.valid := sba.a.valid
    tl2axi.io.tl.a.bits.opcode := sba.a.bits.opcode
    tl2axi.io.tl.a.bits.param := sba.a.bits.param
    tl2axi.io.tl.a.bits.size := sba.a.bits.size
    tl2axi.io.tl.a.bits.source := sba.a.bits.source
    tl2axi.io.tl.a.bits.address := sba.a.bits.address
    tl2axi.io.tl.a.bits.mask := sba.a.bits.mask
    tl2axi.io.tl.a.bits.data := sba.a.bits.data
    tl2axi.io.tl.a.bits.corrupt := sba.a.bits.corrupt
    sba.a.ready := tl2axi.io.tl.a.ready

    sba.d.valid := tl2axi.io.tl.d.valid
    sba.d.bits.opcode := tl2axi.io.tl.d.bits.opcode
    sba.d.bits.param := tl2axi.io.tl.d.bits.param
    sba.d.bits.size := tl2axi.io.tl.d.bits.size
    sba.d.bits.source := tl2axi.io.tl.d.bits.source
    sba.d.bits.sink := tl2axi.io.tl.d.bits.sink
    sba.d.bits.denied := tl2axi.io.tl.d.bits.denied
    sba.d.bits.data := tl2axi.io.tl.d.bits.data
    sba.d.bits.corrupt := tl2axi.io.tl.d.bits.corrupt
    tl2axi.io.tl.d.ready := sba.d.ready
  })
}

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
      address = Seq(AddressSet(0L, 0xFFFF_FFFF_FFFFL)),
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
      intrSyncReg := Cat(io.extIntr(idx), intrSyncReg)(2, 0)
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