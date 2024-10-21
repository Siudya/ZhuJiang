package zhujiang.device.uncore

import chisel3._
import chisel3.util.Cat
import zhujiang.axi._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import xs.utils.ResetGen
import zhujiang.device.async.{DeviceIcnAsyncBundle, DeviceSideAsyncModule}
import zhujiang.device.bridge.axilite.AxiLiteBridge
import zhujiang.device.dma.Axi2Chi
import zhujiang.ZJModule
import zhujiang.device.cluster.interconnect.DftWires

class ShiftSync[T <: Data](gen:T, sync:Int = 3) extends Module {
  val io = IO(new Bundle{
    val in = Input(gen)
    val out = Output(gen)
  })
  private val syncRegSeq = Seq.fill(sync)(Reg(gen))
  (io.out +: syncRegSeq :+ io.in).reduce((a:T, b:T) => {
    a := b
    a
  })
}

object ShiftSync {
  def apply[T <: Data](in:T, syncStage:Int = 3): T = {
    val sync = Module(new ShiftSync(chiselTypeOf(in)))
    sync.io.in := in
    sync.io.out
  }
}

class UncoreComplex(cfgNode: Node, dmaNode: Node)(implicit p: Parameters) extends ZJModule {
  private val coreNum = zjParams.localRing.filter(_.nodeType == NodeType.CC).map(_.cpuNum).sum
  private val extIntrNum = zjParams.externalInterruptNum
  private val cfgAsyncModule = Module(new DeviceSideAsyncModule(cfgNode))
  private val dmaAsyncModule = Module(new DeviceSideAsyncModule(dmaNode))
  private val cfgBridge = Module(new AxiLiteBridge(cfgNode, 64, 3))
  private val dmaBridge = Module(new Axi2Chi(dmaNode))
  private val resetGen = Module(new ResetGen)
  val dft = IO(Input(new DftWires))
  resetGen.clock := clock
  resetGen.reset := cfgAsyncModule.io.async.resetRx
  resetGen.dft := dft.reset

  private val cfgParams = cfgBridge.axi.params
  private val dmaParams = dmaBridge.axi.params
  private val extDmaParams = dmaParams.copy(idBits = dmaParams.idBits - 1)

  private val cfgXBar = Module(new AxiCfgXBar(cfgParams))
  private val dmaXBar = Module(new AxiDmaXBar(Seq(extDmaParams, extDmaParams)))
  private val axi2tl = Module(new AxiLite2TLUL(cfgXBar.io.downstream.head.params))
  private val tl2axi = Module(new TLUL2AxiLite(dmaXBar.io.upstream.head.params))

  private val tlDevBlock = LazyModule(new TLDeviceBlock(
    coreNum,
    extIntrNum,
    axi2tl.io.tl.params.sourceBits,
    axi2tl.io.tl.params.dataBits,
    extDmaParams.dataBits
  )(p.alterPartial {
    case MonitorsEnabled => false
  }))
  private val pb = Module(tlDevBlock.module)

  cfgAsyncModule.reset := resetGen.o_reset
  dmaAsyncModule.reset := resetGen.o_reset
  cfgBridge.reset := resetGen.o_reset
  dmaBridge.reset := resetGen.o_reset
  cfgXBar.reset := resetGen.o_reset
  dmaXBar.reset := resetGen.o_reset
  axi2tl.reset := resetGen.o_reset
  tl2axi.reset := resetGen.o_reset
  pb.reset := resetGen.o_reset

  val io = IO(new Bundle {
    val async = new Bundle {
      val cfg = new DeviceIcnAsyncBundle(cfgNode)
      val dma = new DeviceIcnAsyncBundle(dmaNode)
    }
    val ext = new Bundle {
      val cfg = new AxiBundle(cfgXBar.io.downstream.last.params)
      val dma = Flipped(new AxiBundle(dmaXBar.io.upstream.last.params))
      val intr = Input(UInt(extIntrNum.W))
      val timerTick = Input(Bool())
    }
    val cpu = new Bundle {
      val msip = Output(UInt(coreNum.W))
      val mtip = Output(UInt(coreNum.W))
      val meip = Output(UInt(coreNum.W))
      val seip = Output(UInt(coreNum.W))
      val dbip = Output(UInt(coreNum.W))
    }
    val chip = Input(UInt(nodeAidBits.W))
    val debug = pb.io.debug.cloneType
    val resetCtrl = pb.io.resetCtrl.cloneType
  })

  dontTouch(io)

  cfgXBar.misc.chip := io.chip
  cfgAsyncModule.io.async <> io.async.cfg
  cfgBridge.icn <> cfgAsyncModule.io.icn
  cfgXBar.io.upstream.head <> cfgBridge.axi
  axi2tl.io.axi <> cfgXBar.io.downstream.head
  io.ext.cfg <> cfgXBar.io.downstream.last

  dmaXBar.io.upstream.head <> tl2axi.io.axi
  dmaXBar.io.upstream.last <> io.ext.dma
  dmaBridge.axi <> dmaXBar.io.downstream.head
  dmaAsyncModule.io.icn <> dmaBridge.icn
  io.async.dma <> dmaAsyncModule.io.async

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

  io.cpu.msip := ShiftSync(pb.io.msip)
  io.cpu.mtip := ShiftSync(pb.io.mtip)
  io.cpu.meip := ShiftSync(pb.io.meip)
  io.cpu.seip := ShiftSync(pb.io.seip)
  io.cpu.dbip := ShiftSync(pb.io.dbip)
  pb.io.extIntr := io.ext.intr
  pb.io.timerTick := io.ext.timerTick
  pb.io.debug <> io.debug
  io.resetCtrl.hartResetReq.foreach(_ := ShiftSync(pb.io.resetCtrl.hartResetReq.get))
  pb.io.resetCtrl.hartIsInReset := ShiftSync(io.resetCtrl.hartIsInReset)
}