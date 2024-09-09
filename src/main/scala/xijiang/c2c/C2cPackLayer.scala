package xijiang.c2c

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.Node
import xijiang.router.{CnRx, CnTx}
import zhujiang.{ZJBundle, ZJModule, ZJParametersKey}

class C2cPayload(implicit p: Parameters) extends ZJBundle {
  val flit = UInt(maxFlitBits.W)
  val cid = UInt(2.W)
}

class C2cRingPort(node: Node)(implicit p: Parameters) extends ZJBundle {
  val tx = Flipped(new CnRx(node))
  val rx = Flipped(new CnTx(node))
  val chip = Output(UInt(chipAddrBits.W))
}

class C2cLinkPort(implicit p: Parameters) extends ZJBundle {
  val tx = Decoupled(new C2cPayload)
  val rx = Flipped(Decoupled(new C2cPayload))
  val chip = Input(UInt(chipAddrBits.W))
}

class C2cPackLayerTx(node: Node)(implicit p: Parameters) extends ZJModule {
  private val c2cParams = p(ZJParametersKey).c2cParams
  val io = IO(new Bundle {
    val fromRing = Flipped(new CnTx(node))
    val toLink = Decoupled(new C2cPayload)
  })
  private val reqTxBuffer = Module(new Queue(UInt(reqFlitBits.W), c2cParams.reqBufTxDepth))
  private val respTxBuffer = Module(new Queue(UInt(respFlitBits.W), c2cParams.respBufTxDepth))
  private val dataTxBuffer = Module(new Queue(UInt(dataFlitBits.W), c2cParams.dataBufTxDepth))
  private val snoopTxBuffer = Module(new Queue(UInt(snoopFlitBits.W), c2cParams.snoopBufTxDepth))
  private def connRing(buf: Queue[UInt], fromRing: DecoupledIO[Data]): Unit = {
    buf.io.enq.valid := fromRing.valid
    buf.io.enq.bits := fromRing.bits.asUInt
    fromRing.ready := buf.io.enq.ready
  }
  connRing(reqTxBuffer, io.fromRing.req)
  connRing(respTxBuffer, io.fromRing.resp)
  connRing(dataTxBuffer, io.fromRing.data)
  connRing(snoopTxBuffer, io.fromRing.snoop)

  private val arb = Module(new RRArbiter(UInt(maxFlitBits.W), 4))
  arb.io.in(0) <> reqTxBuffer.io.deq
  arb.io.in(1) <> respTxBuffer.io.deq
  arb.io.in(2) <> dataTxBuffer.io.deq
  arb.io.in(3) <> snoopTxBuffer.io.deq

  io.toLink.valid := arb.io.out.valid
  io.toLink.bits.flit := arb.io.out.bits
  io.toLink.bits.cid := arb.io.chosen
  arb.io.out.ready := io.toLink.ready
}

class C2cPackLayerRx(node: Node)(implicit p: Parameters) extends ZJModule {
  private val c2cParams = p(ZJParametersKey).c2cParams
  val io = IO(new Bundle {
    val toRing = Flipped(new CnRx(node))
    val fromLink = Flipped(Decoupled(new C2cPayload))
  })
  private val reqRxBuffer = Module(new Queue(UInt(reqFlitBits.W), c2cParams.reqBufRxDepth))
  private val respRxBuffer = Module(new Queue(UInt(respFlitBits.W), c2cParams.respBufRxDepth))
  private val dataRxBuffer = Module(new Queue(UInt(dataFlitBits.W), c2cParams.dataBufRxDepth))
  private val snoopRxBuffer = Module(new Queue(UInt(snoopFlitBits.W), c2cParams.snoopBufRxDepth))
  private def connRing(buf: Queue[UInt], toRing: DecoupledIO[Data]): Unit = {
    toRing.valid := buf.io.deq.valid
    toRing.bits := buf.io.deq.bits.asTypeOf(toRing.bits)
    buf.io.deq.ready := toRing.ready
  }
  connRing(reqRxBuffer, io.toRing.req)
  connRing(respRxBuffer, io.toRing.resp)
  connRing(dataRxBuffer, io.toRing.data)
  connRing(snoopRxBuffer, io.toRing.snoop)

  private val chooseOH = Seq.tabulate(4)(idx => idx).map(_.U === io.fromLink.bits.cid)
  reqRxBuffer.io.enq.valid := io.fromLink.valid && chooseOH(0)
  reqRxBuffer.io.enq.bits := io.fromLink.bits.flit(reqFlitBits - 1, 0)

  respRxBuffer.io.enq.valid := io.fromLink.valid && chooseOH(1)
  respRxBuffer.io.enq.bits := io.fromLink.bits.flit(respFlitBits - 1, 0)

  dataRxBuffer.io.enq.valid := io.fromLink.valid && chooseOH(2)
  dataRxBuffer.io.enq.bits := io.fromLink.bits.flit(dataFlitBits - 1, 0)

  snoopRxBuffer.io.enq.valid := io.fromLink.valid && chooseOH(3)
  snoopRxBuffer.io.enq.bits := io.fromLink.bits.flit(snoopFlitBits - 1, 0)

  io.fromLink.ready := Mux1H(chooseOH, Seq(
    reqRxBuffer.io.enq.ready,
    respRxBuffer.io.enq.ready,
    dataRxBuffer.io.enq.ready,
    snoopRxBuffer.io.enq.ready
  ))
}

class C2cPackLayer(node: Node)(implicit p: Parameters) extends ZJModule {
  val io = IO(new Bundle {
    val ring = new C2cRingPort(node)
    val c2c = new C2cLinkPort
  })
  private val transmitter = Module(new C2cPackLayerTx(node))
  private val receiver = Module(new C2cPackLayerRx(node))
  transmitter.io.fromRing <> io.ring.rx
  io.c2c.tx <> transmitter.io.toLink
  receiver.io.fromLink <> io.c2c.rx
  io.ring.tx <> receiver.io.toRing
  io.ring.chip := io.c2c.chip
}