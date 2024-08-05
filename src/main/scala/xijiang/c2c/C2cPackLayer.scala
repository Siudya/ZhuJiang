package xijiang.c2c

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.{CnRx, CnTx}
import zhujiang.{ZJBundle, ZJModule, ZJParametersKey}

class C2cPayload(implicit p: Parameters) extends ZJBundle {
  val flit = UInt(maxFlitBits.W)
  val cid = UInt(2.W)
}

class C2cRingPort(implicit p: Parameters) extends ZJBundle {
  val tx = Flipped(new CnRx)
  val rx = Flipped(new CnTx)
  val chip = Output(UInt(chipAddrBits.W))
}

class C2cLinkPort(implicit p: Parameters) extends ZJBundle {
  val tx = Decoupled(new C2cPayload)
  val rx = Flipped(Decoupled(new C2cPayload))
  val chip = Input(UInt(chipAddrBits.W))
}

class C2cPackLayerTx(implicit p: Parameters) extends ZJModule {
  private val c2cParams = p(ZJParametersKey).c2cParams
  val io = IO(new Bundle {
    val fromRing = Flipped(new CnTx)
    val toLink = Decoupled(new C2cPayload)
  })
  private val reqTxBuffer = Module(new Queue(UInt(reqFlitBits.W), c2cParams.reqBufTxDepth))
  private val respTxBuffer = Module(new Queue(UInt(respFlitBits.W), c2cParams.respBufTxDepth))
  private val dataTxBuffer = Module(new Queue(UInt(dataFlitBits.W), c2cParams.dataBufTxDepth))
  private val snoopTxBuffer = Module(new Queue(UInt(snoopFlitBits.W), c2cParams.snoopBufTxDepth))
  reqTxBuffer.io.enq <> io.fromRing.req
  respTxBuffer.io.enq <> io.fromRing.resp
  dataTxBuffer.io.enq <> io.fromRing.data
  snoopTxBuffer.io.enq <> io.fromRing.snoop

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

class C2cPackLayerRx(implicit p: Parameters) extends ZJModule {
  private val c2cParams = p(ZJParametersKey).c2cParams
  val io = IO(new Bundle {
    val toRing = Flipped(new CnRx)
    val fromLink = Flipped(Decoupled(new C2cPayload))
  })
  private val reqRxBuffer = Module(new Queue(UInt(reqFlitBits.W), c2cParams.reqBufRxDepth))
  private val respRxBuffer = Module(new Queue(UInt(respFlitBits.W), c2cParams.respBufRxDepth))
  private val dataRxBuffer = Module(new Queue(UInt(dataFlitBits.W), c2cParams.dataBufRxDepth))
  private val snoopRxBuffer = Module(new Queue(UInt(snoopFlitBits.W), c2cParams.snoopBufRxDepth))
  io.toRing.req <> reqRxBuffer.io.deq
  io.toRing.resp <> respRxBuffer.io.deq
  io.toRing.data <> dataRxBuffer.io.deq
  io.toRing.snoop <> snoopRxBuffer.io.deq

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

class C2cPackLayer(implicit p: Parameters) extends ZJModule {
  val io = IO(new Bundle {
    val ring = new C2cRingPort
    val c2c = new C2cLinkPort
  })
  private val transmitter = Module(new C2cPackLayerTx)
  private val receiver = Module(new C2cPackLayerRx)
  transmitter.io.fromRing <> io.ring.rx
  io.c2c.tx <> transmitter.io.toLink
  receiver.io.fromLink <> io.c2c.rx
  io.ring.tx <> receiver.io.toRing
  io.ring.chip := io.c2c.chip
}