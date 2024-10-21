package zhujiang.device.dma

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import xijiang.router.base.DeviceIcnBundle
import zhujiang.ZJModule
import zhujiang.axi._
import zhujiang.chi._

case class DmaParams(
  bufferSize: Int = 32,
  idBits: Int = 12
)

class Axi2Chi(node: Node)(implicit p: Parameters) extends ZJModule {
  require(node.nodeType == NodeType.RI)
  private val dmaParams = zjParams.dmaParams
  private val axiParams = AxiParams(dataBits = dw, addrBits = raw, idBits = dmaParams.idBits)
  val axi = IO(Flipped(new AxiBundle(axiParams)))
  val icn = IO(new DeviceIcnBundle(node))

  dontTouch(icn)
  dontTouch(axi)
  icn.tx := DontCare
  axi := DontCare

  //How to use CHI rx flit
  private val rxRespBits = WireInit(icn.rx.resp.get.bits.asTypeOf(new RespFlit))
  private val rxDataBits = WireInit(icn.rx.data.get.bits.asTypeOf(new DataFlit))
  dontTouch(rxRespBits)
  dontTouch(rxDataBits)

  //How to drive CHI tx flit
  private val txReqFlit = Wire(new ReqFlit)
  txReqFlit := DontCare
  txReqFlit.Opcode := ReqOpcode.ReadOnce
  txReqFlit.Addr := 0x12345678L.U
  icn.tx.req.get.bits := txReqFlit.asTypeOf(icn.tx.req.get.bits)
  icn.tx.req.get.valid := true.B

  icn.rx.resp.get.ready := false.B
  icn.rx.data.get.ready := false.B

}
