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
  val icn = IO(new DeviceIcnBundle(node))
  val axi = IO(new AxiBundle(axiParams))

  private val rxRespBits = WireInit(icn.rx.resp.get.bits.asTypeOf(new RespFlit))
  private val rxDataBits = WireInit(icn.rx.data.get.bits.asTypeOf(new DataFlit))
  dontTouch(icn)
  dontTouch(axi)
  dontTouch(rxRespBits)
  dontTouch(rxDataBits)

  icn.rx.resp.get.ready := false.B
  icn.rx.data.get.ready := false.B
  icn.tx := DontCare
  axi := DontCare
}
