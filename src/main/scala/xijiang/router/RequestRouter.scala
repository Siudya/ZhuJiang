package xijiang.router

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import zhujiang.ZJBundle
import zhujiang.chi._

class RnTx(implicit p: Parameters) extends ZJBundle {
  val resp = Decoupled(UInt(respFlitBits.W))
  val data = Decoupled(UInt(dataFlitBits.W))
  val snoop = Decoupled(UInt(snoopFlitBits.W))
}

class RnRx(implicit p: Parameters) extends ZJBundle {
  val req = Flipped(Decoupled(UInt(reqFlitBits.W)))
  val resp = Flipped(Decoupled(UInt(respFlitBits.W)))
  val data = Flipped(Decoupled(UInt(dataFlitBits.W)))
}

class RnIcn(implicit p: Parameters) extends ZJBundle {
  val tx = new RnTx
  val rx = new RnRx
}

class RequestRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node,
  Seq("RSP", "DAT", "SNP"), Seq("REQ", "RSP", "DAT")
) {
  val icn = IO(new RnIcn)

  icn.tx.resp <> ejectMap("RSP")
  icn.tx.data <> ejectMap("DAT")
  icn.tx.snoop <> ejectMap("SNP")
  injectMap("REQ") <> icn.rx.req
  injectMap("RSP") <> icn.rx.resp
  injectMap("DAT") <> icn.rx.data

  private val injectReqFlit = WireInit(icn.rx.req.bits.asTypeOf(new ReqFlit))
  if(node.csnNode) {
    injectReqFlit.TgtID := genNodeId(1.U(1.W), NodeType.HF.U(nodeTypeBits.W), injectReqFlit.tgtChipId)
  } else {
    injectReqFlit.TgtID := Mux(injectReqFlit.mmioReq, node.tgtHomeI.U(niw.W), node.tgtHomeF.U(niw.W))
  }
  injectMap("REQ").bits := injectReqFlit.asUInt

  if(!node.csnNode) {
    print(
      s"""
         |RequestNode rn_${node.nid} {
         |  node_id: 0x${node.nodeId.litValue.toInt.toHexString}
         |  prefer_home_full: ${node.tgtHomeF & ((1 << nodeNidBits) - 1)}
         |  prefer_home_io: ${node.tgtHomeI & ((1 << nodeNidBits) - 1)}
         |  lefts: ${node.leftNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  rights: ${node.rightNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |}
         |""".stripMargin)
  }
}
