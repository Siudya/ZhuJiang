package xijiang.router

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import zhujiang.{ZJBundle, ZJParametersKey}
import zhujiang.chi._
import xijiang.router.base.{BaseIcnBundle, BaseRouter}

class RnTx(node: Node)(implicit p: Parameters) extends ZJBundle {
  val resp = if(node.splitFlit) Decoupled(new RespFlit) else Decoupled(UInt(respFlitBits.W))
  val data = if(node.splitFlit) Decoupled(new DataFlit) else Decoupled(UInt(dataFlitBits.W))
  val snoop = if(node.splitFlit) Decoupled(new SnoopFlit) else Decoupled(UInt(snoopFlitBits.W))
}

class RnRx(node: Node)(implicit p: Parameters) extends ZJBundle {
  val req = if(node.splitFlit) Flipped(Decoupled(new ReqFlit)) else Flipped(Decoupled(UInt(reqFlitBits.W)))
  val resp = if(node.splitFlit) Flipped(Decoupled(new RespFlit)) else Flipped(Decoupled(UInt(respFlitBits.W)))
  val data = if(node.splitFlit) Flipped(Decoupled(new DataFlit)) else Flipped(Decoupled(UInt(dataFlitBits.W)))
}

class RnIcn(node: Node)(implicit p: Parameters) extends BaseIcnBundle(node) {
  val tx = new RnTx(node)
  val rx = new RnRx(node)
}

class RequestRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node,
  Seq("RSP", "DAT", "SNP"), Seq("REQ", "RSP", "DAT")
) {
  val icn = IO(new RnIcn(node))

  connEject(icn.tx.resp, "RSP")
  connEject(icn.tx.data, "DAT")
  connEject(icn.tx.snoop, "SNP")
  connInject(icn.rx.req, "REQ")
  connInject(icn.rx.resp, "RSP")
  connInject(icn.rx.data, "DAT")

  private val injectReqFlit = WireInit(icn.rx.req.bits.asTypeOf(new ReqFlit))
  if(p(ZJParametersKey).tfsParams.isEmpty) {
    if(node.csnNode) {
      injectReqFlit.TgtID := genNodeId(1.U(1.W), NodeType.HF.U(nodeTypeBits.W), injectReqFlit.tgtChipId)
    } else {
      injectReqFlit.TgtID := Mux(injectReqFlit.mmioReq, node.tgtHomeI.U(niw.W), node.tgtHomeF.U(niw.W))
    }
  }
  injectMap("REQ").bits := injectReqFlit

  if(!node.csnNode) {
    print(
      s"""
         |RequestNode rn_${node.nid} {
         |  node_id: 0x${node.nodeId.toHexString}
         |  prefer_home_full: 0x${node.tgtHomeF.toHexString}
         |  prefer_home_io: 0x${node.tgtHomeI.toHexString}
         |  lefts: ${node.leftNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  rights: ${node.rightNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |}
         |""".stripMargin)
  }
}
