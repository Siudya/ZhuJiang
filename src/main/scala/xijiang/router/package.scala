package xijiang

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.base.BaseRouter
import zhujiang.ZJParametersKey
import zhujiang.chi._

package object router {
  class RequestRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    require(node.nodeType == NodeType.RF || node.nodeType == NodeType.RI)
    private val nStr = if(node.nodeType == NodeType.RF) "rnf" else "rni"
    private val injectReqFlit = icn.rx.req.get.bits.asTypeOf(new ReqFlit)
    if(p(ZJParametersKey).tfsParams.isEmpty) {
      if(node.csnNode) {
        reqBufEnq.get.bits.tgt := genNodeId(1.U(1.W), NodeType.HF.U(nodeTypeBits.W), injectReqFlit.tgtChipId)
      } else {
        reqBufEnq.get.bits.tgt := Mux(injectReqFlit.mmioReq, node.tgtHomeI.U(niw.W), node.tgtHomeF.U(niw.W))
      }
    }

    if(!node.csnNode) {
      print(
        s"""
           |RequestNode ${nStr}_${node.nid} {
           |  node_id: 0x${node.nodeId.toHexString}
           |  prefer_home_full: 0x${node.tgtHomeF.toHexString}
           |  prefer_home_io: 0x${node.tgtHomeI.toHexString}
           |  lefts: ${node.leftNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
           |  rights: ${node.rightNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
           |}
           |""".stripMargin)
    }
  }

  class HomeRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    require(node.nodeType == NodeType.HF || node.nodeType == NodeType.HI)
    private val nStr = if(node.nodeType == NodeType.HF) "hnf" else "hni"
    if(!node.csnNode) {
      print(
        s"""
           |HomeNode ${nStr}_${node.nid} {
           |  node_id: 0x${node.nodeId.toHexString}
           |  lefts: ${node.leftNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
           |  rights: ${node.rightNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
           |}
           |""".stripMargin)
    }
  }

  class SubordinateRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    require(node.nodeType == NodeType.S)
    private val nStr = if(node.mainMemory) "mem" else "dcu"
    if(!node.csnNode) {
      print(
        s"""
           |SubordinateNode ${nStr}_sn_${node.nid} {
           |  node_id: 0x${node.nodeId.toHexString}
           |  lefts: ${node.leftNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
           |  rights: ${node.rightNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
           |}
           |""".stripMargin)
    }
  }

  class PipelineRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    router.nodeId := node.nodeId.U
  }

  class ChipToChipRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    require(node.csnNode && node.nodeType == NodeType.C)
  }
}
