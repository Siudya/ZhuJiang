package xijiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.base.BaseRouter
import zhujiang.ZJParametersKey
import zhujiang.chi._

package object router {
  class RequestRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    require(node.nodeType == NodeType.RF || node.nodeType == NodeType.RI)
    private val nStr = if(node.nodeType == NodeType.RF) "rnf" else "rni"
    private val injectAddr = icn.rx.req.get.bits.asTypeOf(new ReqFlit).Addr.asTypeOf(new ReqAddrBundle)
    if(p(ZJParametersKey).tfsParams.isEmpty) {
      if(node.csnNode) {
        val c2cHits = router.remoteChipIds.get.map(_ === injectAddr.chip)
        when(icn.rx.req.get.valid) {
          assert(PopCount(c2cHits) === 1.U, cf"Invalid REQ chip target ${injectAddr.chip} of node 0x${node.nodeId.toHexString}")
        }
        val c2cId = PriorityEncoder(c2cHits)
        injectsMap("REQ").bits.tgt := Cat(
          1.U(nodeNetBits.W),
          NodeType.HF.U(nodeTypeBits.W),
          0.U((nodeNidBits - chipAddrBits).W),
          c2cId.asTypeOf(UInt(chipAddrBits.W))
        )
      } else {
        val hnfCnt = p(ZJParametersKey).localRing.count(_.nodeType == NodeType.HF)
        val hniCnt = p(ZJParametersKey).localRing.count(_.nodeType == NodeType.HI)
        val hnfId = if(hnfCnt > 1) injectAddr.tag(log2Ceil(hnfCnt) - 1, 0).asTypeOf(UInt(nodeNidBits.W)) else 0.U(nodeNidBits.W)
        val hniId = if(hniCnt > 1) injectAddr.tag(log2Ceil(hniCnt) - 1, 0).asTypeOf(UInt(nodeNidBits.W)) else 0.U(nodeNidBits.W)
        val tgtHnfId = Cat(0.U(nodeNetBits.W), NodeType.HF.U(nodeTypeBits.W), hnfId)
        val tgtHniId = Cat(0.U(nodeNetBits.W), NodeType.HI.U(nodeTypeBits.W), hniId)
        injectsMap("REQ").bits.tgt := Mux(injectAddr.mmio, tgtHniId, tgtHnfId)
      }
    }
    print(
      s"""
         |  RequestNode ${nStr}_${node.nid} {
         |    node_id: 0x${node.nodeId.toHexString}
         |    lefts: ${node.leftNodes.map(e => "0x" + e.nodeId.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |    rights: ${node.rightNodes.map(e => "0x" + e.nodeId.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  }
         |""".stripMargin)

  }

  class HomeRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    require(node.nodeType == NodeType.HF || node.nodeType == NodeType.HI)
    private val nStr = if(node.nodeType == NodeType.HF) "hnf" else "hni"
    if(p(ZJParametersKey).tfsParams.isEmpty) {
      if(node.csnNode) {
        injectsMap("SNP").bits.tgt := Cat(
          1.U(nodeNetBits.W),
          NodeType.RF.U(nodeTypeBits.W),
          0.U((nodeNidBits - chipAddrBits).W),
          icn.rx.snoop.get.bits.asTypeOf(new SnoopFlit).TgtID(chipAddrBits - 1, 0)
        )
      }
    }
    print(
      s"""
         |  HomeNode ${nStr}_${node.nid} {
         |    node_id: 0x${node.nodeId.toHexString}
         |    lefts: ${node.leftNodes.map(e => "0x" + e.nodeId.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |    rights: ${node.rightNodes.map(e => "0x" + e.nodeId.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  }
         |""".stripMargin)
  }

  class SubordinateRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    require(node.nodeType == NodeType.S)
    private val nStr = if(node.mainMemory) "mem" else "dcu"
    print(
      s"""
         |  SubordinateNode ${nStr}_sn_${node.nid} {
         |    node_id: 0x${node.nodeId.toHexString}
         |    lefts: ${node.leftNodes.map(e => "0x" + e.nodeId.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |    rights: ${node.rightNodes.map(e => "0x" + e.nodeId.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  }
         |""".stripMargin)
  }

  class PipelineRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    router.nodeId := node.nodeId.U
  }

  class ChipToChipRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node) {
    require(node.csnNode && node.nodeType == NodeType.C)
    private val csnHfNum = p(ZJParametersKey).csnRing.count(_.nodeType == NodeType.HF)
    private val csnRfNum = p(ZJParametersKey).csnRing.count(_.nodeType == NodeType.RF)
    if(p(ZJParametersKey).tfsParams.isEmpty) {
      val reqTgt = WireInit(icn.rx.req.get.bits.asTypeOf(new ReqFlit).TgtID.asTypeOf(new NodeIdBundle))
      val reqAddr = WireInit(icn.rx.req.get.bits.asTypeOf(new ReqFlit).Addr.asTypeOf(new ReqAddrBundle))
      val reqInject = injectsMap("REQ")
      if(csnHfNum > 1) {
        reqTgt.nodeCsnNid := reqAddr.csnNid(log2Ceil(csnHfNum) - 1, 0).asTypeOf(UInt((nodeNidBits - chipAddrBits).W))
        reqInject.bits.tgt := reqTgt.asUInt
      }

      val snoopTgt = WireInit(icn.rx.snoop.get.bits.asTypeOf(new SnoopFlit).TgtID.asTypeOf(new NodeIdBundle))
      val snoopAddr = WireInit(icn.rx.snoop.get.bits.asTypeOf(new SnoopFlit).Addr.asTypeOf(new SnpAddrBundle))
      val snoopInject = injectsMap("SNP")
      if(csnRfNum > 1) {
        snoopTgt.nodeCsnNid := snoopAddr.csnNid(log2Ceil(csnRfNum) - 1, 0).asTypeOf(UInt((nodeNidBits - chipAddrBits).W))
        snoopInject.bits.tgt := snoopTgt.asUInt
      }
    }
    print(
      s"""
         |  C2cNode c2c_${node.nid} {
         |    node_id: 0x${node.nodeId.toHexString}
         |    lefts: ${node.leftNodes.map(e => "0x" + e.nodeId.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |    rights: ${node.rightNodes.map(e => "0x" + e.nodeId.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  }
         |""".stripMargin)
  }
}
