package xijiang.router

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.base.BaseRouter
import xijiang.{Node, NodeType}
import zhujiang.ZJBundle
import zhujiang.chi._

class HnTx(node: Node)(implicit p: Parameters) extends ZJBundle {
  val req = if(node.splitFlit) Decoupled(new ReqFlit) else Decoupled(UInt(reqFlitBits.W))
  val resp = if(node.splitFlit) Decoupled(new RespFlit) else Decoupled(UInt(respFlitBits.W))
  val data = if(node.splitFlit) Decoupled(new DataFlit) else Decoupled(UInt(dataFlitBits.W))
}

class HnRx(local: Boolean, node: Node)(implicit p: Parameters) extends ZJBundle {
  val ereq = if(local) {
    if(node.splitFlit) {
      Some(Flipped(Decoupled(new ReqFlit)))
    } else {
      Some(Flipped(Decoupled(UInt(reqFlitBits.W))))
    }
  } else {
    None
  }
  val resp = if(node.splitFlit) Flipped(Decoupled(new RespFlit)) else Flipped(Decoupled(UInt(respFlitBits.W)))
  val data = if(node.splitFlit) Flipped(Decoupled(new DataFlit)) else Flipped(Decoupled(UInt(dataFlitBits.W)))
  val snoop = if(node.splitFlit) Flipped(Decoupled(new SnoopFlit)) else Flipped(Decoupled(UInt(snoopFlitBits.W)))
}

class HnIcn(local: Boolean, node: Node)(implicit p: Parameters) extends ZJBundle {
  val tx = new HnTx(node)
  val rx = new HnRx(local, node)
}

class HomeRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node,
  Seq("REQ", "RSP", "DAT"), Seq("RSP", "DAT", "SNP", "ERQ")) {
  private val local = !node.csnNode
  val icn = IO(new HnIcn(local, node))

  connEject(icn.tx.req, "REQ")
  connEject(icn.tx.resp, "RSP")
  connEject(icn.tx.data, "DAT")
  if(local) connInject(icn.rx.ereq.get, "ERQ")
  connInject(icn.rx.resp, "RSP")
  connInject(icn.rx.data, "DAT")
  connInject(icn.rx.snoop, "SNP")

  if(!node.csnNode) {
    val nStr = if(node.nodeType == NodeType.HF) "hnf" else "hni"
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
