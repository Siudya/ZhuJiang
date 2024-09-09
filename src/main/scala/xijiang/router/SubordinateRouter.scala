package xijiang.router

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.base.BaseRouter
import xijiang.{Node, NodeType}
import zhujiang.ZJBundle
import zhujiang.chi._

class SnTx(node: Node)(implicit p: Parameters) extends ZJBundle {
  val ereq = if(node.splitFlit) Decoupled(new ReqFlit) else Decoupled(UInt(reqFlitBits.W))
  val data = if(node.splitFlit) Decoupled(new DataFlit) else Decoupled(UInt(dataFlitBits.W))
}

class SnRx(node: Node)(implicit p: Parameters) extends ZJBundle {
  val resp = if(node.splitFlit) Flipped(Decoupled(new RespFlit)) else Flipped(Decoupled(UInt(respFlitBits.W)))
  val data = if(node.splitFlit) Flipped(Decoupled(new DataFlit)) else Flipped(Decoupled(UInt(dataFlitBits.W)))
}

class SnIcn(node: Node)(implicit p: Parameters) extends ZJBundle {
  val tx = new SnTx(node)
  val rx = new SnRx(node)
}

class SubordinateRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node,
  Seq("ERQ", "DAT"), Seq("RSP", "DAT")) {
  val icn = IO(new SnIcn(node))

  connEject(icn.tx.ereq, "ERQ")
  connEject(icn.tx.data, "DAT")
  connInject(icn.rx.resp, "RSP")
  connInject(icn.rx.data, "DAT")

  if(!node.csnNode) {
    print(
      s"""
         |SubordinateNode sn_${node.nid} {
         |  node_id: 0x${node.nodeId.toHexString}
         |  lefts: ${node.leftNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  rights: ${node.rightNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |}
         |""".stripMargin)
  }
}
