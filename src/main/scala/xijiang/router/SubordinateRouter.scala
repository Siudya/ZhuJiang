package xijiang.router

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.base.BaseRouter
import xijiang.{Node, NodeType}
import zhujiang.ZJBundle

class SnTx(implicit p: Parameters) extends ZJBundle {
  val resp = Decoupled(UInt(respFlitBits.W))
  val data = Decoupled(UInt(dataFlitBits.W))
}

class SnRx(implicit p: Parameters) extends ZJBundle {
  val req = Flipped(Decoupled(UInt(reqFlitBits.W)))
  val data = Flipped(Decoupled(UInt(dataFlitBits.W)))
}

class SnIcn(implicit p: Parameters) extends ZJBundle {
  val tx = new SnTx
  val rx = new SnRx
}

class SubordinateRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node,
  Seq("RSP", "DAT"), Seq("REQ", "DAT")) {
  val icn = IO(new SnIcn)

  icn.tx.resp <> ejectMap("RSP")
  icn.tx.data <> ejectMap("DAT")
  injectMap("REQ") <> icn.rx.req
  injectMap("DAT") <> icn.rx.data

  if(!node.csnNode) {
    print(
      s"""
         |SubordinateNode sn_${node.nid} {
         |  node_id: 0x${node.nodeId.litValue.toInt.toHexString}
         |  lefts: ${node.leftNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  rights: ${node.rightNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |}
         |""".stripMargin)
  }
}
