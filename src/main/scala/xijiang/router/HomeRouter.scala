package xijiang.router

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.base.BaseRouter
import xijiang.{Node, NodeType}
import zhujiang.ZJBundle

class HnTx(implicit p: Parameters) extends ZJBundle {
  val req = Decoupled(UInt(reqFlitBits.W))
  val resp = Decoupled(UInt(respFlitBits.W))
  val data = Decoupled(UInt(dataFlitBits.W))
}

class HnRx(local: Boolean)(implicit p: Parameters) extends ZJBundle {
  val ereq = if(local) Some(Flipped(Decoupled(UInt(reqFlitBits.W)))) else None
  val resp = Flipped(Decoupled(UInt(respFlitBits.W)))
  val data = Flipped(Decoupled(UInt(dataFlitBits.W)))
  val snoop = Flipped(Decoupled(UInt(snoopFlitBits.W)))
}

class HnIcn(local: Boolean)(implicit p: Parameters) extends ZJBundle {
  val tx = new HnTx
  val rx = new HnRx(local)
}

class HomeRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node,
  Seq("REQ", "RSP", "DAT"), Seq("RSP", "DAT", "SNP", "ERQ")) {
  private val local = !node.csnNode
  val icn = IO(new HnIcn(local))

  injectMap("RSP") <> icn.rx.resp
  injectMap("DAT") <> icn.rx.data
  injectMap("SNP") <> icn.rx.snoop
  if(local) injectMap("ERQ") <> icn.rx.ereq.get
  icn.tx.req <> ejectMap("REQ")
  icn.tx.resp <> ejectMap("RSP")
  icn.tx.data <> ejectMap("DAT")

  if(!node.csnNode) {
    val nStr = if(node.nodeType == NodeType.HF) "hnf" else "hni"
    print(
      s"""
         |HomeNode ${nStr}_${node.nid} {
         |  node_id: 0x${node.nodeId.litValue.toInt.toHexString}
         |  lefts: ${node.leftNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  rights: ${node.rightNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |}
         |""".stripMargin)
  }
}
