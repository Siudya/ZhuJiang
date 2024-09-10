package xijiang.router

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.base.{BaseIcnBundle, BaseRouter}
import xijiang.{Node, NodeType}
import zhujiang.ZJBundle
import zhujiang.chi._

class HniTx(node: Node)(implicit p: Parameters) extends ZJBundle {
  val req = if(node.splitFlit) Decoupled(new ReqFlit) else Decoupled(UInt(reqFlitBits.W))
  val resp = if(node.splitFlit) Decoupled(new RespFlit) else Decoupled(UInt(respFlitBits.W))
  val data = if(node.splitFlit) Decoupled(new DataFlit) else Decoupled(UInt(dataFlitBits.W))
}

class HniRx(local: Boolean, node: Node)(implicit p: Parameters) extends ZJBundle {
  val req = if(local) {
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
}

class HniIcn(local: Boolean, node: Node)(implicit p: Parameters) extends BaseIcnBundle(node) {
  val tx = new HniTx(node)
  val rx = new HniRx(local, node)
}

class HomeIoRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node,
  Seq("REQ", "RSP", "DAT"), Seq("RSP", "DAT", "ERQ")) {
  val icn = IO(new HniIcn(local, node))

  connEject(icn.tx.req, "REQ")
  connEject(icn.tx.resp, "RSP")
  connEject(icn.tx.data, "DAT")
  if(local) connInject(icn.rx.req.get, "ERQ")
  connInject(icn.rx.resp, "RSP")
  connInject(icn.rx.data, "DAT")

  if(!node.csnNode) {
    print(
      s"""
         |HomeNode hni_${node.nid} {
         |  node_id: 0x${node.nodeId.toHexString}
         |  lefts: ${node.leftNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |  rights: ${node.rightNodes.map(e => "0x" + e.toHexString).reduce((a: String, b: String) => s"$a, $b")}
         |}
         |""".stripMargin)
  }
}
