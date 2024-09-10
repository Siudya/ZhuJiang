package xijiang.router

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import zhujiang.ZJBundle
import xijiang.router.base.{BaseIcnBundle, BaseRouter}
import zhujiang.chi._

class CnTx(node: Node)(implicit p: Parameters) extends ZJBundle {
  val req = if(node.splitFlit) Decoupled(new ReqFlit) else Decoupled(UInt(reqFlitBits.W))
  val resp = if(node.splitFlit) Decoupled(new RespFlit) else Decoupled(UInt(respFlitBits.W))
  val data = if(node.splitFlit) Decoupled(new DataFlit) else Decoupled(UInt(dataFlitBits.W))
  val snoop = if(node.splitFlit) Decoupled(new SnoopFlit) else Decoupled(UInt(snoopFlitBits.W))
}

class CnRx(node: Node)(implicit p: Parameters) extends ZJBundle {
  val req = if(node.splitFlit) Flipped(Decoupled(new ReqFlit)) else Flipped(Decoupled(UInt(reqFlitBits.W)))
  val resp = if(node.splitFlit) Flipped(Decoupled(new RespFlit)) else Flipped(Decoupled(UInt(respFlitBits.W)))
  val data = if(node.splitFlit) Flipped(Decoupled(new DataFlit)) else Flipped(Decoupled(UInt(dataFlitBits.W)))
  val snoop = if(node.splitFlit) Flipped(Decoupled(new SnoopFlit)) else Flipped(Decoupled(UInt(snoopFlitBits.W)))
}

class CnIcn(node: Node)(implicit p: Parameters) extends BaseIcnBundle(node) {
  val tx = new CnTx(node)
  val rx = new CnRx(node)
}

class ChipToChipRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node,
  Seq("RSP", "DAT", "SNP", "REQ"), Seq("RSP", "DAT", "SNP", "REQ")) {
  val icn = IO(new CnIcn(node))
  require(node.csnNode && node.nodeType == NodeType.C)
  connEject(icn.tx.req, "REQ")
  connEject(icn.tx.resp, "RSP")
  connEject(icn.tx.data, "DAT")
  connEject(icn.tx.snoop, "SNP")
  connInject(icn.rx.req, "REQ")
  connInject(icn.rx.resp, "RSP")
  connInject(icn.rx.data, "DAT")
  connInject(icn.rx.snoop, "SNP")
}
