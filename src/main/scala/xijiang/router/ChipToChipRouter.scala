package xijiang.router

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import zhujiang.ZJBundle
import xijiang.router.base.BaseRouter

class CnTx(implicit p: Parameters) extends ZJBundle {
  val req = Decoupled(UInt(reqFlitBits.W))
  val resp = Decoupled(UInt(respFlitBits.W))
  val data = Decoupled(UInt(dataFlitBits.W))
  val snoop = Decoupled(UInt(snoopFlitBits.W))
}

class CnRx(implicit p: Parameters) extends ZJBundle {
  val req = Flipped(Decoupled(UInt(reqFlitBits.W)))
  val resp = Flipped(Decoupled(UInt(respFlitBits.W)))
  val data = Flipped(Decoupled(UInt(dataFlitBits.W)))
  val snoop = Flipped(Decoupled(UInt(snoopFlitBits.W)))
}

class CnIcn(implicit p: Parameters) extends ZJBundle {
  val tx = new CnTx
  val rx = new CnRx
}

class ChipToChipRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node,
  Seq("RSP", "DAT", "SNP", "REQ"), Seq("RSP", "DAT", "SNP", "REQ")) {
  val icn = IO(new CnIcn)
  require(node.csnNode && node.nodeType == NodeType.C)
  icn.tx.req <> ejectMap("REQ")
  icn.tx.resp <> ejectMap("RSP")
  icn.tx.data <> ejectMap("DAT")
  icn.tx.snoop <> ejectMap("SNP")
  injectMap("REQ") <> icn.rx.req
  injectMap("RSP") <> icn.rx.resp
  injectMap("DAT") <> icn.rx.data
  injectMap("SNP") <> icn.rx.snoop
}
