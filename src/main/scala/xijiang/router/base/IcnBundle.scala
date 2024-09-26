package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import zhujiang.ZJBundle
import zhujiang.chi._

class IcnTxBundle(node: Node)(implicit p: Parameters) extends ZJBundle {
  private val illegal = node.ejects.contains("REQ") && node.ejects.contains("ERQ")
  require(!illegal)
  val req = if(node.ejects.contains("REQ") || node.ejects.contains("ERQ") && !node.csnNode) {
    if(node.splitFlit) Some(Decoupled(new ReqFlit)) else Some(Decoupled(UInt(reqFlitBits.W)))
  } else None

  val resp = if(node.ejects.contains("RSP")) {
    if(node.splitFlit) Some(Decoupled(new RespFlit)) else Some(Decoupled(UInt(respFlitBits.W)))
  } else None

  val data = if(node.ejects.contains("DAT")) {
    if(node.splitFlit) Some(Decoupled(new DataFlit)) else Some(Decoupled(UInt(dataFlitBits.W)))
  } else None

  val snoop = if(node.ejects.contains("SNP")) {
    if(node.splitFlit) Some(Decoupled(new SnoopFlit)) else Some(Decoupled(UInt(snoopFlitBits.W)))
  } else None

  def getBundle(chn: String): Option[DecoupledIO[Data]] = {
    chn match {
      case "REQ" => if(node.ejects.contains("REQ")) req else None
      case "RSP" => resp
      case "DAT" => data
      case "SNP" => snoop
      case "ERQ" => if(node.ejects.contains("ERQ")) req else None
      case _ => None
    }
  }
}

class IcnRxBundle(node: Node)(implicit p: Parameters) extends ZJBundle {
  private val illegal = node.injects.contains("REQ") && node.injects.contains("ERQ")
  require(!illegal)
  val req = if(node.injects.contains("REQ") || node.injects.contains("ERQ") && !node.csnNode) {
    if(node.splitFlit) Some(Flipped(Decoupled(new ReqFlit))) else Some(Flipped(Decoupled(UInt(reqFlitBits.W))))
  } else None

  val resp = if(node.injects.contains("RSP")) {
    if(node.splitFlit) Some(Flipped(Decoupled(new RespFlit))) else Some(Flipped(Decoupled(UInt(respFlitBits.W))))
  } else None

  val data = if(node.injects.contains("DAT")) {
    if(node.splitFlit) Some(Flipped(Decoupled(new DataFlit))) else Some(Flipped(Decoupled(UInt(dataFlitBits.W))))
  } else None

  val snoop = if(node.injects.contains("SNP")) {
    if(node.splitFlit) Some(Flipped(Decoupled(new SnoopFlit))) else Some(Flipped(Decoupled(UInt(snoopFlitBits.W))))
  } else None

  def getBundle(chn: String): Option[DecoupledIO[Data]] = {
    chn match {
      case "REQ" => if(node.injects.contains("REQ")) req else None
      case "RSP" => resp
      case "DAT" => data
      case "SNP" => snoop
      case "ERQ" => if(node.injects.contains("ERQ")) req else None
      case _ => None
    }
  }
}

class IcnBundle(val node: Node)(implicit p: Parameters) extends ZJBundle {
  val tx = new IcnTxBundle(node)
  val rx = new IcnRxBundle(node)
  val clusterId = if(node.nodeType == NodeType.CC) Some(Output(UInt(clusterIdBits.W))) else None
}
