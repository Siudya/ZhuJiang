package xijiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router._
import xijiang.router.base.BaseRouter
import zhujiang.chi.NodeIdBundle

object NodeType {
  val RF: Int = 0
  val RI: Int = 1
  val HF: Int = 2
  val HI: Int = 3
  val C: Int = 4
  val S: Int = 5
  val P: Int = 6
  def width: Int = log2Ceil(P)
  def min: Int = RF
  def max: Int = P
}

case class NodeParam(
  name: String = "",
  nodeType: Int = NodeType.RF,
  splitFlit: Boolean = false,
  mainMemory: Boolean = false
) {
  if(mainMemory) require(nodeType == NodeType.S)
}

case class Node(
  suffix: String = "",
  nodeType: Int = NodeType.RF,
  csnNode: Boolean = false,
  nodeNetBits: Int = 1,
  nodeNidBits: Int = 4,
  ringSize: Int = 3,
  oddNode: Boolean = false,
  splitFlit: Boolean = false,
  mainMemory: Boolean = false
) {
  var nid: Int = 0
  var left: Node = null
  var right: Node = null

  lazy val net = if(csnNode) 1 else 0
  lazy private val netOff = NodeType.width + nodeNidBits
  lazy private val typeOff = nodeNidBits
  require(NodeType.min <= nodeType && nodeType <= NodeType.max)
  lazy val nodeId = (net << netOff) | (nodeType << typeOff) | nid

  lazy val (leftNodes, rightNodes) = getLeftAndRight
  private def getLeftAndRight: (Seq[Node], Seq[Node]) = {
    require(left != null)
    require(right != null)
    val otherNodesNum = ringSize - 1
    val leftNum = if(oddNode) otherNodesNum / 2 else (otherNodesNum + 1) / 2
    var leftNow: Node = this
    val leftNodes = for(i <- 0 until leftNum) yield {
      leftNow = leftNow.left
      require(leftNow != null)
      leftNow
    }

    val rightNum = otherNodesNum - leftNum
    var rightNow: Node = this
    val rightNodes = for(i <- 0 until rightNum) yield {
      rightNow = rightNow.right
      require(rightNow != null)
      rightNow
    }
    (leftNodes, rightNodes)
  }

  def genRouter(p: Parameters): BaseRouter = {
    val csnStr = if(csnNode && nodeType != NodeType.C) "c" else ""
    val (res, nodeStr) = nodeType match {
      case NodeType.RF => (Module(new RequestRouter(this)(p)), "rnf")
      case NodeType.RI => (Module(new RequestRouter(this)(p)), "rni")
      case NodeType.HF => (Module(new HomeRouter(this)(p)), "hnf")
      case NodeType.HI => (Module(new HomeRouter(this)(p)), "hni")
      case NodeType.C => (Module(new ChipToChipRouter(this)(p)), "c2c")
      case NodeType.S => (Module(new SubordinateRouter(this)(p)), "sn")
      case _ => (Module(new PipelineRouter(this)(p)), "pip")
    }
    res.suggestName(s"$csnStr$nodeStr$suffix")
    res
  }

  lazy val routerPrefixStr: String = if(csnNode) "Csn" else ""
  lazy val routerStr: String = nodeType match {
    case NodeType.RF => s"${routerPrefixStr}RequestFullRouter"
    case NodeType.RI => s"${routerPrefixStr}RequestIoRouter"
    case NodeType.HF => s"${routerPrefixStr}HomeFullFullRouter"
    case NodeType.HI => s"${routerPrefixStr}HomeIoRouter"
    case NodeType.C => s"${routerPrefixStr}C2cRouter"
    case NodeType.S => s"${routerPrefixStr}SubordinateRouter"
    case _ => s"${routerPrefixStr}PipelineRouter"
  }

  lazy val icnPrefixStr: String = if(csnNode) "csn_" else ""
  lazy val icnStr: String = nodeType match {
    case NodeType.RF => s"${icnPrefixStr}rnf_id_${nodeId.toHexString}"
    case NodeType.RI => s"${icnPrefixStr}rni_id_${nodeId.toHexString}"
    case NodeType.HF => s"${icnPrefixStr}hnf_id_${nodeId.toHexString}"
    case NodeType.HI => s"${icnPrefixStr}hni_id_${nodeId.toHexString}"
    case NodeType.C => s"${icnPrefixStr}c2c_id_${nodeId.toHexString}"
    case NodeType.S => if(mainMemory) s"${icnPrefixStr}mem_sn_id_${nodeId.toHexString}" else s"${icnPrefixStr}dcu_sn_id_${nodeId.toHexString}"
    case _ => ""
  }

  lazy val nodeStr: String = nodeType match {
    case NodeType.RF => "RNF"
    case NodeType.RI => "RNI"
    case NodeType.HF => "HNF"
    case NodeType.HI => "HNI"
    case NodeType.C => "C2C"
    case NodeType.S => "SN"
    case _ => "PN"
  }

  lazy val ejects: Seq[String] = {
    val res = nodeType match {
      case NodeType.RF => Seq("RSP", "DAT", "SNP")
      case NodeType.RI => Seq("RSP", "DAT")
      case NodeType.HF => Seq("REQ", "RSP", "DAT")
      case NodeType.HI => Seq("REQ", "RSP", "DAT")
      case NodeType.C => Seq("RSP", "DAT", "SNP", "REQ")
      case NodeType.S => Seq("ERQ", "DAT")
      case _ => Seq()
    }
    val illegal = res.contains("REQ") && res.contains("ERQ")
    require(!illegal, "Cannot eject from both REQ and ERQ")
    res
  }

  lazy val injects: Seq[String] = {
    val res = nodeType match {
      case NodeType.RF => Seq("REQ", "RSP", "DAT")
      case NodeType.RI => Seq("REQ", "RSP", "DAT")
      case NodeType.HF => Seq("RSP", "DAT", "SNP", "ERQ")
      case NodeType.HI => Seq("RSP", "DAT", "ERQ")
      case NodeType.C => Seq("RSP", "DAT", "SNP", "REQ")
      case NodeType.S => Seq("RSP", "DAT")
      case _ => Seq()
    }
    val illegal = res.contains("REQ") && res.contains("ERQ")
    require(!illegal, "Cannot inject to both REQ and ERQ")
    res
  }

  private def getLocalLegalTarget(ring: Seq[Node]): Map[String, Seq[Int]] = {
    nodeType match {
      case NodeType.RF => Map[String, Seq[Int]](
        "REQ" -> ring.filter(n => n.nodeType == NodeType.HF || n.nodeType == NodeType.HI).map(_.nodeId).filterNot(_ == nodeId),
        "RSP" -> ring.filter(n => n.nodeType == NodeType.HF || n.nodeType == NodeType.HI).map(_.nodeId).filterNot(_ == nodeId),
        "DAT" -> ring.filter(n => n.nodeType == NodeType.HF || n.nodeType == NodeType.HI || n.nodeType == NodeType.S || n.nodeType == NodeType.RF).map(_.nodeId).filterNot(_ == nodeId)
      )
      case NodeType.RI => Map[String, Seq[Int]](
        "REQ" -> ring.filter(n => n.nodeType == NodeType.HF || n.nodeType == NodeType.HI).map(_.nodeId).filterNot(_ == nodeId),
        "RSP" -> ring.filter(n => n.nodeType == NodeType.HF || n.nodeType == NodeType.HI).map(_.nodeId).filterNot(_ == nodeId),
        "DAT" -> ring.filter(n => n.nodeType == NodeType.HF || n.nodeType == NodeType.HI || n.nodeType == NodeType.S).map(_.nodeId).filterNot(_ == nodeId)
      )
      case NodeType.HF => Map[String, Seq[Int]](
        "RSP" -> ring.filter(n => n.nodeType == NodeType.RF || n.nodeType == NodeType.RI || n.nodeType == NodeType.S).map(_.nodeId).filterNot(_ == nodeId),
        "DAT" -> ring.filter(n => n.nodeType == NodeType.RF || n.nodeType == NodeType.RI || n.nodeType == NodeType.S).map(_.nodeId).filterNot(_ == nodeId),
        "SNP" -> ring.filter(n => n.nodeType == NodeType.RF).map(_.nodeId).filterNot(_ == nodeId),
        "ERQ" -> ring.filter(n => n.nodeType == NodeType.S).map(_.nodeId).filterNot(_ == nodeId)
      )
      case NodeType.HI => Map[String, Seq[Int]](
        "RSP" -> ring.filter(n => n.nodeType == NodeType.RF || n.nodeType == NodeType.RI || n.nodeType == NodeType.S).map(_.nodeId).filterNot(_ == nodeId),
        "DAT" -> ring.filter(n => n.nodeType == NodeType.RF || n.nodeType == NodeType.RI || n.nodeType == NodeType.S).map(_.nodeId).filterNot(_ == nodeId),
        "ERQ" -> ring.filter(n => n.nodeType == NodeType.S).map(_.nodeId).filterNot(_ == nodeId)
      )
      case NodeType.S => Map[String, Seq[Int]](
        "RSP" -> ring.filter(n => n.nodeType == NodeType.HF || n.nodeType == NodeType.HI || n.nodeType == NodeType.RF || n.nodeType == NodeType.RI).map(_.nodeId).filterNot(_ == nodeId),
        "DAT" -> ring.filter(n => n.nodeType == NodeType.RF || n.nodeType == NodeType.RI || n.nodeType == NodeType.HF || n.nodeType == NodeType.HI || (n.nodeType == NodeType.S & n.mainMemory)).map(_.nodeId).filterNot(_ == nodeId),
      )
      case _ => Map[String, Seq[Int]]()
    }
  }

  private def getCsnLegalTarget(ring: Seq[Node]): Map[String, Seq[Int]] = {
    nodeType match {
      case NodeType.RF => Map[String, Seq[Int]](
        "REQ" -> ring.filter(n => n.nodeType == NodeType.C).map(_.nodeId),
        "RSP" -> ring.filter(n => n.nodeType == NodeType.C).map(_.nodeId),
        "DAT" -> ring.filter(n => n.nodeType == NodeType.C).map(_.nodeId),
      )
      case NodeType.HF => Map[String, Seq[Int]](
        "RSP" -> ring.filter(n => n.nodeType == NodeType.C).map(_.nodeId).filterNot(_ == nodeId),
        "DAT" -> ring.filter(n => n.nodeType == NodeType.C).map(_.nodeId).filterNot(_ == nodeId),
        "SNP" -> ring.filter(n => n.nodeType == NodeType.C).map(_.nodeId).filterNot(_ == nodeId)
      )
      case NodeType.C => Map[String, Seq[Int]](
        "REQ" -> ring.filter(n => n.nodeType == NodeType.HF).map(_.nodeId).filterNot(_ == nodeId),
        "RSP" -> ring.filter(n => n.nodeType == NodeType.HF || n.nodeType == NodeType.RF).map(_.nodeId).filterNot(_ == nodeId),
        "DAT" -> ring.filter(n => n.nodeType == NodeType.HF || n.nodeType == NodeType.RF).map(_.nodeId).filterNot(_ == nodeId),
        "SNP" -> ring.filter(n => n.nodeType == NodeType.RF).map(_.nodeId).filterNot(_ == nodeId),
      )
      case _ => Map[String, Seq[Int]]()
    }
  }

  private def checkTarget(n: Int, tgt: NodeIdBundle, localChip: Option[UInt]): Bool = {
    if(csnNode && nodeType == NodeType.C) {
      (n.U(tgt.getWidth.W) | localChip.get) === tgt.asUInt
    } else if(csnNode && nodeType != NodeType.C) {
      n.U(tgt.getWidth.W).asTypeOf(tgt).nodeCsnChip === tgt.nodeCsnChip
    } else {
      n.U(tgt.getWidth.W) === tgt.asUInt
    }
  }

  def checkLegalStaticInjectTarget(ring: Seq[Node], chn: String, tgt: NodeIdBundle, valid: Bool, nid: UInt, localChip: Option[UInt]): Unit = {
    val targetsSeq = if(csnNode) getCsnLegalTarget(ring)(chn) else getLocalLegalTarget(ring)(chn)
    require(targetsSeq.nonEmpty, s"targets are empty when making node 0x${nodeId.toHexString} of $chn")
    val targetHits = targetsSeq.map(checkTarget(_, tgt, localChip))
    val legalStr = targetsSeq.map(i => s"0x${i.toHexString} ").reduce(_ + _)
    val legal = Cat(targetHits).orR
    when(valid) {
      assert(legal, cf"Illegal target id 0x${tgt.asUInt}%x of $chn flit @ node 0x${nid}%x legal target_id: $legalStr")
    }
  }
}
