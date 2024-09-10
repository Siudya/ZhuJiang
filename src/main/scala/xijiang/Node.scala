package xijiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router._
import xijiang.router.base.BaseRouter

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

  val net = if(csnNode) 1 else 0
  private val netOff = NodeType.width + nodeNidBits
  private val typeOff = nodeNidBits
  require(NodeType.min <= nodeType && nodeType <= NodeType.max)
  lazy val nodeId = (net << netOff) | (nodeType << typeOff) | nid

  lazy val (leftNodes, rightNodes) = getLeftAndRight
  private def getLeftAndRight: (Seq[Int], Seq[Int]) = {
    require(left != null)
    require(right != null)
    require(!csnNode)
    val otherNodesNum = ringSize - 1
    val leftNum = if(oddNode) otherNodesNum / 2 else (otherNodesNum + 1) / 2
    var leftNow: Node = this
    val leftNodesId = for(i <- 0 until leftNum) yield {
      leftNow = leftNow.left
      require(leftNow != null)
      leftNow.nodeId
    }

    val rightNum = otherNodesNum - leftNum
    var rightNow: Node = this
    val rightlNodesId = for(i <- 0 until rightNum) yield {
      rightNow = rightNow.right
      require(rightNow != null)
      rightNow.nodeId
    }
    (leftNodesId, rightlNodesId)
  }

  lazy val tgtHomeF = getPreferHome(true)
  lazy val tgtHomeI = getPreferHome(false)
  private def getPreferHome(full: Boolean): Int = {
    def cond(id: Int): Boolean = {
      val nt = (id >> typeOff) & ((1 << NodeType.width) - 1)
      if(full) nt == NodeType.HF else nt == NodeType.HI
    }

    val leftPos = if(leftNodes.count(cond) != 0) leftNodes.indexOf(leftNodes.filter(cond).head) else Int.MaxValue
    val rightPos = if(rightNodes.count(cond) != 0) rightNodes.indexOf(rightNodes.filter(cond).head) else Int.MaxValue
    require(leftPos != Int.MaxValue || rightPos != Int.MaxValue)
    if(leftPos < rightPos) {
      leftNodes(leftPos)
    } else {
      rightNodes(rightPos)
    }
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

  lazy val routerStr: String = nodeType match {
    case NodeType.RF => "RequestFullRouter"
    case NodeType.RI => "RequestIoRouter"
    case NodeType.HF => "HomeFullFullRouter"
    case NodeType.HI => "HomeIoRouter"
    case NodeType.C => "C2cRouter"
    case NodeType.S => "SubordinateRouter"
    case _ => "PipelineRouter"
  }

  def icnStr(local: Boolean): String = nodeType match {
    case NodeType.RF => if(local) s"rnf_id_${nodeId.toHexString}" else "rnf"
    case NodeType.RI => if(local) s"rni_id_${nodeId.toHexString}" else "rni"
    case NodeType.HF => if(local) s"hnf_id_${nodeId.toHexString}" else "hnf"
    case NodeType.HI => if(local) s"hni_id_${nodeId.toHexString}" else "hni"
    case NodeType.C => if(local) s"c2c_id_${nodeId.toHexString}" else "c2c"
    case NodeType.S => if(local) {
      if(mainMemory) s"mem_sn_id_${nodeId.toHexString}" else s"dcu_sn_id_${nodeId.toHexString}"
    } else {
      "sn"
    }
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

  def getLegalTarget(ring: Seq[Node]): Map[String, Seq[Int]] = {
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
        "DAT" -> ring.filter(n => n.nodeType == NodeType.RF || n.nodeType == NodeType.RI || n.nodeType == NodeType.HF || n.nodeType == NodeType.HI).map(_.nodeId).filterNot(_ == nodeId),
      )
      case _ => Map[String, Seq[Int]]()
    }
  }
}
