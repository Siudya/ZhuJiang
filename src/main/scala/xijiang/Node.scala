package xijiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.router._
import xijiang.router.base.BaseRouter

object NodeType {
  val R: Int = 0 // TODO: R -> RF / RI
  val HF: Int = 1
  val HI: Int = 2
  val C: Int = 3
  val S: Int = 4 // TODO: Add extra req channel for sn
  val P: Int = 5
  def width: Int = log2Ceil(P)
  def min: Int = R
  def max: Int = P
}

case class NodeParam(
  name: String = "",
  nodeType: Int = NodeType.R,
  splitFlit: Boolean = false
)

case class Node(
  suffix: String = "",
  nodeType: Int = NodeType.R,
  csnNode: Boolean = false,
  nodeNetBits: Int = 1,
  nodeNidBits: Int = 4,
  ringSize: Int = 3,
  oddNode: Boolean = false,
  splitFlit: Boolean = false
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
      case NodeType.R => (Module(new RequestRouter(this)(p)), "rn")
      case NodeType.HF => (Module(new HomeRouter(this)(p)), "hnf")
      case NodeType.HI => (Module(new HomeRouter(this)(p)), "hni")
      case NodeType.C => (Module(new ChipToChipRouter(this)(p)), "c2c")
      case NodeType.S => (Module(new SubordinateRouter(this)(p)), "sn")
      case _ => (Module(new PipelineRouter(this)(p)), "pip")
    }
    res.suggestName(s"$csnStr$nodeStr$suffix")
    res
  }
}
