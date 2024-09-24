package zhujiang

import chisel3._
import org.chipsalliance.cde.config.{Field, Parameters}
import xijiang.c2c.C2cParams
import xijiang.tfb.TrafficBoardParams
import xijiang.tfs.TrafficSimParams
import xijiang.{Node, NodeParam, NodeType}

case object ZJParametersKey extends Field[ZJParameters]

case class ZJParameters(
  ringId: Int = 0,
  modulePrefix: String = "",
  chipAddrBits: Int = 3,
  nodeNidBits: Int = 5,
  dataBits: Int = 256,
  M: Int = 0,
  PB: Int = 0,
  E: Int = 0,
  R: Int = 0,
  S: Int = 0,
  Y: Int = 0,
  DC: Boolean = false,
  P: Boolean = false,
  snoopEjectBufDepth: Int = 8,
  reqEjectBufDepth: Int = 8,
  localNodeParams: Seq[NodeParam] = Seq(),
  csnNodeParams: Seq[NodeParam] = Seq(),
  c2cParams: C2cParams = C2cParams(),
  tfbParams: Option[TrafficBoardParams] = Some(TrafficBoardParams()),
  tfsParams: Option[TrafficSimParams] = None,
  injectRsvdTimerShift: Int = 8
) {
  lazy val requestAddrBits = 48
  lazy val snoopAddrBits = requestAddrBits - 3

  lazy val nodeTypeBits = NodeType.width
  lazy val nodeNetBits = 1
  lazy val nodeIdBits: Int = nodeNetBits + nodeTypeBits + nodeNidBits
  lazy val beBits: Int = dataBits / 8
  lazy val dataCheckBits: Int = if(DC) dataBits / 8 else 0
  lazy val poisonBits: Int = if(P) dataBits / 64 else 0
  private lazy val dwb = if(dataBits == 128) {
    234
  } else if(dataBits == 256) {
    383
  } else {
    681
  }
  lazy val reqFlitBits = 3 * (nodeIdBits - 7) + 88 + requestAddrBits + Y + M + PB + S + E.max(R)
  lazy val respFlitBits = 2 * (nodeIdBits - 7) + 65
  lazy val snoopFlitBits = 3 * (nodeIdBits - 7) + 59 + snoopAddrBits + M + E
  lazy val dataFlitBits = 3 * (nodeIdBits - 7) + dwb + Y + dataCheckBits + poisonBits
  lazy val maxFlitBits = Seq(reqFlitBits, respFlitBits, snoopFlitBits, dataFlitBits).max
  require(nodeIdBits >= 7 && nodeIdBits <= 11)

  lazy val localRing: Seq[Node] = if(localNodeParams.nonEmpty) getRing(localNodeParams, false) else Seq()
  lazy val csnRing: Seq[Node] = if(csnNodeParams.nonEmpty) getRing(csnNodeParams, true) else Seq()

  private def getRing(nodeParams: Seq[NodeParam], csn: Boolean): Seq[Node] = {
    require(nodeParams.size >= 3)
    var rfId = 0
    var riId = 0
    var hfId = 0
    var hiId = 0
    var cId = 0
    var sId = 0
    var pId = 0
    val shmt = if(csn) chipAddrBits else 0
    val nodes = for((np, idx) <- nodeParams.zipWithIndex) yield {
      val n = Node(
        suffix = np.name,
        nodeType = np.nodeType,
        csnNode = csn,
        nodeNetBits = nodeNetBits,
        nodeNidBits = nodeNidBits,
        ringSize = nodeParams.size,
        oddNode = idx % 2 == 1,
        splitFlit = np.splitFlit,
        mainMemory = np.mainMemory
      )
      if(csn) require(n.nodeType == NodeType.RF || n.nodeType == NodeType.HF || n.nodeType == NodeType.C || n.nodeType == NodeType.P)
      n.nodeType match {
        case NodeType.RF => n.nid = rfId << shmt; rfId = rfId + 1
        case NodeType.RI => n.nid = riId; riId = riId + 1
        case NodeType.HF => n.nid = hfId << shmt; hfId = hfId + 1
        case NodeType.HI => n.nid = hiId; hiId = hiId + 1
        case NodeType.C => n.nid = cId; cId = cId + 1
        case NodeType.S => n.nid = sId; sId = sId + 1
        case NodeType.P => n.nid = pId << shmt; pId = pId + 1
      }
      n
    }
    for((n, idx) <- nodes.zipWithIndex) {
      n.left = if(idx == 0) nodes.last else nodes(idx - 1)
      n.right = if(idx == nodes.size - 1) nodes.head else nodes(idx + 1)
    }
    nodes
  }
}

trait HasZJParams {
  implicit val p: Parameters
  val zjParams = p(ZJParametersKey)
  lazy val M = zjParams.M
  lazy val PB = zjParams.PB
  lazy val E = zjParams.E
  lazy val R = zjParams.R
  lazy val S = zjParams.S
  lazy val Y = zjParams.Y
  lazy val raw = zjParams.requestAddrBits
  lazy val saw = zjParams.snoopAddrBits
  lazy val niw = zjParams.nodeIdBits
  lazy val dcw = zjParams.dataCheckBits
  lazy val pw = zjParams.poisonBits
  lazy val dw = zjParams.dataBits
  lazy val bew = zjParams.beBits
  lazy val chipAddrBits = zjParams.chipAddrBits
  lazy val reqFlitBits = zjParams.reqFlitBits
  lazy val respFlitBits = zjParams.respFlitBits
  lazy val snoopFlitBits = zjParams.snoopFlitBits
  lazy val dataFlitBits = zjParams.dataFlitBits
  lazy val maxFlitBits = zjParams.maxFlitBits
  lazy val nodeNidBits = zjParams.nodeNidBits
  lazy val nodeTypeBits = zjParams.nodeTypeBits
  lazy val nodeNetBits = zjParams.nodeNetBits
  lazy val hasTfb = zjParams.tfbParams.isDefined
  lazy val csnNidBits = nodeNidBits - chipAddrBits
}

class ZJBundle(implicit val p: Parameters) extends Bundle with HasZJParams

class ZJModule(implicit val p: Parameters) extends Module with HasZJParams

class ZJRawModule(implicit val p: Parameters) extends RawModule with HasZJParams