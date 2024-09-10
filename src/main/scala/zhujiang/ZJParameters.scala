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
  nodeNidBits: Int = 4,
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
  val requestAddrBits = 48
  val snoopAddrBits = requestAddrBits - 3

  val nodeTypeBits = NodeType.width
  val nodeNetBits = 1
  val nodeIdBits: Int = nodeNetBits + nodeTypeBits + nodeNidBits
  val beBits: Int = dataBits / 8
  val dataCheckBits: Int = if(DC) dataBits / 8 else 0
  val poisonBits: Int = if(P) dataBits / 64 else 0
  private val dwb = if(dataBits == 128) {
    234
  } else if(dataBits == 256) {
    383
  } else {
    681
  }
  val reqFlitBits = 3 * (nodeIdBits - 7) + 88 + requestAddrBits + Y + M + PB + S + E.max(R)
  val respFlitBits = 2 * (nodeIdBits - 7) + 65
  val snoopFlitBits = 3 * (nodeIdBits - 7) + 59 + snoopAddrBits + M + E
  val dataFlitBits = 3 * (nodeIdBits - 7) + dwb + Y + dataCheckBits + poisonBits
  val maxFlitBits = Seq(reqFlitBits, respFlitBits, snoopFlitBits, dataFlitBits).max
  require(nodeIdBits >= 7 && nodeIdBits <= 11)

  lazy val localRing: Seq[Node] = if(localNodeParams.nonEmpty) getRing(localNodeParams, false) else Seq()
  lazy val csnRing: Seq[Node] = if(csnNodeParams.nonEmpty) getRing(csnNodeParams, true) else Seq()

  private def getRing(nodeParams: Seq[NodeParam], csn: Boolean): Seq[Node] = {
    require(nodeParams.size >= 3)
    var rId = 0
    var hfId = 0
    var hiId = 0
    var cId = 0
    var sId = 0
    var pId = 0
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
        mainMemory = np.mainMemory,
        dmaPort = np.dmaPort
      )
      n.nodeType match {
        case NodeType.R => n.nid = rId; rId = rId + 1
        case NodeType.HF => n.nid = hfId; hfId = hfId + 1
        case NodeType.HI => n.nid = hiId; hiId = hiId + 1
        case NodeType.C => n.nid = cId; cId = cId + 1
        case NodeType.S => n.nid = sId; sId = sId + 1
        case NodeType.P => n.nid = pId; pId = pId + 1
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
  val M = p(ZJParametersKey).M
  val PB = p(ZJParametersKey).PB
  val E = p(ZJParametersKey).E
  val R = p(ZJParametersKey).R
  val S = p(ZJParametersKey).S
  val Y = p(ZJParametersKey).Y
  val raw = p(ZJParametersKey).requestAddrBits
  val saw = p(ZJParametersKey).snoopAddrBits
  val niw = p(ZJParametersKey).nodeIdBits
  val dcw = p(ZJParametersKey).dataCheckBits
  val pw = p(ZJParametersKey).poisonBits
  val dw = p(ZJParametersKey).dataBits
  val bew = p(ZJParametersKey).beBits
  val chipAddrBits = p(ZJParametersKey).chipAddrBits
  val reqFlitBits = p(ZJParametersKey).reqFlitBits
  val respFlitBits = p(ZJParametersKey).respFlitBits
  val snoopFlitBits = p(ZJParametersKey).snoopFlitBits
  val dataFlitBits = p(ZJParametersKey).dataFlitBits
  val maxFlitBits = p(ZJParametersKey).maxFlitBits
  val nodeNidBits = p(ZJParametersKey).nodeNidBits
  val nodeTypeBits = p(ZJParametersKey).nodeTypeBits
  val nodeNetBits = p(ZJParametersKey).nodeNetBits
  val hasTfb = p(ZJParametersKey).tfbParams.isDefined

  def genNodeId(net: UInt, nt: UInt, nid: UInt): UInt = {
    val netOff = nodeNidBits + nodeTypeBits
    val typeOff = nodeNidBits
    ((net << netOff.U).asUInt | (nt << typeOff.U).asUInt | nid.asUInt)(niw - 1, 0)
  }
}

class ZJBundle(implicit val p: Parameters) extends Bundle with HasZJParams

class ZJModule(implicit val p: Parameters) extends Module with HasZJParams

class ZJRawModule(implicit val p: Parameters) extends RawModule with HasZJParams