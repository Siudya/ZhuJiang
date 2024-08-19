package xijiang

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xijiang.c2c.{C2cLinkPort, C2cPackLayer}
import xijiang.router._
import zhujiang.{ZJBundle, ZJModule, ZJParametersKey}
import xijiang.tfs._

class RingIO(local: Boolean)(implicit p: Parameters) extends ZJBundle {
  private val ring = if(local) p(ZJParametersKey).localRing else p(ZJParametersKey).csnRing
  private val rnNum = ring.count(_.nodeType == NodeType.R)
  private val hfNum = ring.count(_.nodeType == NodeType.HF)
  private val hiNum = ring.count(_.nodeType == NodeType.HI)
  private val c2cNum = ring.count(_.nodeType == NodeType.C)
  val rn = Vec(rnNum, new RnIcn)
  val hnf = Vec(hfNum, new HnIcn)
  val hni = Vec(hiNum, new HnIcn)
  val c2c = Vec(c2cNum, new C2cLinkPort)
  val chip = Input(UInt(chipAddrBits.W))
}

class TfsIO(local: Boolean)(implicit p: Parameters) extends ZJBundle {
  private val ring = if(local) p(ZJParametersKey).localRing else p(ZJParametersKey).csnRing
  private val c2cNum = ring.count(_.nodeType == NodeType.C)
  val chip = Input(UInt(chipAddrBits.W))
  val remoteChip = Input(Vec(c2cNum, UInt(chipAddrBits.W)))
}

class Ring(local: Boolean)(implicit p: Parameters) extends ZJModule {
  private val tfs = p(ZJParametersKey).tfsParams.isDefined
  private val ringName = if(local) "LocalRing" else "CsnRing"
  override val desiredName = ringName
  val io = if(!tfs) Some(IO(new RingIO(local))) else None
  val tfsio = if(tfs) Some(IO(new TfsIO(local))) else None

  private val ring = if(local) p(ZJParametersKey).localRing else p(ZJParametersKey).csnRing
  private val routersAndNodes = ring.map(n => (n.genRouter(p), n))
  for(((r, n), i) <- routersAndNodes.zipWithIndex) {
    val left = if(i == 0) routersAndNodes.last else routersAndNodes(i - 1)
    val right = if(i == routersAndNodes.size - 1) routersAndNodes.head else routersAndNodes(i + 1)
    r.router.rings.head.rx := left._1.router.rings.head.tx
    if(tfs) {
      r.router.chip := tfsio.get.chip
    } else {
      r.router.chip := io.get.chip
    }
    if(local) r.router.rings.last.rx := right._1.router.rings.last.tx
  }
  private val rns = routersAndNodes.filter(_._2.nodeType == NodeType.R)
  private val hnfs = routersAndNodes.filter(_._2.nodeType == NodeType.HF)
  private val hnis = routersAndNodes.filter(_._2.nodeType == NodeType.HI)
  private val c2cs = routersAndNodes.filter(_._2.nodeType == NodeType.C)

  rns.zipWithIndex.foreach({ case ((r, _), idx) =>
    val rn = r.asInstanceOf[RequestRouter]
    if(tfs) {
      val m = Module(new RnTrafficGen)
      m.io.rx <> rn.icn.tx
      rn.icn.rx <> m.io.tx
      m.io.nodeId := rn.router.nodeId
      m.suggestName(s"rnTxGen_$idx")
    } else {
      io.get.rn(idx) <> rn.icn
    }
  })

  hnfs.zipWithIndex.foreach({ case ((r, _), idx) =>
    val hnf = r.asInstanceOf[HomeRouter]
    if(tfs) {
      val m = Module(new HnTrafficGen)
      m.io.rx <> hnf.icn.tx
      hnf.icn.rx <> m.io.tx
      m.io.nodeId := hnf.router.nodeId
      m.suggestName(s"hnfTxGen_$idx")
    } else {
      io.get.hnf(idx) <> hnf.icn
    }
  })

  hnis.zipWithIndex.foreach({ case ((r, _), idx) =>
    val hni = r.asInstanceOf[HomeRouter]
    if(tfs) {
      val m = Module(new HnTrafficGen)
      m.io.rx <> hni.icn.tx
      hni.icn.rx <> m.io.tx
      m.io.nodeId := hni.router.nodeId
      m.suggestName(s"hniTxGen_$idx")
    } else {
      io.get.hni(idx) <> hni.icn
    }
  })

  c2cs.zipWithIndex.foreach({ case ((r, _), idx) =>
    val cn = r.asInstanceOf[ChipToChipRouter]
    if(tfs) {
      val m = Module(new CnTrafficGen)
      m.io.rx <> cn.icn.tx
      cn.icn.rx <> m.io.tx
      cn.router.chip := tfsio.get.remoteChip(idx)
      m.io.nodeId := cn.router.nodeId
      m.suggestName(s"cnTxGen_$idx")
    } else {
      val c2cPacker = Module(new C2cPackLayer)
      c2cPacker.io.ring.rx <> cn.icn.tx
      cn.icn.rx <> c2cPacker.io.ring.tx
      cn.router.chip := c2cPacker.io.ring.chip
      c2cPacker.suggestName("c2cPackLayer")
      io.get.c2c(idx) <> c2cPacker.io.c2c
    }
  })

  private val functionalRouters = routersAndNodes.filter(_._2.nodeType != NodeType.P).map(_._1)
  for(i <- functionalRouters.indices) {
    for(j <- (i + 1) until functionalRouters.size) {
      if(local) {
        require(functionalRouters(i).node.nodeId != functionalRouters(j).node.nodeId)
      } else {
        assert(functionalRouters(i).router.nodeId =/= functionalRouters(j).router.nodeId)
      }
    }
  }
}
