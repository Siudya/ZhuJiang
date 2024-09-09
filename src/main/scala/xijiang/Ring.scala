package xijiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.c2c.{C2cLinkPort, C2cPackLayer}
import xijiang.router._
import zhujiang.{ZJBundle, ZJModule, ZJParametersKey}
import xijiang.tfs._

class RingIO(local: Boolean)(implicit p: Parameters) extends ZJBundle {
  private val ring = if(local) p(ZJParametersKey).localRing else p(ZJParametersKey).csnRing
  private val rns = ring.filter(_.nodeType == NodeType.R)
  private val hfs = ring.filter(_.nodeType == NodeType.HF)
  private val his = ring.filter(_.nodeType == NodeType.HI)
  private val c2cs = ring.filter(_.nodeType == NodeType.C)
  private val sns = ring.filter(_.nodeType == NodeType.S)
  val rn = MixedVec(rns.map(r => new RnIcn(r)))
  val hnf = MixedVec(hfs.map(h => new HnIcn(local, h)))
  val hni = MixedVec(his.map(h => new HnIcn(local, h)))
  val c2c = MixedVec(c2cs.map(c => new CnIcn(c)))
  val sn = MixedVec(sns.map(s => new SnIcn(s)))
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
  io.foreach(dontTouch(_))
  tfsio.foreach(dontTouch(_))

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
  private val sns = routersAndNodes.filter(_._2.nodeType == NodeType.S)

  rns.zipWithIndex.foreach({ case ((r, n), idx) =>
    val rn = r.asInstanceOf[RequestRouter]
    if(tfs) {
      val m = Module(new RnTrafficGen(n))
      m.io.rx <> rn.icn.tx
      rn.icn.rx <> m.io.tx
      m.io.nodeId := rn.router.nodeId
      m.suggestName(s"rnTxGen_$idx")
    } else {
      io.get.rn(idx) <> rn.icn
    }
  })

  hnfs.zipWithIndex.foreach({ case ((r, n), idx) =>
    val hnf = r.asInstanceOf[HomeRouter]
    if(tfs) {
      val m = Module(new HnTrafficGen(local, n))
      m.io.rx <> hnf.icn.tx
      hnf.icn.rx <> m.io.tx
      m.io.nodeId := hnf.router.nodeId
      m.suggestName(s"hnfTxGen_$idx")
    } else {
      io.get.hnf(idx) <> hnf.icn
    }
  })

  hnis.zipWithIndex.foreach({ case ((r, n), idx) =>
    val hni = r.asInstanceOf[HomeRouter]
    if(tfs) {
      val m = Module(new HnTrafficGen(local, n))
      m.io.rx <> hni.icn.tx
      hni.icn.rx <> m.io.tx
      m.io.nodeId := hni.router.nodeId
      m.suggestName(s"hniTxGen_$idx")
    } else {
      io.get.hni(idx) <> hni.icn
    }
  })

  c2cs.zipWithIndex.foreach({ case ((r, n), idx) =>
    val cn = r.asInstanceOf[ChipToChipRouter]
    if(tfs) {
      val m = Module(new CnTrafficGen(n))
      m.io.rx <> cn.icn.tx
      cn.icn.rx <> m.io.tx
      cn.router.chip := tfsio.get.remoteChip(idx)
      m.io.nodeId := cn.router.nodeId
      m.suggestName(s"cnTxGen_$idx")
    } else {
      val c2cPacker = Module(new C2cPackLayer(n))
      c2cPacker.io.ring.rx <> cn.icn.tx
      cn.icn.rx <> c2cPacker.io.ring.tx
      cn.router.chip := c2cPacker.io.ring.chip
      c2cPacker.suggestName("c2cPackLayer")
      io.get.c2c(idx) <> c2cPacker.io.c2c
    }
  })

  sns.zipWithIndex.foreach({ case ((r, n), idx) =>
    val sn = r.asInstanceOf[SubordinateRouter]
    if(tfs) {
      val m = Module(new SnTrafficGen(n))
      m.io.rx <> sn.icn.tx
      sn.icn.rx <> m.io.tx
      m.io.nodeId := sn.router.nodeId
      m.suggestName(s"snTxGen_$idx")
    } else {
      io.get.sn(idx) <> sn.icn
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
