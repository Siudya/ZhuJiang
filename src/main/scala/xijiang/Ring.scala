package xijiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.c2c.{C2cLinkPort, C2cPackLayer}
import xijiang.router._
import zhujiang.{ZJBundle, ZJModule, ZJParametersKey}
import xijiang.tfs._

object RingIO {
  def apply(local: Boolean, p: Parameters): (Seq[RnIcn], Seq[HnfIcn], Seq[HniIcn], Seq[SnIcn], Seq[C2cLinkPort], UInt) = {
    val ring = if(local) p(ZJParametersKey).localRing else p(ZJParametersKey).csnRing
    val rns = ring.filter(_.nodeType == NodeType.R)
    val hfs = ring.filter(_.nodeType == NodeType.HF)
    val his = ring.filter(_.nodeType == NodeType.HI)
    val c2cs = ring.filter(_.nodeType == NodeType.C)
    val sns = ring.filter(_.nodeType == NodeType.S)
    val rn = rns.map(n => {
      val icn = IO(new RnIcn(n)(p))
      val pfxStr = if(n.dmaPort) "dma_" else "cpu_"
      if(local) icn.suggestName(s"${pfxStr}rn_id_${n.nodeId.toHexString}")
      icn
    })
    val hnf = hfs.map(n => {
      val icn = IO(new HnfIcn(local, n)(p))
      if(local) icn.suggestName(s"hnf_id_${n.nodeId.toHexString}")
      icn
    })
    val hni = his.map(n => {
      val icn = IO(new HniIcn(local, n)(p))
      if(local) icn.suggestName(s"hni_id_${n.nodeId.toHexString}")
      icn
    })
    val sn = sns.map(n => {
      val icn = IO(new SnIcn(n)(p))
      val pfxStr = if(n.mainMemory) "mem_" else "dcu_"
      if(local) icn.suggestName(s"${pfxStr}sn_id_${n.nodeId.toHexString}")
      icn
    })
    val c2c = Seq.fill(c2cs.size)(IO(new C2cLinkPort()(p)))
    val chip = IO(Input(UInt(p(ZJParametersKey).chipAddrBits.W)))
    (rn, hnf, hni, sn, c2c, chip)
  }
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
  private val icn = if(!tfs) Some(RingIO(local, p)) else None
  val tfsio = if(tfs) Some(IO(new TfsIO(local))) else None
  val icnRns: Option[Seq[RnIcn]] = icn.map(_._1)
  val icnHfs: Option[Seq[HnfIcn]] = icn.map(_._2)
  val icnHis: Option[Seq[HniIcn]] = icn.map(_._3)
  val icnSns: Option[Seq[SnIcn]] = icn.map(_._4)
  val icnC2cs: Option[Seq[C2cLinkPort]] = icn.map(_._5)
  val ioChip: Option[UInt] = icn.map(_._6)

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
      r.router.chip := ioChip.get
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
      icnRns.get(idx) <> rn.icn
      dontTouch(icnRns.get(idx))
    }
  })

  hnfs.zipWithIndex.foreach({ case ((r, n), idx) =>
    val hnf = r.asInstanceOf[HomeFullRouter]
    if(tfs) {
      val m = Module(new HnfTrafficGen(local, n))
      m.io.rx <> hnf.icn.tx
      hnf.icn.rx <> m.io.tx
      m.io.nodeId := hnf.router.nodeId
      m.suggestName(s"hnfTxGen_$idx")
    } else {
      icnHfs.get(idx) <> hnf.icn
      dontTouch(icnHfs.get(idx))
    }
  })

  hnis.zipWithIndex.foreach({ case ((r, n), idx) =>
    val hni = r.asInstanceOf[HomeIoRouter]
    if(tfs) {
      val m = Module(new HniTrafficGen(local, n))
      m.io.rx <> hni.icn.tx
      hni.icn.rx <> m.io.tx
      m.io.nodeId := hni.router.nodeId
      m.suggestName(s"hniTxGen_$idx")
    } else {
      icnHis.get(idx) <> hni.icn
      dontTouch(icnHis.get(idx))
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
      icnSns.get(idx) <> sn.icn
      dontTouch(icnSns.get(idx))
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
      icnC2cs.get(idx) <> c2cPacker.io.c2c
      dontTouch(icnC2cs.get(idx))
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
