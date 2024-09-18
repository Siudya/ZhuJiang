package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.tfb.{FlitMonitor, NodeRegister}
import xijiang.{Node, NodeType}
import zhujiang.chi._
import zhujiang.{ZJBundle, ZJModule, ZJParametersKey}

import scala.collection.mutable

class ChannelBundle[T <: Flit](gen: T)(implicit p: Parameters) extends ZJBundle {
  val flit = Valid(gen)
  val rsvd = Valid(UInt(niw.W))
}

class RingSide(local: Boolean)(implicit p: Parameters) extends ZJBundle {
  val req = new ChannelBundle(new ReqFlit)
  val resp = new ChannelBundle(new RespFlit)
  val data = new ChannelBundle(new DataFlit)
  val snoop = new ChannelBundle(new SnoopFlit)
  val ereq = if(local) Some(new ChannelBundle(new ReqFlit)) else None

  def getBundle(chn: String) = {
    chn match {
      case "REQ" => req
      case "RSP" => resp
      case "DAT" => data
      case "SNP" => snoop
      case "ERQ" => ereq.get
    }
  }
}

class RouterRingIO(csn: Boolean)(implicit p: Parameters) extends ZJBundle {
  val tx = Output(new RingSide(csn))
  val rx = Input(new RingSide(csn))
}

trait BaseRouterUtils {
  m: ZJModule =>
  def node: Node
  override val desiredName = node.routerStr

  val local = !node.csnNode
  val c2c = node.csnNode && node.nodeType == NodeType.C
  private val csnRing = m.p(ZJParametersKey).csnRing
  private val csnC2cNum = csnRing.count(_.nodeType == NodeType.C)

  val router = IO(new Bundle {
    val rings = Vec(2, new RouterRingIO(local))
    val chip = Input(UInt(p(ZJParametersKey).chipAddrBits.W))
    val nodeId = Output(UInt(niw.W))
    val remoteChipIds = if(!local && !c2c && node.nodeType != NodeType.P) Some(Input(Vec(csnC2cNum, UInt(chipAddrBits.W)))) else None
    val localChip = if(c2c) Some(Input(UInt(chipAddrBits.W))) else None
  })
  val icn = IO(new IcnBundle(node))

  val nid = if(local) {
    node.nodeId.U(niw.W)
  } else if(c2c) {
    val mask = Cat(Fill(niw - chipAddrBits, true.B), Fill(chipAddrBits, false.B))
    (node.nodeId.U(niw.W) & mask) | router.chip
  } else {
    node.nodeId.U(niw.W) | router.chip
  }

  private val flitMap = Map[String, Flit](
    "REQ" -> new ReqFlit,
    "RSP" -> new RespFlit,
    "DAT" -> new DataFlit,
    "SNP" -> new SnoopFlit,
    "ERQ" -> new ReqFlit
  )
  val injectsMap = mutable.Map[String, DecoupledIO[Flit]]()

  private val ejectBufSizeMap = Map[String, Int](
    "REQ" -> p(ZJParametersKey).reqEjectBufDepth,
    "RSP" -> 0,
    "DAT" -> 0,
    "SNP" -> p(ZJParametersKey).snoopEjectBufDepth,
    "ERQ" -> p(ZJParametersKey).reqEjectBufDepth
  )

  def connectRing[K <: Flit](chn: String): Unit = {
    val icnRx = icn.rx.getBundle(chn)
    val icnTx = icn.tx.getBundle(chn)
    val tap = if(icnRx.isDefined || icnTx.isDefined) Some(Module(new ChannelTap(flitMap(chn), chn, ejectBufSizeMap(chn), c2c))) else None
    if(tap.isDefined) {
      tap.get.suggestName(s"${chn.toLowerCase()}ChannelTap")
      tap.get.io.nid := nid
      tap.get.io.rx(0) := router.rings(0).rx.getBundle(chn)
      tap.get.io.rx(1) := router.rings(1).rx.getBundle(chn)
      router.rings(0).tx.getBundle(chn) := tap.get.io.tx(0)
      router.rings(1).tx.getBundle(chn) := tap.get.io.tx(1)
      tap.get.io.inject.valid := false.B
      tap.get.io.inject.bits := DontCare
      tap.get.io.eject.ready := false.B
      tap.get.io.injectTapSelOH := DontCare
    } else {
      router.rings.foreach(r => {
        r.tx.getBundle(chn).flit := Pipe(r.rx.getBundle(chn).flit)
        r.tx.getBundle(chn).rsvd := Pipe(r.rx.getBundle(chn).rsvd)
      })
    }

    if(icnRx.isDefined) {
      val buf = Module(new Queue(flitMap(chn), 2))
      injectsMap.addOne(chn -> buf.io.enq)
      val mon = if(hasTfb) Some(Module(new FlitMonitor)) else None
      buf.suggestName(s"inject${chn.toLowerCase().capitalize}Buffer")
      buf.io.enq.valid := icnRx.get.valid
      buf.io.enq.bits := icnRx.get.bits.asTypeOf(flitMap(chn))
      icnRx.get.ready := buf.io.enq.ready
      tap.get.io.inject <> buf.io.deq

      def checkTarget(n: Node): Bool = {
        if(local) {
          n.nodeId.U === buf.io.deq.bits.tgt
        } else if(c2c) {
          (n.nodeId.U | router.localChip.get) === buf.io.deq.bits.tgt
        } else {
          n.nodeId.U === Cat(
            1.U(nodeNetBits.W),
            NodeType.C.U(nodeTypeBits.W),
            buf.io.deq.bits.tgt(chipAddrBits - 1, 0).asTypeOf(UInt(nodeNidBits.W))
          )
        }
      }

      tap.get.io.injectTapSelOH(0) := node.rightNodes.map(checkTarget).reduce(_ || _)
      tap.get.io.injectTapSelOH(1) := node.leftNodes.map(checkTarget).reduce(_ || _)
      when(tap.get.io.inject.valid) {
        assert(PopCount(tap.get.io.injectTapSelOH) === 1.U, cf"Unknown routing path on $chn of node 0x${node.nodeId.toHexString}, flit tgt: 0x${buf.io.deq.bits.tgt}%x")
      }
      val tgt = WireInit(buf.io.deq.bits.tgt.asTypeOf(new NodeIdBundle))
      if(!local && !c2c) {
        if(csnC2cNum == 1) {
          tgt.nodeCsnChip := router.remoteChipIds.get.head
        } else {
          tgt.nodeCsnChip := router.remoteChipIds.get(buf.io.deq.bits.tgt(log2Ceil(csnC2cNum) - 1, 0))
        }
        tap.get.io.inject.bits.tgt := tgt.asUInt
      }
      mon.foreach(m => {
        m.suggestName(s"inject${chn.toLowerCase().capitalize}Monitor")
        m.io.clock := clock
        m.io.valid := tap.get.io.inject.fire
        m.io.nodeId := nid
        m.io.inject := true.B
        m.io.flitType := ChannelEncodings.encodingsMap(chn).U
        val flit = WireInit(tap.get.io.inject.bits.asTypeOf(tap.get.gen))
        if(!c2c) flit.src := nid
        m.io.flit := flit.asUInt
        when(m.io.valid) {
          assert(!m.io.fault, s"channel $chn inject wrong flit!")
        }
      })
      if(m.p(ZJParametersKey).tfsParams.isEmpty) {
        val ring = if(local) m.p(ZJParametersKey).localRing else m.p(ZJParametersKey).csnRing
        node.checkLegalStaticInjectTarget(ring, chn, buf.io.deq.bits.tgt.asTypeOf(new NodeIdBundle), buf.io.deq.valid, nid, router.localChip)
      }
    }

    if(icnTx.isDefined) {
      val buf = Module(new Queue(flitMap(chn), 1, pipe = true))
      val mon = if(hasTfb) Some(Module(new FlitMonitor)) else None
      buf.suggestName(s"eject${chn.toLowerCase().capitalize}Buffer")
      icnTx.get.valid := buf.io.deq.valid
      icnTx.get.bits := buf.io.deq.bits.asTypeOf(icnTx.get.bits)
      buf.io.deq.ready := icnTx.get.ready
      buf.io.enq <> tap.get.io.eject

      val src = WireInit(tap.get.io.eject.bits.src.asTypeOf(new NodeIdBundle))
      if(!local && !c2c && m.p(ZJParametersKey).tfsParams.isEmpty) {
        val chipSrc = tap.get.io.eject.bits.src.asTypeOf(new NodeIdBundle).nodeCsnChip
        val c2cHits = router.remoteChipIds.get.map(_ === chipSrc)
        when(tap.get.io.eject.valid) {
          assert(PopCount(c2cHits) === 1.U, cf"Invalid eject chip target $chipSrc of node 0x${nid}%x on $chn")
        }
        src.nodeCsnChip := PriorityEncoder(c2cHits)
        buf.io.enq.bits.src := src.asUInt
      }

      mon.foreach(m => {
        m.suggestName(s"eject${chn.toLowerCase().capitalize}Monitor")
        m.io.clock := clock
        m.io.valid := tap.get.io.eject.fire
        m.io.nodeId := nid
        m.io.inject := false.B
        m.io.flitType := ChannelEncodings.encodingsMap(chn).U
        m.io.flit := tap.get.io.eject.bits.asUInt
        when(m.io.valid) {
          assert(!m.io.fault, s"channel $chn ejects wrong flit!")
        }
      })
    }
  }
}

class BaseRouter(val node: Node)(implicit p: Parameters) extends ZJModule
  with BaseRouterUtils {
  router.nodeId := nid
  private val tfbNodeRegister = if((node.injects ++ node.ejects).nonEmpty && hasTfb) Some(Module(new NodeRegister)) else None
  tfbNodeRegister.foreach(_.io.nodeId := nid)

  connectRing("REQ")
  connectRing("RSP")
  connectRing("DAT")
  connectRing("SNP")
  if(local) connectRing("ERQ")
}
