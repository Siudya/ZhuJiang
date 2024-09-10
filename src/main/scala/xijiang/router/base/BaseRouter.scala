package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.tfb.{FlitMonitor, NodeRegister}
import xijiang.{Node, NodeType}
import zhujiang.chi._
import zhujiang.{ZJBundle, ZJModule, ZJParametersKey}

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
}

class RouterRingIO(csn: Boolean)(implicit p: Parameters) extends ZJBundle {
  val tx = Output(new RingSide(csn))
  val rx = Input(new RingSide(csn))
}

trait BaseRouterUtils {
  m: ZJModule =>
  def node: Node
  override val desiredName = node.routerStr
  val injects = node.injects
  val ejects = node.ejects

  val local = !node.csnNode
  val ringNum = if(node.csnNode) 1 else 2
  val c2c = node.csnNode && node.nodeType == NodeType.C

  val router = IO(new Bundle {
    val rings = Vec(ringNum, new RouterRingIO(local))
    val chip = Input(UInt(p(ZJParametersKey).chipAddrBits.W))
    val nodeId = Output(UInt(niw.W))
  })
  val icn = IO(new IcnBundle(node))

  val nid = if(node.csnNode) {
    genNodeId(1.U(1.W), node.nodeType.U(nodeTypeBits.W), router.chip)
  } else {
    node.nodeId.U(niw.W)
  }

  private val flitMap = Map[String, Flit](
    "REQ" -> new ReqFlit,
    "RSP" -> new RespFlit,
    "DAT" -> new DataFlit,
    "SNP" -> new SnoopFlit,
    "ERQ" -> new ReqFlit
  )

  def connectRing[K <: Flit](tap: Option[ChannelTap[K]], tx: ChannelBundle[K], rx: ChannelBundle[K], ridx: Int): Unit = {
    if(tap.isDefined) {
      tap.get.io.nid := nid
      tap.get.io.rx(ridx) := rx
      tx := tap.get.io.tx(ridx)
      tap.get.io.eject.ready := false.B
      tap.get.io.inject.valid := false.B
      tap.get.io.inject.bits := DontCare
      if(node.csnNode) {
        tap.get.io.injectTapSelOH.head := true.B
      } else {
        val tgt = tap.get.io.inject.bits.tgt
        tap.get.io.injectTapSelOH(0) := node.rightNodes.map(_.U === tgt).reduce(_ || _)
        tap.get.io.injectTapSelOH(1) := node.leftNodes.map(_.U === tgt).reduce(_ || _)
      }
    } else {
      tx.flit := Pipe(rx.flit)
      tx.rsvd := Pipe(rx.rsvd)
    }
  }

  def registerInjectTap[K <: Flit](chn: String, tap: Option[ChannelTap[K]]): Option[DecoupledIO[Flit]] = {
    val icnChn = icn.rx.getBundle(chn)
    if(icnChn.isDefined) {
      val buf = Module(new Queue(flitMap(chn), 2))
      val mon = if(hasTfb) Some(Module(new FlitMonitor)) else None
      buf.suggestName(s"inject${chn.toLowerCase().capitalize}Buffer")
      buf.io.enq.valid := icnChn.get.valid
      buf.io.enq.bits := icnChn.get.bits.asTypeOf(flitMap(chn))
      icnChn.get.ready := buf.io.enq.ready
      tap.get.io.inject <> buf.io.deq
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
      if(local && m.p(ZJParametersKey).tfsParams.isEmpty) {
        val legalTargets = node.getLegalTarget(m.p(ZJParametersKey).localRing)(chn)
        val legal = legalTargets.map(_.U === tap.get.io.inject.bits.tgt).reduce(_ || _)
        val legalStr = legalTargets.map(i => s"0x${i.toHexString} ").reduce(_ + _)
        when(tap.get.io.inject.fire) {
          assert(legal, cf"Illegal target id 0x${tap.get.io.inject.bits.tgt}%x of $chn flit @ node 0x${node.nodeId.toHexString} legal target_id: $legalStr")
        }
      }
      Some(buf.io.enq)
    } else {
      None
    }
  }

  def registerEjectTap[K <: Flit](chn: String, tap: Option[ChannelTap[K]]): Unit = {
    val icnChn = icn.tx.getBundle(chn)
    if(icnChn.isDefined) {
      val buf = Module(new Queue(flitMap(chn), 1, pipe = true))
      val mon = if(hasTfb) Some(Module(new FlitMonitor)) else None
      buf.suggestName(s"eject${chn.toLowerCase().capitalize}Buffer")
      icnChn.get.valid := buf.io.deq.valid
      icnChn.get.bits := buf.io.deq.bits.asTypeOf(icnChn.get.bits)
      buf.io.deq.ready := icnChn.get.ready
      buf.io.enq <> tap.get.io.eject
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
  private val ejectReqBuf = p(ZJParametersKey).reqEjectBufDepth
  private val ejectSnoopBuf = p(ZJParametersKey).snoopEjectBufDepth

  val reqTap = if((injects ++ ejects).contains("REQ")) Some(Module(new ChannelTap(new ReqFlit, "REQ", ringNum, ejectReqBuf, c2c = c2c))) else None
  private val respTap = if((injects ++ ejects).contains("RSP")) Some(Module(new ChannelTap(new RespFlit, "RSP", ringNum, 0, c2c))) else None
  private val dataTap = if((injects ++ ejects).contains("DAT")) Some(Module(new ChannelTap(new DataFlit, "DAT", ringNum, 0, c2c))) else None
  private val snoopTap = if((injects ++ ejects).contains("SNP")) Some(Module(new ChannelTap(new SnoopFlit, "SNP", ringNum, ejectSnoopBuf, c2c = c2c))) else None
  private val ereqTap = if((injects ++ ejects).contains("ERQ") && local) Some(Module(new ChannelTap(new ReqFlit, "REQ", ringNum, ejectReqBuf, c2c = c2c))) else None
  private val tfbNodeRegister = if((injects ++ ejects).nonEmpty && hasTfb) Some(Module(new NodeRegister)) else None
  tfbNodeRegister.foreach(_.io.nodeId := nid)

  for(ridx <- 0 until ringNum) {
    connectRing(reqTap, router.rings(ridx).tx.req, router.rings(ridx).rx.req, ridx)
    connectRing(respTap, router.rings(ridx).tx.resp, router.rings(ridx).rx.resp, ridx)
    connectRing(dataTap, router.rings(ridx).tx.data, router.rings(ridx).rx.data, ridx)
    connectRing(snoopTap, router.rings(ridx).tx.snoop, router.rings(ridx).rx.snoop, ridx)
    if(local) connectRing(ereqTap, router.rings(ridx).tx.ereq.get, router.rings(ridx).rx.ereq.get, ridx)
  }

  val reqBufEnq = registerInjectTap("REQ", reqTap)
  val respBufEnq = registerInjectTap("RSP", respTap)
  val dataBufEnq = registerInjectTap("DAT", dataTap)
  val snoopBufEnq = registerInjectTap("SNP", snoopTap)
  val ereqBufEnq = registerInjectTap("ERQ", ereqTap)

  registerEjectTap("REQ", reqTap)
  registerEjectTap("RSP", respTap)
  registerEjectTap("DAT", dataTap)
  registerEjectTap("SNP", snoopTap)
  registerEjectTap("ERQ", ereqTap)
}
