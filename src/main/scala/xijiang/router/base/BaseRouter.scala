package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.tfb.{FlitMonitor, NodeRegister}
import xijiang.{Node, NodeType}
import zhujiang.chi._
import zhujiang.{ZJBundle, ZJModule, ZJParametersKey}

import scala.collection.mutable

class BaseIcnBundle(val node: Node)(implicit p: Parameters) extends ZJBundle

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

class BaseRouter(val node: Node, ejects: Seq[String], injects: Seq[String])(implicit p: Parameters) extends ZJModule {
  val ringNum = if(node.csnNode) 1 else 2
  private val local = !node.csnNode
  val router = IO(new Bundle {
    val rings = Vec(ringNum, new RouterRingIO(local))
    val chip = Input(UInt(p(ZJParametersKey).chipAddrBits.W))
    val nodeId = Output(UInt(niw.W))
  })

  private val c2c = node.csnNode && node.nodeType == NodeType.C
  private val nid = if(node.csnNode) {
    genNodeId(1.U(1.W), (node.nodeType % 4).U(nodeTypeBits.W), router.chip)
  } else {
    node.nodeId.U(niw.W)
  }
  router.nodeId := nid
  private val ejectReqBuf = p(ZJParametersKey).reqEjectBufDepth
  private val ejectSnoopBuf = p(ZJParametersKey).snoopEjectBufDepth

  private val reqTap = if((injects ++ ejects).contains("REQ")) Some(Module(new ChannelTap(new ReqFlit, "REQ", ringNum, ejectReqBuf, c2c = c2c))) else None
  private val respTap = if((injects ++ ejects).contains("RSP")) Some(Module(new ChannelTap(new RespFlit, "RSP", ringNum, 0, c2c))) else None
  private val dataTap = if((injects ++ ejects).contains("DAT")) Some(Module(new ChannelTap(new DataFlit, "DAT", ringNum, 0, c2c))) else None
  private val snoopTap = if((injects ++ ejects).contains("SNP")) Some(Module(new ChannelTap(new SnoopFlit, "SNP", ringNum, ejectSnoopBuf, c2c = c2c))) else None
  private val ereqTap = if((injects ++ ejects).contains("ERQ") && local) Some(Module(new ChannelTap(new ReqFlit, "REQ", ringNum, ejectReqBuf, c2c = c2c))) else None
  private val tfbNodeRegister = if((injects ++ ejects).nonEmpty && hasTfb) Some(Module(new NodeRegister)) else None
  tfbNodeRegister.foreach(_.io.nodeId := nid)

  private def connectRing[K <: Flit](tap: Option[ChannelTap[K]], tx: ChannelBundle[K], rx: ChannelBundle[K], ridx: Int): Unit = {
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

  for(ridx <- 0 until ringNum) {
    connectRing(reqTap, router.rings(ridx).tx.req, router.rings(ridx).rx.req, ridx)
    connectRing(respTap, router.rings(ridx).tx.resp, router.rings(ridx).rx.resp, ridx)
    connectRing(dataTap, router.rings(ridx).tx.data, router.rings(ridx).rx.data, ridx)
    connectRing(snoopTap, router.rings(ridx).tx.snoop, router.rings(ridx).rx.snoop, ridx)
    if(local) connectRing(ereqTap, router.rings(ridx).tx.ereq.get, router.rings(ridx).rx.ereq.get, ridx)
  }

  val injectMap = mutable.Map[String, DecoupledIO[Flit]]()
  val ejectMap = mutable.Map[String, DecoupledIO[Flit]]()

  private val flitMap = Map[String, Flit](
    "REQ" -> new ReqFlit,
    "RSP" -> new RespFlit,
    "DAT" -> new DataFlit,
    "SNP" -> new SnoopFlit,
    "ERQ" -> new ReqFlit
  )

  private def registerInjectTap[K <: Flit](chn: String, tap: Option[ChannelTap[K]]): Unit = {
    if(injects.contains(chn)) {
      val buf = Module(new Queue(flitMap(chn), 2))
      val mon = if(hasTfb) Some(Module(new FlitMonitor)) else None
      buf.suggestName(s"inject${chn.toLowerCase().capitalize}Buffer")
      injectMap(chn) = buf.io.enq
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
    }
  }
  registerInjectTap("REQ", reqTap)
  registerInjectTap("RSP", respTap)
  registerInjectTap("DAT", dataTap)
  registerInjectTap("SNP", snoopTap)
  if(local) registerInjectTap("ERQ", ereqTap)

  private def registerEjectTap[K <: Flit](chn: String, tap: Option[ChannelTap[K]]): Unit = {
    if(ejects.contains(chn)) {
      val buf = Module(new Queue(flitMap(chn), 1, pipe = true))
      val mon = if(hasTfb) Some(Module(new FlitMonitor)) else None
      buf.suggestName(s"eject${chn.toLowerCase().capitalize}Buffer")
      ejectMap(chn) = buf.io.deq
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
  registerEjectTap("REQ", reqTap)
  registerEjectTap("RSP", respTap)
  registerEjectTap("DAT", dataTap)
  registerEjectTap("SNP", snoopTap)
  if(local) registerEjectTap("ERQ", ereqTap)

  def connInject(io: DecoupledIO[Data], chn: String): Unit = {
    injectMap(chn).valid := io.valid
    injectMap(chn).bits := io.bits.asTypeOf(injectMap(chn).bits)
    io.ready := injectMap(chn).ready
  }
  def connEject(io: DecoupledIO[Data], chn: String): Unit = {
    io.valid := ejectMap(chn).valid
    io.bits := ejectMap(chn).bits.asTypeOf(io.bits)
    ejectMap(chn).ready := io.ready
  }
}
