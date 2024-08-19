package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.tfb.{FlitMonitor, NodeRegister}
import xijiang.{Node, NodeType}
import zhujiang.chi._
import zhujiang.{ZJBundle, ZJModule, ZJParametersKey}

import scala.collection.mutable

class ChannelPayload[T <: Flit](gen: T)(implicit p: Parameters) extends ZJBundle {
  val flit = UInt(gen.getWidth.W)
  val liveCnt = UInt(maxRingSize.W)
}

class RingSide(implicit p: Parameters) extends ZJBundle {
  val req = Valid(new ChannelPayload(new ReqFlit))
  val resp = Valid(new ChannelPayload(new RespFlit))
  val data = Valid(new ChannelPayload(new DataFlit))
  val snoop = Valid(new ChannelPayload(new SnoopFlit))
}

class RouterRingIO(implicit p: Parameters) extends ZJBundle {
  val tx = Output(new RingSide)
  val rx = Input(new RingSide)
}

class BaseRouter(val node: Node, ejects: Seq[String], injects: Seq[String])(implicit p: Parameters) extends ZJModule {
  val ringNum = if(node.csnNode) 1 else 2
  val router = IO(new Bundle {
    val rings = Vec(ringNum, new RouterRingIO)
    val chip = Input(UInt(p(ZJParametersKey).chipAddrBits.W))
    val nodeId = Output(UInt(niw.W))
  })

  private val c2c = node.csnNode && node.nodeType == NodeType.C
  private val liveCntInit = if(node.csnNode) (csnRingSize - 1).U else (localRingSize - 1).U
  private val nid = if(node.csnNode) {
    genNodeId(1.U(1.W), (node.nodeType % 4).U(nodeTypeBits.W), router.chip)
  } else {
    node.nodeId
  }
  router.nodeId := nid
  private val ejectReq = ejects.contains("REQ")
  private val ejectReqBuf = if(ejectReq) p(ZJParametersKey).reqEjectBufDepth else 0
  private val ejectSnoop = ejects.contains("SNP")
  private val ejectSnoopBuf = if(ejectSnoop) p(ZJParametersKey).snoopEjectBufDepth else 0

  private val dispatchIds = if(node.csnNode) Seq(Seq(0)) else Seq(node.leftNodes, node.rightNodes)
  private val reqTap = if((injects ++ ejects).contains("REQ")) Some(Module(new ChannelTap(new ReqFlit, "REQ", dispatchIds, ejectReq, ejectReqBuf, c2c = c2c))) else None
  private val respTap = if((injects ++ ejects).contains("RSP")) Some(Module(new ChannelTap(new RespFlit, "RSP", dispatchIds, c2c = c2c))) else None
  private val dataTap = if((injects ++ ejects).contains("DAT")) Some(Module(new ChannelTap(new DataFlit, "DAT", dispatchIds, c2c = c2c))) else None
  private val snoopTap = if((injects ++ ejects).contains("SNP")) Some(Module(new ChannelTap(new SnoopFlit, "SNP", dispatchIds, ejectSnoop, ejectSnoopBuf, c2c = c2c))) else None
  private val tfbNodeRegister = if((injects ++ ejects).nonEmpty && hasTfb) Some(Module(new NodeRegister)) else None
  tfbNodeRegister.foreach(_.io.nodeId := nid)

  private def connectRing[K <: Flit](tap: Option[ChannelTap[K]], tx: Valid[ChannelPayload[K]], rx: Valid[ChannelPayload[K]], ridx: Int): Unit = {
    if(tap.isDefined) {
      tap.get.io.nid := nid
      tap.get.io.liveCntInit := liveCntInit
      tap.get.io.rx(ridx) := rx
      tx := tap.get.io.tx(ridx)
      tap.get.io.eject.ready := false.B
      tap.get.io.inject.valid := false.B
      tap.get.io.inject.bits := DontCare
    } else {
      tx.valid := RegNext(rx.valid, false.B)
      tx.bits.flit := RegEnable(rx.bits.flit, rx.valid)
      tx.bits.liveCnt := RegEnable(Mux(rx.bits.liveCnt.orR, rx.bits.liveCnt - 1.U, 0.U), rx.valid)
    }
  }

  for(ridx <- 0 until ringNum) {
    connectRing(reqTap, router.rings(ridx).tx.req, router.rings(ridx).rx.req, ridx)
    connectRing(respTap, router.rings(ridx).tx.resp, router.rings(ridx).rx.resp, ridx)
    connectRing(dataTap, router.rings(ridx).tx.data, router.rings(ridx).rx.data, ridx)
    connectRing(snoopTap, router.rings(ridx).tx.snoop, router.rings(ridx).rx.snoop, ridx)
  }

  val injectMap = mutable.Map[String, DecoupledIO[Data]]()
  val ejectMap = mutable.Map[String, DecoupledIO[Data]]()

  private val flitBitsMap = Map[String, Int](
    "REQ" -> reqFlitBits,
    "RSP" -> respFlitBits,
    "DAT" -> dataFlitBits,
    "SNP" -> snoopFlitBits
  )

  private def registerInjectTap[K <: Flit](chn: String, tap: Option[ChannelTap[K]]): Unit = {
    if(injects.contains(chn)) {
      val buf = Module(new Queue(UInt(flitBitsMap(chn).W), 2))
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

  private def registerEjectTap[K <: Flit](chn: String, tap: Option[ChannelTap[K]]): Unit = {
    if(ejects.contains(chn)) {
      val buf = Module(new Queue(UInt(flitBitsMap(chn).W), 1, pipe = true))
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
        m.io.flit := tap.get.io.eject.bits
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
}
