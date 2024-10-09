package zhujiang.device.cluster

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import xijiang.router.base.{DeviceIcnBundle, IcnBundle}
import zhujiang.chi._
import zhujiang.device.async.{DeviceIcnAsyncBundle, DeviceSideAsyncModule, IcnAsyncBundle}
import zhujiang.{ZJBundle, ZJModule}

class ClusterMiscWires(node: Node)(implicit p: Parameters) extends ZJBundle {
  val msip = Input(Vec(node.cpuNum, Bool()))
  val mtip = Input(Vec(node.cpuNum, Bool()))
  val meip = Input(Vec(node.cpuNum, Bool()))
  val seip = Input(Vec(node.cpuNum, Bool()))
  val dbip = Input(Vec(node.cpuNum, Bool()))
  val reset_vector = Input(Vec(node.cpuNum, UInt(raw.W)))
  val mhartid = Input(Vec(node.cpuNum, UInt(clusterIdBits.W)))
  val halt = Output(Vec(node.cpuNum, Bool()))
  val osc_clock = Input(Clock())
}

class ClusterIcnBundle(node: Node)(implicit p: Parameters) extends ZJBundle {
  val async = new IcnAsyncBundle(node)
  val misc = new ClusterMiscWires(node)
  def <>(that: ClusterHubBundle): Unit = {
    this.async <> that.async
    this.misc <> that.misc
  }
}

class ClusterHubBundle(node: Node)(implicit p: Parameters) extends ZJBundle {
  val async = new DeviceIcnAsyncBundle(node)
  val misc = Flipped(new ClusterMiscWires(node))
  def <>(that: ClusterIcnBundle): Unit = {
    this.async <> that.async
    this.misc <> that.misc
  }
}

class ClusterInternalBundle(node: Node)(implicit p: Parameters) extends ZJBundle {
  val bridge = new IcnBundle(node.copy(nodeType = NodeType.HI))
  val l2cache = new IcnBundle(node.copy(nodeType = NodeType.RF))
  val misc = Flipped(new ClusterMiscWires(node))
}

class ClusterHub(node: Node)(implicit p: Parameters) extends ZJModule {
  require(node.nodeType == NodeType.CC)
  val io = IO(new Bundle {
    val ext = new ClusterHubBundle(node)
    val int = new ClusterInternalBundle(node)
    val peripheral = new IcnBundle(node.copy(nodeType = NodeType.HI))
    val l2cache = new IcnBundle(node.copy(nodeType = NodeType.RF))
  })

  io.int.misc <> io.ext.misc
  private val asyncModule = Module(new DeviceSideAsyncModule(node))
  asyncModule.io.async <> io.ext.async

  private val rxChnMap = node.ejects.map({ chn =>
    val rx = asyncModule.io.icn.rx.getBundle(chn).get
    val pipe = Module(new Queue(rx.bits.cloneType, entries = 2))
    pipe.suggestName(s"rxPipe${chn}")
    pipe.io.enq <> rx
    (chn, pipe.io.deq)
  }).toMap

  private val icnRxReq = rxChnMap("REQ")
  private val icnRxRsp = rxChnMap("RSP")
  private val icnRxDat = rxChnMap("DAT")
  private val icnRxSnp = rxChnMap("SNP")
  private val icnRxRspAid = icnRxRsp.bits.asTypeOf(new RespFlit).TgtID.asTypeOf(new NodeIdBundle).aid
  private val icnRxDatAid = icnRxDat.bits.asTypeOf(new DataFlit).TgtID.asTypeOf(new NodeIdBundle).aid

  io.peripheral.tx.req.get <> icnRxReq
  io.peripheral.tx.resp.get.valid := icnRxRsp.valid && icnRxRspAid === 0.U
  io.peripheral.tx.resp.get.bits := icnRxRsp.bits
  io.peripheral.tx.data.get.valid := icnRxDat.valid && icnRxDatAid === 0.U
  io.peripheral.tx.data.get.bits := icnRxDat.bits

  io.l2cache.tx.snoop.get <> icnRxSnp
  io.l2cache.tx.resp.get.valid := icnRxRsp.valid && icnRxRspAid === 1.U
  io.l2cache.tx.resp.get.bits := icnRxRsp.bits
  io.l2cache.tx.data.get.valid := icnRxDat.valid && icnRxDatAid === 1.U
  io.l2cache.tx.data.get.bits := icnRxDat.bits

  icnRxRsp.ready := MuxCase(false.B, Seq(
    (icnRxRspAid === 0.U, io.peripheral.tx.resp.get.ready),
    (icnRxRspAid === 1.U, io.l2cache.tx.resp.get.ready),
  ))
  icnRxDat.ready := MuxCase(false.B, Seq(
    (icnRxDatAid === 0.U, io.peripheral.tx.data.get.ready),
    (icnRxDatAid === 1.U, io.l2cache.tx.data.get.ready),
  ))

  private val txChnMap = node.injects.map({ chn =>
    val tx = asyncModule.io.icn.tx.getBundle(chn).get
    val pipe = Module(new Queue(tx.bits.cloneType, entries = 2))
    pipe.suggestName(s"txPipe${chn}")
    tx <> pipe.io.deq
    (chn, pipe)
  }).toMap

  private val icnTxReq = txChnMap("REQ").io.enq
  icnTxReq.valid := io.l2cache.rx.req.get.valid
  io.l2cache.rx.req.get.ready := icnTxReq.ready
  private val reqFlit = WireInit(io.l2cache.rx.req.get.bits.asTypeOf(new ReqFlit))
  private val reqSrcId = WireInit(0.U.asTypeOf(new NodeIdBundle))
  reqSrcId.aid := 1.U
  reqFlit.SrcID := reqSrcId.asUInt
  io.l2cache.rx.req.get.bits := reqFlit.asTypeOf(io.l2cache.rx.req.get.bits)

  private val flitMap = Seq(
    ("RSP", new RespFlit),
    ("DAT", new DataFlit),
  ).toMap

  def txConn(chn: String): Unit = {
    val arb = Module(new RRArbiter(flitMap(chn), 2))
    arb.suggestName(s"txArb$chn")
    val peri = io.peripheral.rx.getBundle(chn).get
    val l2c = io.l2cache.rx.getBundle(chn).get
    arb.io.in.zip(Seq(peri, l2c)).zipWithIndex.foreach({ case ((tx, rx), idx) =>
      tx.valid := rx.valid
      rx.ready := tx.ready
      val flit = WireInit(rx.bits.asTypeOf(flitMap(chn)))
      val src = WireInit(0.U.asTypeOf(new NodeIdBundle))
      src.aid := idx.U
      flit.src := src
      tx.bits := flit
    })
  }
  txConn("RSP")
  txConn("DAT")
}
