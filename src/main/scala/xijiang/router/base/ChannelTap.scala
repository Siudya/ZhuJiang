package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.ZJModule
import zhujiang.chi.Flit

class SingleChannelTap[T <: Flit](gen: T, channel: String, c2c: Boolean)(implicit p: Parameters) extends ZJModule {
  private val fw = gen.getWidth
  val io = IO(new Bundle {
    val in = Input(Valid(new ChannelPayload(gen)))
    val out = Output(Valid(new ChannelPayload(gen)))
    val inject = Flipped(Decoupled(UInt(fw.W)))
    val eject = Decoupled(new ChannelPayload(gen))
    val nid = Input(UInt(niw.W))
    val liveCntInit = Input(UInt(maxRingSize.W))
  })
  private val modName = if(c2c) {
    s"C2cSingleChannelTap$channel"
  } else {
    s"SingleChannelTap$channel"
  }
  override val desiredName = modName

  private val actualIn = Wire(Valid(new ChannelPayload(gen)))
  private val selIn = io.in.valid && !io.eject.fire
  actualIn.valid := selIn || io.inject.valid
  private val injectFlit = WireInit(io.inject.bits.asTypeOf(gen))
  if(!c2c) injectFlit.src := io.nid

  when(selIn) {
    actualIn.bits.liveCnt := Mux(io.in.bits.liveCnt.orR, io.in.bits.liveCnt - 1.U, 0.U)
  }.otherwise {
    actualIn.bits.liveCnt := io.liveCntInit
  }
  actualIn.bits.flit := Mux(selIn, io.in.bits.flit, injectFlit.asUInt)
  io.inject.ready := !selIn

  io.out := Pipe(actualIn)

  if(c2c) {
    io.eject.valid := Flit.getTgt(io.in.bits.flit)(p)(nodeNidBits - 1, 0) === io.nid(nodeNidBits - 1, 0) && io.in.valid
  } else {
    io.eject.valid := Flit.getTgt(io.in.bits.flit)(p) === io.nid && io.in.valid
  }
  io.eject.bits := io.in.bits
}

class ChannelTap[T <: Flit](
  val gen: T, channel: String, tgtSeq: Seq[Seq[Int]],
  escape: Boolean = false, ejectBuf: Int = 0, c2c: Boolean = false
)(implicit p: Parameters) extends ZJModule {
  private val fw = gen.getWidth
  val io = IO(new Bundle {
    val rx = Input(Vec(tgtSeq.size, Valid(new ChannelPayload(gen))))
    val tx = Output(Vec(tgtSeq.size, Valid(new ChannelPayload(gen))))
    val inject = Flipped(Decoupled(UInt(fw.W)))
    val eject = Decoupled(UInt(fw.W))
    val nid = Input(UInt(niw.W))
    val liveCntInit = Input(UInt(maxRingSize.W))
  })

  private val tgt = Flit.getTgt(io.inject.bits)(p)
  private val injectTapSelOH = if(tgtSeq.size > 1) tgtSeq.map(_.map(_.U === tgt).reduce(_ || _)) else Seq(true.B)
  when(io.inject.valid) {
    assert(PopCount(injectTapSelOH) === 1.U, "Only one side can be picked!")
  }

  private val taps = Seq.fill(tgtSeq.size)(Module(new SingleChannelTap(gen, channel, c2c)))
  private val ejectArb = Module(new RRArbiter(new ChannelPayload(gen), tgtSeq.size))
  for(idx <- taps.indices) {
    taps(idx).io.in := io.rx(idx)
    io.tx(idx) := taps(idx).io.out
    taps(idx).io.inject.valid := io.inject.valid && injectTapSelOH(idx)
    taps(idx).io.inject.bits := io.inject.bits
    taps(idx).io.nid := io.nid
    taps(idx).io.liveCntInit := io.liveCntInit
    ejectArb.io.in(idx) <> taps(idx).io.eject
  }
  io.inject.ready := Mux1H(injectTapSelOH, taps.map(_.io.inject.ready))

  private val tryToEject = Wire(Decoupled(UInt(fw.W)))
  private val ejectBuffer = if(ejectBuf > 0) Some(Module(new Queue(UInt(fw.W), ejectBuf, pipe = true))) else None
  if(ejectBuf > 2) {
    io.eject <> ejectBuffer.get.io.deq
    ejectBuffer.get.io.enq <> tryToEject
  } else {
    io.eject <> tryToEject
  }

  private val escapeBuffer = if(escape) Some(Module(new Queue(UInt(fw.W), 2))) else None
  if(escape) {
    val ebuf = escapeBuffer.get
    val tryEscEnq = ejectArb.io.out.bits.liveCnt === 0.U && (!tryToEject.ready || ebuf.io.deq.valid)
    ebuf.io.enq.valid := ejectArb.io.out.valid && tryEscEnq
    ebuf.io.enq.bits := ejectArb.io.out.bits.flit
    ejectArb.io.out.ready := Mux(tryEscEnq, ebuf.io.enq.ready, tryToEject.ready)
    tryToEject.bits := Mux(ebuf.io.deq.valid, ebuf.io.deq.bits, ejectArb.io.out.bits.flit)
    tryToEject.valid := ebuf.io.deq.valid || ejectArb.io.out.valid
    ebuf.io.deq.ready := tryToEject.ready
  } else {
    tryToEject.valid := ejectArb.io.out.valid
    tryToEject.bits := ejectArb.io.out.bits.flit
    ejectArb.io.out.ready := tryToEject.ready
  }
}