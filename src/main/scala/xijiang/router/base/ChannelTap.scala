package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.{ZJModule, ZJParametersKey}
import zhujiang.chi.Flit

class SingleChannelTap[T <: Flit](gen: T, channel: String, c2c: Boolean)(implicit p: Parameters) extends ZJModule {

  private val timerBits = p(ZJParametersKey).injectRsvdTimerShift
  val io = IO(new Bundle {
    val in = Input(new ChannelBundle(gen))
    val out = Output(new ChannelBundle(gen))
    val inject = Flipped(Decoupled(gen))
    val eject = Decoupled(gen)
    val nid = Input(UInt(niw.W))
  })
  private val modName = if(c2c) {
    s"C2cSingleChannelTap$channel"
  } else {
    s"SingleChannelTap$channel"
  }
  override val desiredName = modName
  private val normalShift = 0
  private val injectRsvdShift = 1
  private val waitSlotShift = 2

  private val s_normal = (1 << normalShift).U(3.W)
  private val s_inject_reserved = (1 << injectRsvdShift).U(3.W)
  private val s_wait_slot = (1 << waitSlotShift).U(3.W)
  private val state = RegInit(s_normal)
  private val counter = RegInit(0.U(timerBits.W))
  private val flitNext = Wire(Valid(gen))
  private val rsvdNext = Wire(Valid(UInt(niw.W)))
  private val injectFire = io.inject.fire
  private val ejectFire = io.eject.fire
  private val meetRsvdSlot = io.in.rsvd.bits === io.nid

  when(injectFire) {
    counter := 0.U
  }.elsewhen(io.inject.valid && !io.inject.ready && state(normalShift) && !counter(timerBits - 1)) {
    counter := counter + 1.U
  }

  switch(state) {
    is(s_normal) {
      state := Mux(counter(timerBits - 1), s_inject_reserved, s_normal)
    }
    is(s_inject_reserved) {
      state := Mux(injectFire, s_normal, Mux(io.in.rsvd.valid, s_inject_reserved, s_wait_slot))
    }
    is(s_wait_slot) {
      state := Mux(injectFire, s_normal, s_wait_slot)
    }
  }
  when(io.in.rsvd.valid && meetRsvdSlot) {
    assert(state(waitSlotShift), "Unexpected reserved slot!")
  }

  private val emptySlot = Mux(io.in.flit.valid, ejectFire, true.B)
  private val availableSlot = Mux(io.in.rsvd.valid, meetRsvdSlot, !state(waitSlotShift))
  dontTouch(emptySlot)
  dontTouch(availableSlot)
  io.inject.ready := emptySlot && availableSlot

  private val injectFlit = WireInit(io.inject.bits)
  if(!c2c) injectFlit.src := io.nid
  flitNext.valid := injectFire || io.in.flit.valid && !ejectFire
  flitNext.bits := Mux(injectFire, injectFlit, io.in.flit.bits)

  rsvdNext.valid := (state(injectRsvdShift) || io.in.rsvd.valid) && !injectFire
  rsvdNext.bits := Mux(state(injectRsvdShift) && !io.in.rsvd.valid, io.nid, io.in.rsvd.bits)

  io.out.flit := Pipe(flitNext)
  io.out.rsvd := Pipe(rsvdNext)

  if(c2c) {
    io.eject.valid := io.in.flit.bits.tgt(nodeNidBits - 1, 0) === io.nid(nodeNidBits - 1, 0) && io.in.flit.valid
  } else {
    io.eject.valid := io.in.flit.bits.tgt === io.nid && io.in.flit.valid
  }
  io.eject.bits := io.in.flit.bits
}

class ChannelTap[T <: Flit](
  val gen: T, channel: String, ringNum: Int,
  ejectBuf: Int = 0, c2c: Boolean = false
)(implicit p: Parameters) extends ZJModule {
  val io = IO(new Bundle {
    val rx = Input(Vec(ringNum, new ChannelBundle(gen)))
    val tx = Output(Vec(ringNum, new ChannelBundle(gen)))
    val inject = Flipped(Decoupled(gen))
    val eject = Decoupled(gen)
    val nid = Input(UInt(niw.W))
    val injectTapSelOH = Input(Vec(ringNum, Bool()))
  })
  override val desiredName = if(c2c) s"C2cChannelTap$channel" else s"ChannelTap$channel"

  when(io.inject.valid) {
    assert(PopCount(io.injectTapSelOH) === 1.U, "Only one side can be picked!")
  }

  private val taps = Seq.fill(ringNum)(Module(new SingleChannelTap(gen, channel, c2c)))
  private val ejectArb = Module(new RRArbiter(gen, ringNum))
  for(idx <- taps.indices) {
    taps(idx).io.in := io.rx(idx)
    io.tx(idx) := taps(idx).io.out
    taps(idx).io.inject.valid := io.inject.valid && io.injectTapSelOH(idx)
    taps(idx).io.inject.bits := io.inject.bits
    taps(idx).io.nid := io.nid
    ejectArb.io.in(idx) <> taps(idx).io.eject
  }
  io.inject.ready := Mux1H(io.injectTapSelOH, taps.map(_.io.inject.ready))

  private val ejectBuffer = if(ejectBuf > 0) Some(Module(new EjectBuffer(gen, ejectBuf, channel))) else None
  if(ejectBuf > 0) {
    io.eject <> ejectBuffer.get.io.deq
    ejectBuffer.get.io.enq <> ejectArb.io.out
  } else {
    io.eject <> ejectArb.io.out
  }
}