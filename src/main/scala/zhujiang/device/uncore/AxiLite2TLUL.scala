package zhujiang.device.uncore

import chisel3._
import chisel3.util._
import xs.utils.ResetRRArbiter
import zhujiang.axi._
import zhujiang.tilelink._

class AxiLite2TLUL(axiParams: AxiParams) extends Module {
  require(axiParams.dataBits == 64)
  private val tlParams = TilelinkParams(
    addrBits = axiParams.addrBits,
    sourceBits = axiParams.idBits,
    dataBits = axiParams.dataBits
  )
  val io = IO(new Bundle {
    val axi = Flipped(new AxiBundle(axiParams))
    val tl = new TLULBundle(tlParams)
  })
  private val busSize = log2Ceil(axiParams.dataBits / 8)
  private val awPipe = Module(new Queue(new AWFlit(axiParams), entries = 2))
  private val arPipe = Module(new Queue(new ARFlit(axiParams), entries = 2))
  private val wPipe = Module(new Queue(new WFlit(axiParams), entries = 2))
  private val rPipe = Module(new Queue(new RFlit(axiParams), entries = 2))
  private val bPipe = Module(new Queue(new BFlit(axiParams), entries = 2))
  awPipe.io.enq <> io.axi.aw
  arPipe.io.enq <> io.axi.ar
  wPipe.io.enq <> io.axi.w
  io.axi.r <> rPipe.io.deq
  io.axi.b <> bPipe.io.deq
  when(io.axi.aw.valid) {
    assert(io.axi.aw.bits.len === 0.U)
    assert(io.axi.aw.bits.size <= busSize.U)
  }
  when(io.axi.ar.valid) {
    assert(io.axi.ar.bits.len === 0.U)
    assert(io.axi.ar.bits.size <= busSize.U)
  }

  private val arb = Module(new ResetRRArbiter(new AFlit(tlParams), 2))
  io.tl.a <> arb.io.out
  private val wp = arb.io.in.head
  private val rp = arb.io.in.last

  wp.valid := awPipe.io.deq.valid && wPipe.io.deq.valid
  wp.bits := DontCare
  wp.bits.opcode := Mux(awPipe.io.deq.bits.size < busSize.U, AOpcode.PutPartialData, AOpcode.PutFullData)
  wp.bits.size := awPipe.io.deq.bits.size
  wp.bits.source := awPipe.io.deq.bits.id
  wp.bits.address := awPipe.io.deq.bits.addr
  wp.bits.mask := wPipe.io.deq.bits.strb
  wp.bits.data := wPipe.io.deq.bits.data
  wp.bits.corrupt := false.B
  awPipe.io.deq.ready := wp.ready && wPipe.io.deq.valid
  wPipe.io.deq.ready := wp.ready && awPipe.io.deq.valid

  rp.valid := arPipe.io.deq.valid
  rp.bits := DontCare
  rp.bits.opcode := AOpcode.Get
  rp.bits.size := arPipe.io.deq.bits.size
  rp.bits.source := arPipe.io.deq.bits.id
  rp.bits.address := arPipe.io.deq.bits.addr
  arPipe.io.deq.ready := rp.ready

  rPipe.io.enq.valid := io.tl.d.valid && io.tl.d.bits.opcode === DOpcode.AccessAckData
  rPipe.io.enq.bits := DontCare
  rPipe.io.enq.bits.id := io.tl.d.bits.source
  rPipe.io.enq.bits.data := io.tl.d.bits.data
  rPipe.io.enq.bits.resp := Cat(io.tl.d.bits.corrupt, io.tl.d.bits.denied)
  rPipe.io.enq.bits.last := true.B

  bPipe.io.enq.valid := io.tl.d.valid && io.tl.d.bits.opcode === DOpcode.AccessAck
  bPipe.io.enq.bits := DontCare
  bPipe.io.enq.bits.id := io.tl.d.bits.source
  bPipe.io.enq.bits.resp := Cat(io.tl.d.bits.corrupt, io.tl.d.bits.denied)

  io.tl.d.ready := Mux(rPipe.io.enq.valid, rPipe.io.enq.ready, bPipe.io.enq.ready)
}
