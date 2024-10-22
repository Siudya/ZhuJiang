package zhujiang.device.uncore

import chisel3._
import chisel3.util._
import xs.utils.ResetRRArbiter
import zhujiang.axi._
import zhujiang.tilelink._

class TLUL2AxiLite(axiParams: AxiParams) extends Module {
  private val tlParams = TilelinkParams(
    addrBits = axiParams.addrBits,
    sourceBits = axiParams.idBits,
    dataBits = axiParams.dataBits
  )
  val io = IO(new Bundle {
    val tl = Flipped(new TLULBundle(tlParams))
    val axi = new AxiBundle(axiParams)
  })
  private val busSize = log2Ceil(tlParams.dataBits / 8)
  private val awPipe = Module(new Queue(new AWFlit(axiParams), entries = 2))
  private val wPipe = Module(new Queue(new WFlit(axiParams), entries = 2))
  private val arPipe = Module(new Queue(new ARFlit(axiParams), entries = 2))
  private val dPipe = Module(new Queue(new DFlit(tlParams), entries = 2))
  private val arb = Module(new ResetRRArbiter(new DFlit(tlParams), 2))
  private val rp = arb.io.in.head
  private val bp = arb.io.in.last

  io.axi.aw <> awPipe.io.deq
  io.axi.ar <> arPipe.io.deq
  io.axi.w <> wPipe.io.deq
  io.tl.d <> dPipe.io.deq
  dPipe.io.enq <> arb.io.out

  when(io.tl.a.valid) {
    assert(io.tl.a.bits.size <= busSize.U)
  }

  private val pf = io.tl.a.bits.opcode === AOpcode.PutFullData
  private val pp = io.tl.a.bits.opcode === AOpcode.PutPartialData
  private val get = io.tl.a.bits.opcode === AOpcode.Get
  private val aw = awPipe.io.enq
  private val ar = arPipe.io.enq
  private val w = wPipe.io.enq

  aw.valid := io.tl.a.valid && (pf || pp) && w.ready
  aw.bits := DontCare
  aw.bits.id := io.tl.a.bits.source
  aw.bits.addr := io.tl.a.bits.address
  aw.bits.size := io.tl.a.bits.size

  ar.valid := io.tl.a.valid && get
  ar.bits := DontCare
  ar.bits.id := io.tl.a.bits.source
  ar.bits.addr := io.tl.a.bits.address
  ar.bits.size := io.tl.a.bits.size

  w.valid := io.tl.a.valid && (pf || pp) && aw.ready
  w.bits := DontCare
  w.bits.data := Fill(axiParams.dataBits / tlParams.dataBits, io.tl.a.bits.data)
  w.bits.strb := io.tl.a.bits.mask
  w.bits.last := true.B

  io.tl.a.ready := Mux(ar.valid, ar.ready, aw.ready & w.ready)

  rp.valid := io.axi.r.valid
  rp.bits := DontCare
  rp.bits.opcode := DOpcode.AccessAckData
  rp.bits.data := io.axi.r.bits.data
  rp.bits.source := io.axi.r.bits.id
  io.axi.r.ready := rp.ready

  bp.valid := io.axi.b.valid
  bp.bits := DontCare
  bp.bits.opcode := DOpcode.AccessAck
  bp.bits.source := io.axi.b.bits.id
  io.axi.b.ready := bp.ready
}
