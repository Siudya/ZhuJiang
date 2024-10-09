package zhujiang.device.uncore

import chisel3._
import chisel3.util._
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
  private val wPipe = Module(new Queue(new AWFlit(axiParams), entries = 2))
  private val dPipe = Module(new Queue(new DFlit(tlParams), entries = 2))
  private val arb = Module(new RRArbiter(new DFlit(tlParams), 2))
  private val rp = arb.io.in.head
  private val bp = arb.io.in.last

  aPipe.io.enq <> io.tl.a
  dPipe.io.enq <> arb.io.out
  io.tl.d <> dPipe.io.deq

  when(io.tl.a.valid) {
    assert(io.axi.aw.bits.size <= busSize.U)
  }

  private val pf = aPipe.io.deq.bits.opcode === AOpcode.PutFullData
  private val pp = aPipe.io.deq.bits.opcode === AOpcode.PutPartialData
  private val get = aPipe.io.deq.bits.opcode === AOpcode.Get
  io.axi.aw.valid := aPipe.io.deq.valid && (pf || pp) && io.axi.w.ready
  io.axi.aw.bits := DontCare
  io.axi.aw.bits.id := aPipe.io.deq.bits.source
  io.axi.aw.bits.addr := aPipe.io.deq.bits.address
  io.axi.aw.bits.size := aPipe.io.deq.bits.size

  io.axi.ar.valid := aPipe.io.deq.valid && get
  io.axi.ar.bits := DontCare
  io.axi.ar.bits.id := aPipe.io.deq.bits.source
  io.axi.ar.bits.addr := aPipe.io.deq.bits.address
  io.axi.ar.bits.size := aPipe.io.deq.bits.size

  io.axi.w.valid := aPipe.io.deq.valid && (pf || pp) && io.axi.aw.ready
  io.axi.w.bits := DontCare
  io.axi.w.bits.data := Fill(axiParams.dataBits / tlParams.dataBits, aPipe.io.deq.bits.data)
  io.axi.w.bits.strb := aPipe.io.deq.bits.mask
  io.axi.w.bits.last := true.B

  aPipe.io.deq.ready := Mux(get, io.axi.ar.ready, io.axi.aw.ready & io.axi.w.ready)

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
