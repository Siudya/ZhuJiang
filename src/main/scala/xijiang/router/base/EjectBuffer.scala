package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xs.utils.PickOneLow
import zhujiang.ZJModule
import zhujiang.chi.Flit

class EjectBuffer[T <: Flit](gen: T, size: Int, chn: String)(implicit p: Parameters) extends ZJModule {
  private val tagBits = 12 + 2 * niw // TgtId + SrcId + TxnId
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(UInt(gen.getWidth.W)))
    val deq = Decoupled(UInt(gen.getWidth.W))
  })
  override val desiredName = s"EjectBuffer$chn"
  private val queue = Module(new Queue(UInt(gen.getWidth.W), size, pipe = true))
  private val rsvdTags = Reg(Vec(size, UInt(tagBits.W)))
  private val rsvdValids = RegInit(VecInit(Seq.fill(size)(false.B)))
  private val empties = RegInit(size.U(log2Ceil(size + 1).W))
  private val rsvdNumReg = RegInit(0.U(log2Ceil(size + 1).W))

  when(queue.io.enq.fire && !queue.io.deq.fire) {
    assert(empties > 0.U)
    empties := empties - 1.U
  }.elsewhen(!queue.io.enq.fire && queue.io.deq.fire) {
    assert(empties < size.U)
    empties := empties + 1.U
  }
  when(empties.orR) {
    assert(queue.io.enq.ready)
  }

  private def getTag(flit: UInt): UInt = flit(tagBits + 3, 4)

  io.deq <> queue.io.deq
  private val rsvdSel = PickOneLow(rsvdValids)
  private val rsvdHit = Cat(rsvdTags.zip(rsvdValids).map(e => e._1 === getTag(io.enq.bits) && e._2)).orR
  private val doReserve = io.enq.valid && !io.enq.ready && !rsvdHit && rsvdSel.valid

  when(rsvdHit && queue.io.enq.fire) {
    rsvdNumReg := rsvdNumReg - 1.U
  }.elsewhen(doReserve) {
    rsvdNumReg := rsvdNumReg + 1.U
  }
  assert(rsvdNumReg === PopCount(rsvdValids))

  for(idx <- rsvdValids.indices) {
    val v = rsvdValids(idx)
    val tag = rsvdTags(idx)
    val doRsv = rsvdSel.bits(idx) && doReserve
    when(v && queue.io.enq.fire && getTag(io.enq.bits) === tag) {
      v := false.B
    }.elsewhen(doRsv) {
      v := true.B
    }
    when(doRsv) {
      tag := getTag(io.enq.bits)
    }
  }

  private val allowEnq = Mux(rsvdHit, true.B, rsvdNumReg < empties)
  queue.io.enq.valid := io.enq.valid & allowEnq
  queue.io.enq.bits := io.enq.bits
  io.enq.ready := queue.io.enq.ready & allowEnq
}
