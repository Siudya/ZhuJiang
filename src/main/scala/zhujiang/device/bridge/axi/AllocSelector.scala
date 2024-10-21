package zhujiang.device.bridge.axi

import chisel3._
import chisel3.util._
import xs.utils.ParallelOperation

class DataBufferAllocReq(outstanding: Int) extends Bundle {
  val idxOH = UInt(outstanding.W)
  val size = UInt(3.W)
  val waitNum = UInt(log2Ceil(outstanding).W)
}

class DataBufferAllocReqSel2(outstanding: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(2, Valid(new DataBufferAllocReq(outstanding))))
    val out = Output(Valid(new DataBufferAllocReq(outstanding)))
  })
  private val valids = Cat(io.in.map(_.valid).reverse)
  private val payloads = io.in.map(_.bits)
  io.out.valid := valids.orR
  private val sel2 = valids === 2.U || (valids === 3.U && payloads(0).waitNum >= payloads(1).waitNum)
  io.out.bits := Mux(sel2, payloads(1), payloads(0))
}

class DataBufferAllocReqSelector(outstanding: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(outstanding, Flipped(Decoupled(new DataBufferAllocReq(outstanding))))
    val out = Decoupled(new AxiDataBufferAllocReq(outstanding))
  })
  private val selPipe = Module(new Queue(new AxiDataBufferAllocReq(outstanding), entries = 2))
  io.out <> selPipe.io.deq

  private def selOp(a: Valid[DataBufferAllocReq], b: Valid[DataBufferAllocReq]): Valid[DataBufferAllocReq] = {
    val sel = Module(new DataBufferAllocReqSel2(outstanding))
    sel.io.in.head := a
    sel.io.in.last := b
    sel.io.out
  }

  private val selSeq = io.in.map(in => {
    val res = Wire(Valid(new DataBufferAllocReq(outstanding)))
    res.valid := in.valid
    res.bits := in.bits
    res
  })

  private val selRes = ParallelOperation(selSeq, selOp)
  selPipe.io.enq.valid := selRes.valid
  selPipe.io.enq.bits.idxOH := selRes.bits.idxOH
  selPipe.io.enq.bits.size := selRes.bits.size

  for(i <- io.in.indices) io.in(i).ready := selRes.bits.idxOH(i) && selPipe.io.enq.ready
}
