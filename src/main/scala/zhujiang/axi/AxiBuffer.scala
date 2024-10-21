package zhujiang.axi

import chisel3._
import chisel3.util._

class AxiBuffer(axiParams: AxiParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new AxiBundle(axiParams))
    val out = new AxiBundle(axiParams)
  })
  io.out.aw <> Queue(io.in.aw)
  io.out.ar <> Queue(io.in.ar)
  io.out.w <> Queue(io.in.w)
  io.in.r <> Queue(io.out.r)
  io.in.b <> Queue(io.out.b)
}
