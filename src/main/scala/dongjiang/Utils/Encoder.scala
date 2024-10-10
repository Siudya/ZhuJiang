package Utils

import chisel3._
import chisel3.util._
import chisel3.util.RRArbiter

object Encoder {
    def RREncoder(in: Seq[Bool]): UInt = {
        val arb = Module(new RRArbiter(UInt(log2Ceil(in.size).W), in.size))
        arb.io.in.zipWithIndex.foreach {
            case(a, i) =>
                a.valid := in(i)
                a.bits  := i.U
        }
        arb.io.out.ready := true.B
        arb.io.out.bits
    }
}