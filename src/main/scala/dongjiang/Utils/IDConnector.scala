package Utils

import DONGJIANG.HasToIDBits
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.FastArbiter

object IDConnector {
    // DecoupledIO -> DecoupledIOVec
    def idSelDec2DecVec[T <: Bundle with HasToIDBits](in: DecoupledIO[T], out: Seq[DecoupledIO[T]], level: Int): Unit = {
        in.ready := false.B
        out.foreach(_.bits := in.bits)
        out.zipWithIndex.foreach {
            case (o, i) =>
                o.bits := in.bits
                val idMatch = WireInit(false.B)
                if (level == 0) {
                    idMatch := in.bits.to.idL0 === i.U
                } else if (level == 1) {
                    idMatch := in.bits.to.idL1 === i.U
                } else if (level == 2) {
                    idMatch := in.bits.to.idL2 === i.U
                } else {
                    assert(false, "id level cant be over 2")
                }
                when(idMatch) {
                    o.valid := in.valid
                    in.ready := o.ready
                }.otherwise {
                    o.valid := false.B
                }
        }
    }


    // ValidIO -> ValidIOVec
    def idSelVal2ValVec[T <: Bundle with HasToIDBits](in: ValidIO[T], out: Seq[ValidIO[T]], level: Int): Unit = {
        out.foreach(_.bits := in.bits)
        out.zipWithIndex.foreach {
            case (o, i) =>
                o.bits := in.bits
                val idMatch = WireInit(false.B)
                if (level == 0) {
                    idMatch := in.bits.to.idL0 === i.U
                } else if (level == 1) {
                    idMatch := in.bits.to.idL1 === i.U
                } else if (level == 2) {
                    idMatch := in.bits.to.idL2 === i.U
                } else {
                    assert(false, "id level cant be over 2")
                }
                when(idMatch) {
                    o.valid := in.valid
                }.otherwise {
                    o.valid := false.B
                }
        }
    }


}