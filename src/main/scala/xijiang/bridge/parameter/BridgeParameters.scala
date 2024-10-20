package xijiang.bridge.parameter

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import zhujiang.HasZJParams
import zhujiang.chi._
import zhujiang._
import zhujiang.axi._
import _root_.xijiang.bridge.parameter


case object AXIParmKey extends Field[AxiParams]


abstract class BridgeBundle(implicit val p: Parameters) extends Bundle with BridgeTrait
abstract class BridgeModule(implicit val p: Parameters) extends Module with BridgeTrait


trait BridgeTrait {
    implicit val p : Parameters
    val axiParams   = AxiParams(idBits = 11, dataBits = 256, addrBits = 48)
    val Param       = p(AXIParmKey)
    val addrBits    = axiParams.addrBits
    val fakeMemBits = 64
    val chiBeatBits = 256
    val axiBeatBits = axiParams.dataBits
    val offsetBits  = 6
    val chiBeatByte = chiBeatBits / 8
    val axiBeatByte = axiBeatBits / 8
    val axiIdBits   = axiParams.idBits
    val nrEntrys    = 16
    val nidBits     = 4
    val sendNumBits = 4
    val nrBeat      = 2
    val sramEntrys  = 64


    def toDataID(x: UInt): UInt = {
        require(nrBeat == 1 | nrBeat == 2 | nrBeat == 4)
        if (nrBeat == 1) { "b00".U }
        else if (nrBeat == 2) { Mux(x === 0.U, "b00".U, "b10".U) }
        else if (nrBeat == 4) { x }
        else { 0.U }
    }

    def toBeatNum(x: UInt): UInt = {
        if (nrBeat == 1) { assert(x === "b00".U); 0.U }
        else if (nrBeat == 2) { assert(x === "b00".U | x === "b10".U); Mux(x === "b00".U, 0.U, 1.U) }
        else if (nrBeat == 4) { x }
        else { 0.U }
    }
}
object BurstMode {
  val width        = 2
  val Fix          = "b00".U
  val Incr         = "b01".U
  val Wrap         = "b10".U
  val Reserve      = "b11".U
}
