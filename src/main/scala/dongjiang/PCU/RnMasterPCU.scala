package DONGJIANG.PCU

import DONGJIANG._
import DONGJIANG.CHI._
import chisel3._
import org.chipsalliance.cde.config._
import chisel3.util.{Cat, Decoupled, PopCount, RegEnable, Valid, ValidIO, log2Ceil}


class RnMasterPCU(djBankId: Int, rnMasId: Int, param: InterfaceParam)(implicit p: Parameters) extends PCUBaseIO(isSlv = false, hasFree = true, hasReq2Slice = true, hasDBRCReq = true) {
  // Del it
  io <> DontCare
  dontTouch(io)

}