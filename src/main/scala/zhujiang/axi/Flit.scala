package zhujiang.axi

import chisel3._
import chisel3.util.Decoupled

case class AxiParams(
  addrBits: Int = 32,
  idBits: Int = 5,
  userBits: Int = 0,
  dataBits: Int = 64
)

class AWFlit(params: AxiParams) extends Bundle {
  val id = UInt(params.idBits.W)
  val addr = UInt(params.addrBits.W)
  val len = UInt(8.W)
  val size = UInt(3.W)
  val burst = UInt(2.W)
  val lock = UInt(2.W)
  val cache = UInt(4.W)
  val prot = UInt(3.W)
  val qos = UInt(4.W)
  val region = UInt(4.W)
  val user = UInt(params.userBits.W)
}

class ARFlit(params: AxiParams) extends Bundle {
  val id = UInt(params.idBits.W)
  val addr = UInt(params.addrBits.W)
  val len = UInt(8.W)
  val size = UInt(3.W)
  val burst = UInt(2.W)
  val lock = UInt(2.W)
  val cache = UInt(4.W)
  val prot = UInt(3.W)
  val qos = UInt(4.W)
  val region = UInt(4.W)
  val user = UInt(params.userBits.W)
}

class WFlit(params: AxiParams) extends Bundle {
  val data = UInt(params.dataBits.W)
  val strb = UInt((params.dataBits / 8).W)
  val last = Bool()
  val user = UInt(params.userBits.W)
}

class RFlit(params: AxiParams) extends Bundle {
  val id = UInt(params.idBits.W)
  val data = UInt(params.dataBits.W)
  val resp = UInt(2.W)
  val last = Bool()
  val user = UInt(params.userBits.W)
}

class BFlit(params: AxiParams) extends Bundle {
  val id = UInt(params.idBits.W)
  val resp = UInt(2.W)
  val user = UInt(params.userBits.W)
}

class AxiBundle(val params: AxiParams) extends Bundle {
  val aw = Decoupled(new AWFlit(params))
  val ar = Decoupled(new ARFlit(params))
  val w = Decoupled(new WFlit(params))
  val b = Flipped(Decoupled(new BFlit(params)))
  val r = Flipped(Decoupled(new RFlit(params)))
}