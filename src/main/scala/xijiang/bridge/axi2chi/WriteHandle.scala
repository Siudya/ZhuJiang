package xijiang.bridge.axi2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import xijiang.bridge.parameter._
import zhujiang._
import zhujiang.chi._
import zhujiang.axi._

class WriteHandle(implicit p: Parameters) extends BridgeModule{
  val io = IO(new Bundle {
    // AXI4 Interface
    val axi_aw = Flipped(Decoupled(new AWFlit(axiParams)))
    val axi_w  = Flipped(Decoupled(new WFlit(axiParams)))
    val axi_b  = Decoupled(new BFlit(axiParams))

    // CHI Interface
    val chi_txreq = Decoupled(new ReqFlit)
    val chi_rxrsp = Flipped(Decoupled(new RespFlit))
    val chi_txdat = Decoupled(new DataFlit)
  })

  io.axi_aw <> DontCare
  io.axi_w  <> DontCare
  io.axi_b  <> DontCare

  io.chi_txreq <> DontCare
  io.chi_rxrsp <> DontCare
  io.chi_txdat <> DontCare


  io.axi_aw.ready := true.B
  
}