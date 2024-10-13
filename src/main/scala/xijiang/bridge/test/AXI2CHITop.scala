package xijiang.bridge.test

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xijiang.bridge.axi2chi._
import zhujiang._
import zhujiang.chi._
import zhujiang.axi._
import xijiang.bridge.parameter._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.util.SeqToAugmentedSeq
import freechips.rocketchip.diplomacy._

class AXI2CHITop(implicit p : Parameters) extends BridgeModule {
  val io = IO(new Bundle{

    //AXI4 standard interface
    val axi_aw = Flipped(Decoupled(new AWFlit(axiParams)))
    val axi_w  = Flipped(Decoupled(new WFlit(axiParams)))
    val axi_b  = Decoupled(new BFlit(axiParams))
    val axi_r  = Decoupled(new RFlit(axiParams))
    val axi_ar = Flipped(Decoupled(new ARFlit(axiParams)))

  })
  val axi2chi  = Module(new AXI2CHI)
  val chislave = Module(new FakeCHISlave)
  
  // Connection
  io.axi_aw <> axi2chi.io.axi_aw
  io.axi_w  <> axi2chi.io.axi_w
  io.axi_b  <> axi2chi.io.axi_b

  io.axi_ar <> axi2chi.io.axi_ar
  io.axi_r  <> axi2chi.io.axi_r

  chislave.io.txreq <> axi2chi.io.chi_txreq
  chislave.io.txdat <> axi2chi.io.chi_txdat
  chislave.io.txrsp <> axi2chi.io.chi_txrsp

  chislave.io.rxdat <> axi2chi.io.chi_rxdat
  chislave.io.rxrsp <> axi2chi.io.chi_rxrsp
  chislave.io.rxsnp <> axi2chi.io.chi_rxsnp
}

