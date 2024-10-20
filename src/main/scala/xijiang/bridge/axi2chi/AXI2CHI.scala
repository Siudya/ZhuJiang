package xijiang.bridge.axi2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import xijiang.bridge._
import xijiang.bridge.parameter._
import zhujiang._
import zhujiang.chi._
import zhujiang.axi._
import xijiang.bridge.Utils.GenerateVerilog
import _root_.circt.stage.FirtoolOption
import chisel3.stage.ChiselGeneratorAnnotation
import _root_.circt.stage._

class AXI2CHI(implicit p: Parameters) extends BridgeModule{
  val io = IO(new Bundle {
    // AXI4 Interface
    val axi_aw = Flipped(Decoupled(new AWFlit(axiParams)))
    val axi_w  = Flipped(Decoupled(new WFlit(axiParams)))
    val axi_b  = Decoupled(new BFlit(axiParams))
    val axi_r  = Decoupled(new RFlit(axiParams))
    val axi_ar = Flipped(Decoupled(new ARFlit(axiParams)))

    // CHI Interface
    val chi_txreq = Decoupled(new ReqFlit)
    val chi_txdat = Decoupled(new DataFlit)
    val chi_txrsp = Decoupled(new RespFlit)

    val chi_rxrsp = Flipped(Decoupled(new RespFlit))
    val chi_rxdat = Flipped(Decoupled(new DataFlit))
    val chi_rxsnp = Flipped(Decoupled(new SnoopFlit))
    
  })

  val readHandle  = Module(new ReadHandle)
  val writeHandle = Module(new WriteHandle)

  val readReqQ    = Module(new Queue(new ReqFlit, entries = 4, flow = false, pipe = true))
  val writeReqQ   = Module(new Queue(new ReqFlit, entries = 4, flow = false, pipe = true))

  readReqQ.io.enq.bits           := readHandle.io.chi_txreq.bits
  readReqQ.io.enq.valid          := readHandle.io.chi_txreq.valid
  readHandle.io.chi_txreq.ready  := readReqQ.io.enq.ready

  writeReqQ.io.enq.bits          := writeHandle.io.chi_txreq.bits
  writeReqQ.io.enq.valid         := writeHandle.io.chi_txreq.valid
  writeHandle.io.chi_txreq.ready := writeReqQ.io.enq.ready

  writeReqQ.io.deq.ready := io.chi_txreq.ready
  readReqQ.io.deq.ready  := io.chi_txreq.ready & !writeReqQ.io.deq.fire



  io.axi_ar <> readHandle.io.axi_ar
  io.axi_aw <> writeHandle.io.axi_aw
  io.axi_b  <> writeHandle.io.axi_b
  io.axi_r  <> readHandle.io.axi_r
  io.axi_w  <> writeHandle.io.axi_w

  io.chi_rxdat                   <> readHandle.io.chi_rxdat
  writeHandle.io.chi_rxrsp.valid := io.chi_rxrsp.valid
  writeHandle.io.chi_rxrsp.bits  := io.chi_rxrsp.bits
  
  readHandle.io.chi_rxrsp.valid  := io.chi_rxrsp.valid
  readHandle.io.chi_rxrsp.bits   := io.chi_rxrsp.bits
  io.chi_rxrsp.ready             := readHandle.io.chi_rxrsp.ready & writeHandle.io.chi_rxrsp.ready
  io.chi_rxsnp                   <> DontCare

  io.chi_txdat       <> writeHandle.io.chi_txdat
  io.chi_txreq.valid := writeReqQ.io.deq.valid | readReqQ.io.deq.valid
  io.chi_txreq.bits  := Mux(writeReqQ.io.deq.valid, writeReqQ.io.deq.bits, Mux(readReqQ.io.deq.valid, readReqQ.io.deq.bits, 0.U.asTypeOf(readReqQ.io.deq.bits)))
  io.chi_txrsp       <> DontCare
  
}

//-----------------------------------------------------------------------------//
//-------------------------------GenerateVerilog-------------------------------//
//-----------------------------------------------------------------------------//



// object AXI2CHI extends App {
//     val config = new Config((_, _, _) => {
//         case ZJParametersKey      => ZJParameters()
//         case BridgeParamKey  => BridgeParam()
//     })

//     GenerateVerilog(args, () => new AXI2CHI()(config), name = "AXI2CHI", split = false)
// }

object AXI2CHI extends App {
  private val config = new Config((_,_,_) => {
    case ZJParametersKey => ZJParameters()
    case AXIParmKey      => AxiParams()
  })
  private val gen = () => new AXI2CHI()(config)
  (new ChiselStage).execute(
    Array("--target", "verilog") ++ args,
    Seq(
      FirtoolOption("-O=debug"),
    ) ++ Seq(ChiselGeneratorAnnotation(gen))
  )
}