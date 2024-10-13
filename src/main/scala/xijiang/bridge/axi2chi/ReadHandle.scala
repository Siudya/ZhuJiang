package xijiang.bridge.axi2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import xijiang.bridge.parameter._
import zhujiang._
import zhujiang.chi._
import zhujiang.axi._
import xs.utils.sram._
import os.group.set
import freechips.rocketchip.rocket.Instructions.SRA
import _root_.xijiang.bridge.axi2chi.SRAMState.{sendReq => sendReq}


class AREntry(implicit p: Parameters) extends BridgeBundle {
    val busy            = Bool()
    val addr            = UInt(addrBits.W)
    val burst           = UInt(BurstMode.width.W)
    val len             = UInt(8.W)
    val size            = UInt(3.W)
    val arid            = UInt(axiIdBits.W)
    val nid             = UInt(nidBits.W)
    val sendNum         = UInt(sendNumBits.W)
    
}

class sramStateEntry(implicit p : Parameters) extends BridgeBundle {
  val areid         = UInt(4.W)
  val state         = UInt(SRAMState.width.W)
  val num           = UInt(6.W)
  val dataid        = UInt(2.W)
  val full          = Bool()
}

object SRAMState {
  val width        = 3
  val Free         = "b000".U
  val sendReq      = "b001".U
  val ReceiveData  = "b010".U
  val ReceiveRec   = "b011".U
  val ReceiveAll   = "b100".U
}
class SRAMSelector(implicit p: Parameters) extends BridgeModule {
  val io = IO(new Bundle() {
    val idle = Input(Vec(nrEntrys, Bool()))
    val idleNum = Output(UInt((log2Ceil(nrEntrys) + 1).W))
    val out0 = UInt(log2Ceil(nrEntrys).W)
    val out1 = UInt(log2Ceil(nrEntrys).W)
  })
  io.idleNum := PopCount(io.idle)
  io.out0    := PriorityEncoder(io.idle)
  val idle1   = WireInit(io.idle)
  idle1(io.out0) := false.B
  io.out1    := PriorityEncoder(idle1)
}

  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------- Module Define --------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//


class ReadHandle(implicit p: Parameters) extends BridgeModule{
  val io = IO(new Bundle {
    // AXI4 Interface
    val axi_ar = Flipped(Decoupled(new ARFlit(axiParams)))
    val axi_r  = Decoupled(new RFlit(axiParams))

    // CHI Interface
    val chi_txreq = Decoupled(new ReqFlit)
    val chi_rxrsp = Flipped(Decoupled(new RespFlit))
    val chi_rxdat = Flipped(Decoupled(new DataFlit))
  })
  io.axi_ar <> DontCare
  io.axi_r  <> DontCare
  io.chi_txreq <> DontCare
  io.chi_rxrsp <> DontCare
  io.chi_rxdat <> DontCare

  


  //---------------------------------------------------------------------------------------------------------------------------------//
  //-------------------------------------------------- Reg and Wire Define ----------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//

  // val arEntrys          = RegInit(VecInit(Seq.fill(nrEntrys){0.U.asTypeOf(new AREntry)}))
  // val sramSelector      = Module(new SRAMSelector)
  // val readSram          = Module(new SRAMTemplate(gen = UInt(chiBeatBits.W), set = sramEntrys, singlePort = true))

  // val sramStateEntrys   = RegInit(VecInit(Seq.fill(nrEntrys){0.U.asTypeOf(new sramStateEntry)}))
  // val sramFreeEntrys    = sramStateEntrys.map(_.state === SRAMState.Free)

  // val arFreeEntrysVec   = arEntrys.map(_.busy === false.B)
  // val selFreeEntry      = PriorityEncoder(arFreeEntrysVec)

  // val selBusyEntryVec   = arEntrys.map(_.busy === true.B)

  // val nidVec            = arEntrys.map(a => a.arid === io.axi_ar.bits.id & a.busy === true.B)


  //---------------------------------------------------------------------------------------------------------------------------------//
  //------------------------------------------------------- Logic -------------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  // sramSelector.io.idle := sramFreeEntrys


  // arEntrys.zipWithIndex.foreach{
  //   case(a, i) =>
  //     switch(a.busy){
  //       is(false.B){
  //         val hit = io.axi_ar.fire & selFreeEntry === i.U
  //         val nid = PopCount(nidVec)
  //         when(hit){
  //           a.busy  := true.B
  //           a.addr  := io.axi_ar.bits.addr
  //           a.arid  := io.axi_ar.bits.id
  //           a.len   := io.axi_ar.bits.len
  //           a.size  := io.axi_ar.bits.size
  //           a.burst := io.axi_ar.bits.burst
  //           a.nid   := nid
  //           a.sendNum := 0.U
  //         }
  //       }
  //       is(true.B){
  //         val rid = io.axi_r.bits.id
  //         val endHit = io.axi_r.fire & io.axi_r.bits.last & a.nid === 0.U & a.arid === rid
  //         when(endHit){
  //           a        := 0.U.asTypeOf(a)
  //           a.busy   := false.B
  //         }
  //         val sendLastReq = (a.sendNum === a.len(7, 1)).asBool
  //         val sendHalfReq = a.len(0) === 0.U
  //         val shouldSend  = a.len(7, 1) + sendHalfReq.asUInt
  //         when(sendLastReq & sendHalfReq){
  //           sramStateEntrys(sramSelector.io.out0).state := SRAMState.sendReq
  //           sramStateEntrys(sramSelector.io.out0).areid := i.U
  //           sramStateEntrys(sramSelector.io.out0).full  := false.B
  //           sramStateEntrys(sramSelector.io.out0).num   := a.sendNum
  //           a.sendNum                                   := a.sendNum + 1.U
  //         }
  //         when(a.sendNum < a.len(7, 1)){
  //           sramStateEntrys(sramSelector.io.out0).state := SRAMState.sendReq
  //           sramStateEntrys(sramSelector.io.out0).areid := i.U
  //           sramStateEntrys(sramSelector.io.out0).full  := false.B
  //           sramStateEntrys(sramSelector.io.out0).num   := a.sendNum

  //           sramStateEntrys(sramSelector.io.out1).state := SRAMState.sendReq
  //           sramStateEntrys(sramSelector.io.out1).areid := i.U
  //           sramStateEntrys(sramSelector.io.out1).full  := false.B
  //           sramStateEntrys(sramSelector.io.out1).num   := a.sendNum

  //           a.sendNum                                   := a.sendNum + 1.U
  //         }
          
  //       }
  //     }
  //   }
  

  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------- IO Interface ---------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//

  io.axi_ar.ready      := true.B

  
  
}