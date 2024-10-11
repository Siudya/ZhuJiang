package xijiang.bridge.test

import zhujiang._
import zhujiang.chi._
import chisel3._
import chisel3.util._
import xijiang.bridge.parameter._
import org.chipsalliance.cde.config._
import xijiang.bridge.Utils.GenerateVerilog
import _root_.circt.stage.FirtoolOption
import chisel3.stage.ChiselGeneratorAnnotation
import _root_.circt.stage._


object DDRState {
  val width        = 2
  val Free         = "b00".U
  val SendDBIDResp = "b01".U
  val WaitData     = "b10".U
  val WriteData    = "b11".U
}

class DDREntry(implicit p: Parameters) extends BridgeBundle {
    val state           = UInt(DDRState.width.W)
    val datVal          = Vec(nrBeat, Bool())
    val data            = Vec(nrBeat, UInt(fakeMemBits.W))
    val mask            = Vec(nrBeat, UInt(fakeMemBits.W))
    val addr            = UInt(fakeMemBits.W)
    val txnid           = UInt(12.W)
}

class FakeCHISlave(implicit p : Parameters) extends BridgeModule {
  //---------------------------------------------------------------------------------------------------------------------------------//
  //----------------------------------------------------- IO Bundle -----------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  val io = IO(new Bundle{

    //CHI interface
      val txreq = Flipped(DecoupledIO(new ReqFlit))
      val txdat = Flipped(DecoupledIO(new DataFlit))
      val txrsp = Flipped(Decoupled(new RespFlit))

      val rxrsp = DecoupledIO(new RespFlit)
      val rxdat = DecoupledIO(new DataFlit)
      val rxsnp = DecoupledIO(new SnoopFlit)

    })
  //---------------------------------------------------------------------------------------------------------------------------------//
  //-------------------------------------------------- Reg and Wire Define ----------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  io.txrsp <> DontCare
  io.rxsnp <> DontCare

  val mem          = Seq.fill(nrBeat) { Module(new MemHelper()) }
  val wrBufRegVec  = RegInit(VecInit(Seq.fill(nrEntrys){0.U.asTypeOf(new DDREntry)}))

  val rDataQueue   = Module(new Queue(Vec(nrBeat, UInt(chiBeatBits.W)), entries = 4, flow = false, pipe = true))
  val rReqQueue    = Module(new Queue(new ReqFlit, entries = 4, flow = false, pipe = true))
  val rDataFlitQ   = Module(new Queue(new DataFlit, entries = 4, flow = false, pipe = true))


  val bufFreeVec     = wrBufRegVec.map(_.state === DDRState.Free)
  val bufSendDBIDVec = wrBufRegVec.map(_.state === DDRState.SendDBIDResp)
  val bufWaitDataVec = wrBufRegVec.map(_.state === DDRState.WaitData)
  val bufWrDataVec   = wrBufRegVec.map(_.state === DDRState.WriteData)

  val selFreeBuf   = PriorityEncoder(bufFreeVec)
  val selSendDBIDBuf = PriorityEncoder(bufSendDBIDVec)
  val selWrDataBuf   = PriorityEncoder(bufWrDataVec)
  
  
  val receiptValid = RegInit(false.B)
  val receiptGen   = WireInit(rReqQueue.io.deq.fire)
  val readReceipt  = RegInit(0.U.asTypeOf(new RespFlit))

  val dbidValid    = WireInit((io.txreq.bits.Opcode === ReqOpcode.WriteUniqueFull || io.txreq.bits.Opcode === ReqOpcode.WriteUniquePtl) & io.txreq.fire)
  
  val sendBeatNumReg = RegInit(0.U(log2Ceil(nrBeat).W))

  val mask = WireInit(VecInit(Seq.fill(chiBeatByte){0.U(8.W)}))

  //---------------------------------------------------------------------------------------------------------------------------------//
  //------------------------------------------------------- Logic -------------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//

  /* 
   * Generate ReadReceipt signal
   */

  when(receiptGen){
    receiptValid := true.B
  }
  when(io.rxrsp.fire & io.rxrsp.bits.Opcode === RspOpcode.ReadReceipt){
    receiptValid := false.B
  }

  when(receiptValid){
    readReceipt.TxnID  := io.txreq.bits.TxnID
    readReceipt.Opcode := RspOpcode.ReadReceipt
  }


  /* 
   * Read data from fake mem
   */
  
  //  when(io.txreq.fire & io.txreq.bits.Opcode === REQ.ReadOnce){
    rReqQueue.io.enq <> io.txreq
  //  }
   mem.foreach { case m => m.clk := clock}
   mem.foreach { case m => 
    m.rIdx := rReqQueue.io.deq.bits.Addr
    m.ren  := rReqQueue.io.deq.fire
  }

  rDataQueue.io.enq.valid := rReqQueue.io.deq.fire
  rDataQueue.io.enq.bits  := Cat(mem.map(_.rdata)).asTypeOf(Vec(nrBeat, UInt(chiBeatBits.W)))
  rDataQueue.io.deq.ready := sendBeatNumReg === (nrBeat - 1).U & io.rxdat.fire
  
  rDataFlitQ.io.deq.ready := sendBeatNumReg === (nrBeat - 1).U & io.rxdat.fire

  sendBeatNumReg := sendBeatNumReg + io.rxdat.fire.asUInt

  rReqQueue.io.deq.ready  := rDataFlitQ.io.enq.ready
  rDataFlitQ.io.enq.valid := rReqQueue.io.deq.valid
  rDataFlitQ.io.enq.bits  := DontCare
  rDataFlitQ.io.enq.bits.Opcode := DatOpcode.CompData
  rDataFlitQ.io.enq.bits.TxnID  := rReqQueue.io.deq.bits.TxnID

  /* 
   * Write data to fake mem
   */
  val be = io.txdat.bits.BE.asTypeOf(Vec(chiBeatByte, UInt(1.W)))
  mask.zip(be).foreach{
    case(m, b) =>
      when(b === 1.U){
        m := 255.U
      }.otherwise{
        m := 0.U      }
  }

  mem.zipWithIndex.foreach{
    case(m, i) =>
      m.wIdx  := wrBufRegVec(selWrDataBuf).addr
      m.wen   := bufWrDataVec.reduce(_|_)
      m.wdata := wrBufRegVec(selWrDataBuf).data.asTypeOf(Vec(nrBeat, UInt(64.W)))(i)
      m.mask  := wrBufRegVec(selWrDataBuf).mask.asTypeOf(Vec(nrBeat, UInt(64.W)))(i)
  }



  /* 
   * Write FSM Updata
   */

  wrBufRegVec.zipWithIndex.foreach{
    case(w, i) =>
      switch(w.state){
        is(DDRState.Free){
          val hit = dbidValid & selFreeBuf === i.U
          when(hit){
            w.state := DDRState.SendDBIDResp
            w.addr  := io.txreq.bits.Addr
            w.txnid := io.txreq.bits.TxnID
          }.otherwise{
            w := 0.U.asTypeOf(w)
          }
        }
        is(DDRState.SendDBIDResp){
          val hit = io.rxrsp.fire & io.rxrsp.bits.Opcode === RspOpcode.CompDBIDResp & selSendDBIDBuf === i.U
          when(hit){
            w.state := DDRState.WaitData
          }
        }
        is(DDRState.WaitData) {
          val hit = io.txdat.fire & io.txdat.bits.TxnID === i.U
          when(hit){
            w.state := Mux(PopCount(w.datVal) === 1.U, DDRState.WriteData, w.state)
            w.datVal(toBeatNum(io.txdat.bits.DataID)) := true.B
            w.data(toBeatNum(io.txdat.bits.DataID))   := io.txdat.bits.Data
            w.mask(toBeatNum(io.txdat.bits.DataID))   := mask.asUInt
          }
        }
        is(DDRState.WriteData) {
          w.state := DDRState.Free
        }
      }
  }


  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------- IO Interface ---------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//



  io.txdat.ready := true.B

  io.rxrsp.valid       := bufSendDBIDVec.reduce(_|_) | receiptValid
  io.rxrsp.bits        := DontCare
  io.rxrsp.bits.Opcode := Mux(receiptValid, RspOpcode.ReadReceipt, RspOpcode.CompDBIDResp)
  io.rxrsp.bits.DBID   := selSendDBIDBuf
  io.rxrsp.bits.TxnID  := Mux(receiptValid, readReceipt.TxnID, wrBufRegVec(selSendDBIDBuf).txnid)

  io.rxdat.valid       := rDataQueue.io.deq.valid & rDataFlitQ.io.deq.valid
  io.rxdat.bits        := rDataFlitQ.io.deq.bits
  io.rxdat.bits.Data   := rDataQueue.io.deq.bits(sendBeatNumReg)
  io.rxdat.bits.DataID := toDataID(sendBeatNumReg)

  

  //---------------------------------------------------------------------------------------------------------------------------------//
  //----------------------------------------------------- Assertion -----------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  assert(PopCount(bufWrDataVec) <= 1.U)
}

object FakeCHISlave extends App {
  private val config = new Config((_,_,_) => {
    case ZJParametersKey => ZJParameters()
  })
  private val gen = () => new FakeCHISlave()(config)
  (new ChiselStage).execute(
    Array("--target", "verilog") ++ args,
    Seq(
      FirtoolOption("-O=debug"),
    ) ++ Seq(ChiselGeneratorAnnotation(gen))
  )
}