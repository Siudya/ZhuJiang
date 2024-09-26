package DONGJIANG.DCU

import zhujiang.chi._
import xijiang.Node
import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIOp.REQ._
import DONGJIANG.CHI.CHIOp.RSP._
import DONGJIANG.CHI.CHIOp.DAT._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import xijiang.router.base.IcnBundle
import xs.utils.sram._


object DCUWState {
    val width = 3
    // commom
    val Free            = "b000".U
    val SendDBIDResp    = "b001".U
    val WaitData        = "b010".U
    val WriteData       = "b011".U // Send Write Req To DataStorage
    val Writting        = "b100".U // Write Data To DataStorage
}

class DCUWEntry(implicit p: Parameters) extends DJBundle {
    val state           = UInt(DCUWState.width.W)
    val datVal          = Vec(nrBeat, Bool())
    val data            = Vec(nrBeat, UInt(beatBits.W))
    val dsIndex         = UInt(dsIndexBits.W)
    val dsBank          = UInt(dsBankBits.W)
    val srcID           = UInt(chiNodeIdBits.W)
    val txnID           = UInt(djparam.chiTxnidBits.W)
}

class SramReqBundle(implicit p: Parameters) extends DJBundle {
    val index           = UInt(dcuIndexBits.W)
    val wen             = Bool()
    val wBufID          = UInt(dcuWBufIdBits.W)
}

// TODO: DMT
// TODO: DWT
class DCU(node: Node)(implicit p: Parameters) extends DJModule {
// ------------------------------------------ IO declaration --------------------------------------------- //
    val io = IO(new Bundle {
        val sn  = Flipped(new IcnBundle(node))
    })

    require(node.splitFlit)



// ----------------------------------------- Reg and Wire declaration ------------------------------------ //
    // CHI
    val txReq               = io.sn.tx.req.get.bits.asTypeOf(new ReqFlit)
    val txDat               = io.sn.tx.data.get.bits.asTypeOf(new DataFlit)
    val rxRsp               = WireInit(0.U.asTypeOf(new RespFlit))
    val rxDat               = WireInit(0.U.asTypeOf(new DataFlit))
    io.sn.rx.resp.get.bits  := rxRsp
    io.sn.rx.data.get.bits := rxDat

    // S0
    val reqIsW          = Wire(Bool())
    val wBufRegVec      = RegInit(VecInit(Seq.fill(djparam.nrDCUWBuf) { 0.U.asTypeOf(new DCUWEntry) }))
    val rReqQ           = Module(new Queue(new ReqFlit(), entries = djparam.nrDCUReqQ, flow = true, pipe = true)) // TODO: Cant automatic optimization

    // S1: earlyReq ---> multicylce_0(sram.req.fire) ---> multicycle_1 ---> ...... ---> muticycle_last(earlyReq)
    require(djparam.dcuMulticycle >= 2); require(djparam.dcuHoldMcp)
    val earlyReq        = Wire(Vec(djparam.nrDSBank, Valid(new SramReqBundle)))
    val selWenID        = Wire(UInt(dcuWBufIdBits.W))
    val sramReqReg      = RegInit(VecInit(Seq.fill(djparam.nrDSBank) { 0.U.asTypeOf(Valid(new SramReqBundle)) }))
    val shiftRegVec     = RegInit(VecInit(Seq.fill(djparam.nrDSBank) { 0.U.asTypeOf(new Bundle{
        val shift       = UInt(djparam.dcuMulticycle.W)
        val ren = Bool()
    }) }))
    dontTouch(shiftRegVec)

    // S2
    val rRespQ          = Module(new Queue(new DataFlit(), entries = djparam.nrDCURespQ, flow = false, pipe = true)) // TODO: Cant automatic optimization
    val rDatQ           = Module(new Queue(Vec(nrBeat, UInt(beatBits.W)), entries = djparam.nrDCURespQ, flow = false, pipe = true))
    val respBankReg     = Reg(UInt(bankBits.W))
    val respVec         = Wire(Vec(djparam.nrDSBank, Vec(nrBeat, UInt(beatBits.W))))
    val sendBeatNumReg  = RegInit(0.U(beatNumBits.W))

    // DataStorage
    val ds              = Seq.fill(djparam.nrDSBank) { Module(new SRAMTemplate(Vec(nrBeat, UInt(beatBits.W)), nrDSEntry, singlePort = true, multicycle = djparam.dcuMulticycle, holdMcp = djparam.dcuHoldMcp)) }



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------ S0: Receive Req From CHI -------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
    /*
     * Receive Req
     */
    val wBufFreeVec             = wBufRegVec.map(_.state === DCUWState.Free)
    val selRecWID               = PriorityEncoder(wBufFreeVec)
    reqIsW                      := isWriteX(txReq.Opcode)
    when(reqIsW) {
        io.sn.tx.req.get.ready := wBufFreeVec.reduce(_ | _)
        rReqQ.io.enq            <> DontCare
    }.otherwise {
        rReqQ.io.enq            <> io.sn.tx.req.get
    }


    /*
     * Send DBID To Src
     */
    val wBufSDBIDVec        = wBufRegVec.map(_.state === DCUWState.SendDBIDResp)
    val selSDBID            = PriorityEncoder(wBufSDBIDVec)

    io.sn.rx.resp.get.valid := wBufSDBIDVec.reduce(_ | _)
    rxRsp.Opcode            := DBIDResp
    rxRsp.TgtID             := wBufRegVec(selSDBID).srcID
    rxRsp.TxnID             := wBufRegVec(selSDBID).txnID
    rxRsp.DBID              := selSDBID


    /*
     * Write Req Buf
     */
    wBufRegVec.zipWithIndex.foreach {
        case(w, i) =>
            switch(w.state) {
                is(DCUWState.Free) {
                    val hit = io.sn.tx.req.get.fire & reqIsW & selRecWID === i.U
                    when(hit) {
                        w.state     := DCUWState.SendDBIDResp
                        w.dsIndex   := parseDCUAddress(txReq.Addr)._1
                        w.srcID     := txReq.SrcID
                        w.txnID     := txReq.TxnID
                    }
                }
                is(DCUWState.SendDBIDResp) {
                    val hit = io.sn.rx.resp.get.fire & rxRsp.Opcode === DBIDResp & selSDBID === i.U
                    when(hit) {
                        w.state := DCUWState.WaitData
                    }
                }
                is(DCUWState.WaitData) {
                    val hit = io.sn.tx.data.get.fire & txDat.TxnID === i.U
                    when(hit) {
                        w.state := Mux(PopCount(w.datVal) === (nrBeat-1).U, DCUWState.WriteData, w.state)
                        w.datVal(toBeatNum(txDat.DataID))   := true.B
                        w.data(toBeatNum(txDat.DataID))     := txDat.Data
                    }
                }
                is(DCUWState.WriteData) {
                    val hit = earlyReq.map { case e => e.valid & e.bits.wen }.reduce(_ | _) & selWenID === i.U
                    when(hit) {
                        w.state := DCUWState.Writting
                    }
                }
                is(DCUWState.Writting) {
                    w := 0.U.asTypeOf(w)
                }
            }
    }
    io.sn.tx.data.get.ready := true.B



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------- S1: Send Req To DataStorage ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
    /*
     * Select Req Send To SRAM
     */
    val wBufWenVec          = wBufRegVec.map(_.state === DCUWState.WriteData)
    selWenID                := PriorityEncoder(wBufWenVec)
    def canRecEarly(i: Int) = shiftRegVec(i).shift(djparam.dcuMulticycle-1, 1) === 0.U
    earlyReq.zipWithIndex.foreach {
        case(req, i) =>
            val wenHit      = wBufWenVec.reduce(_ | _)  & wBufRegVec(selWenID).dsBank === i.U
            val renHit      = rReqQ.io.deq.valid        & parseDCUAddress(rReqQ.io.deq.bits.Addr)._2(dsBankBits-1, 0) === i.U
            req.valid       := Mux(sramReqReg(i).valid, false.B, Mux(canRecEarly(i), wenHit | renHit, false.B))
            req.bits.wen    := wenHit
            req.bits.wBufID := selWenID
            req.bits.index  := Mux(wenHit, wBufRegVec(selWenID).dsIndex, parseDCUAddress(rReqQ.io.deq.bits.Addr)._1)
    }
    sramReqReg.zip(earlyReq).foreach {
        case(reg, early) =>
            reg.valid       := Mux(early.bits.wen, early.valid, early.valid & rRespQ.io.enq.ready)
            reg.bits        := early.bits
    }


    /*
     * Send Resp to rRespQ
     */
    rReqQ.io.deq.ready          := earlyReq.map { case e => e.valid & !e.bits.wen }.reduce(_ | _) & rRespQ.io.enq.ready
    rRespQ.io.enq.valid         := sramReqReg.map { case e => e.valid & !e.bits.wen }.reduce(_ | _)
    rRespQ.io.enq.bits          := DontCare
    rRespQ.io.enq.bits.Opcode   := CompData
    rRespQ.io.enq.bits.Resp     := ChiResp.UC
    rRespQ.io.enq.bits.TgtID    := rReqQ.io.deq.bits.SrcID
    rRespQ.io.enq.bits.TxnID    := rReqQ.io.deq.bits.TxnID


    /*
     * Send Req To SRAM
     */
    ds.zip(earlyReq).foreach {
        case(sram, req) =>
            sram.io.earlyWen.get := req.valid & req.bits.wen
            sram.io.earlyRen.get := req.valid & !req.bits.wen & rRespQ.io.enq.ready
    }
    ds.zip(sramReqReg).foreach {
        case(sram, req) =>
            // wen
            sram.io.w.req.valid         := req.valid & req.bits.wen
            sram.io.w.req.bits.setIdx   := req.bits.index
            sram.io.w.req.bits.data(0)  := wBufRegVec(req.bits.wBufID).data
            // ren
            sram.io.r.req.valid         := req.valid & !req.bits.wen
            sram.io.r.req.bits.setIdx   := req.bits.index
    }


    /*
     * Set Shift Reg Vec Value
     */
    shiftRegVec.zip(ds).foreach {
        case(s, sram) =>
            s.shift := Cat(sram.io.earlyRen.get | sram.io.earlyWen.get, s.shift(djparam.dcuMulticycle-1, 1))
            s.ren   := Mux(s.ren, !sram.io.earlyWen.get, sram.io.earlyRen.get)
    }


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------- S2: Receive SRAM Resp and Send RxDat ------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
    /*
     * Receive SRAM Resp
     */
    respBankReg         := OHToUInt(shiftRegVec.map { case s => s.shift(0) & s.ren })
    respVec             := ds.map(_.io.r.resp.data(0))
    rDatQ.io.enq.valid  := RegNext(shiftRegVec.map { case s => s.shift(0) & s.ren }.reduce(_ | _))
    rDatQ.io.enq.bits   := respVec(respBankReg)


    /*
     * Send Resp To CHI RxDat
     */
    sendBeatNumReg      := sendBeatNumReg + io.sn.rx.data.get.fire.asUInt
    io.sn.rx.data.get.valid := rRespQ.io.deq.valid & rDatQ.io.deq.valid
    rxDat               := rRespQ.io.deq.bits
    rxDat.Data          := rDatQ.io.deq.bits(sendBeatNumReg)
    rxDat.DataID        := toDataID(sendBeatNumReg)

    rRespQ.io.deq.ready := sendBeatNumReg === (nrBeat - 1).U & io.sn.rx.data.get.fire.asUInt
    rDatQ.io.deq.ready  := sendBeatNumReg === (nrBeat - 1).U & io.sn.rx.data.get.fire.asUInt



// ------------------------------------------------------------ Assertion ----------------------------------------------- //
    ds.foreach { case d =>
            assert(Mux(d.io.r.req.valid, d.io.r.req.ready, true.B))
            assert(Mux(d.io.w.req.valid, d.io.w.req.ready, true.B))
    }

    assert(Mux(io.sn.tx.req.get.valid, txReq.Addr(addressBits - 1, addressBits - sTagBits) === 0.U, true.B))

    assert(Mux(io.sn.tx.req.get.valid, txReq.Size === log2Ceil(djparam.blockBytes).U, true.B))

    shiftRegVec.foreach { case s => assert(PopCount(s.shift) <= 1.U) }

    assert(Mux(rDatQ.io.enq.valid, rDatQ.io.enq.ready, true.B))

}
