package DONGJIANG.DDRC

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
import xijiang.router.SnIcn
import xs.utils.sram._


object DDRCWState {
    val width = 2
    // commom
    val Free            = "b00".U
    val SendDBIDResp    = "b01".U
    val WaitData        = "b10".U
    val WriteData       = "b11".U
}

class DDRCWEntry(implicit p: Parameters) extends DJBundle {
    val state           = UInt(DDRCWState.width.W)
    val datVal          = Vec(nrBeat, Bool())
    val data            = Vec(nrBeat, UInt(beatBits.W))
    val index           = UInt(64.W)
    val srcID           = UInt(djparam.chiNodeIdBits.W)
    val txnID           = UInt(djparam.chiTxnidBits.W)
}

// TODO: DMT
// TODO: DWT
class FakeDDRC(node: Node)(implicit p: Parameters) extends DJModule {
// ------------------------------------------ IO declaration --------------------------------------------- //
    val io = IO(new Bundle {
        val sn  = Flipped(new SnIcn(node))
    })

    require(node.splitFlit)

    def getDDRCAddress(x: UInt) = x >> offsetBits
    require(addressBits - offsetBits < 64)

// ----------------------------------------- Reg and Wire declaration ------------------------------------ //
    // CHI
    val txReq           = io.sn.tx.req.bits.asTypeOf(new ReqFlit)
    val txDat           = io.sn.tx.data.bits.asTypeOf(new DataFlit)
    val rxRsp           = WireInit(0.U.asTypeOf(new RespFlit))
    val rxDat           = WireInit(0.U.asTypeOf(new DataFlit))
    io.sn.rx.resp.bits  := rxRsp
    io.sn.rx.data.bits  := rxDat

    // S0
    val reqIsW          = Wire(Bool())
    val wBufRegVec      = RegInit(VecInit(Seq.fill(nrDDRCWBuf) { 0.U.asTypeOf(new DDRCWEntry) }))
    val rReqQ           = Module(new Queue(new ReqFlit(), entries = nrDDRCReqQ, flow = true, pipe = true)) // TODO: Cant automatic optimization


    // S2
    val rRespQ          = Module(new Queue(new DataFlit(), entries = nrDDRCRespQ, flow = false, pipe = true)) // TODO: Cant automatic optimization
    val rDatQ           = Module(new Queue(Vec(nrBeat, UInt(beatBits.W)), entries = nrDDRCRespQ, flow = false, pipe = true))
    val sendBeatNumReg  = RegInit(0.U(beatNumBits.W))

    // Fake DDR
    val ddr             = Seq.fill(nrDDRCBank) { Module(new MemHelper()) }



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------ S0: Receive Req From CHI -------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
    /*
     * Receive Req
     */
    val wBufFreeVec         = wBufRegVec.map(_.state === DDRCWState.Free)
    val selRecWID           = PriorityEncoder(wBufFreeVec)
    reqIsW                  := isWriteX(txReq.Opcode)
    when(reqIsW) {
        io.sn.tx.req.ready  := wBufFreeVec.reduce(_ | _)
        rReqQ.io.enq        <> DontCare
    }.otherwise {
        rReqQ.io.enq        <> io.sn.tx.req
    }


    /*
     * Send DBID To Src
     */
    val wBufSDBIDVec        = wBufRegVec.map(_.state === DDRCWState.SendDBIDResp)
    val selSDBID            = PriorityEncoder(wBufSDBIDVec)

    io.sn.rx.resp.valid     := wBufSDBIDVec.reduce(_ | _)
    rxRsp.Opcode            := CompDBIDResp
    rxRsp.TgtID             := wBufRegVec(selSDBID).srcID
    rxRsp.TxnID             := wBufRegVec(selSDBID).txnID
    rxRsp.DBID              := selSDBID


    /*
     * Write Req Buf
     */
    wBufRegVec.zipWithIndex.foreach {
        case(w, i) =>
            switch(w.state) {
                is(DDRCWState.Free) {
                    val hit = io.sn.tx.req.fire & reqIsW & selRecWID === i.U
                    when(hit) {
                        w.state     := DDRCWState.SendDBIDResp
                        w.index     := getDDRCAddress(txReq.Addr)
                        w.srcID     := txReq.SrcID
                        w.txnID     := txReq.TxnID
                    }.otherwise {
                        w           := 0.U.asTypeOf(w)
                    }
                }
                is(DDRCWState.SendDBIDResp) {
                    val hit = io.sn.rx.resp.fire & rxRsp.Opcode === CompDBIDResp & selSDBID === i.U
                    when(hit) {
                        w.state := DDRCWState.WaitData
                    }
                }
                is(DDRCWState.WaitData) {
                    val hit = io.sn.tx.data.fire & txDat.TxnID === i.U
                    when(hit) {
                        w.state := Mux(PopCount(w.datVal) === (nrBeat-1).U, DDRCWState.WriteData, w.state)
                        w.datVal(toBeatNum(txDat.DataID))   := true.B
                        w.data(toBeatNum(txDat.DataID))     := txDat.Data
                    }
                }
                is(DDRCWState.WriteData) {
                    w.state := DDRCWState.Free
                }
            }
    }
    io.sn.tx.data.ready := true.B



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------ S1: Send Req To DDRC ------------------------------------------------ //
// ---------------------------------------------------------------------------------------------------------------------- //
    /*
     * Select Req Send To SRAM
     */
    val wDataVec    = wBufRegVec.map(_.state === DDRCWState.WriteData)
    val wDataId     = PriorityEncoder(wDataVec)
    assert(PopCount(wDataVec) <= 1.U)

    ddr.foreach { case d => d.clk := clock }
    ddr.foreach {
        case d =>
            d.rIdx  := getDDRCAddress(rReqQ.io.deq.bits.Addr)
            d.ren   := rReqQ.io.deq.fire
    }
    ddr.zipWithIndex.foreach {
        case(d, i) =>
            d.wIdx  := wBufRegVec(wDataId).index
            d.wen   := wDataVec.reduce(_ | _)
            d.wdata := Cat(wBufRegVec(wDataId).data).asTypeOf(Vec(nrDDRCBank, UInt(64.W)))(i)
    }


    /*
     * Send Resp to rRespQ
     */
    rReqQ.io.deq.ready          := rRespQ.io.enq.ready
    rRespQ.io.enq.valid         := rReqQ.io.deq.valid
    rRespQ.io.enq.bits          := DontCare
    rRespQ.io.enq.bits.Opcode   := CompData
    rRespQ.io.enq.bits.Resp     := ChiResp.UC
    rRespQ.io.enq.bits.TgtID    := rReqQ.io.deq.bits.SrcID
    rRespQ.io.enq.bits.TxnID    := rReqQ.io.deq.bits.TxnID



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------- S2: Receive DRRC Resp and Send RxDat ------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
    /*
     * Receive SRAM Resp
     */
    rDatQ.io.enq.valid  := rReqQ.io.deq.fire
    rDatQ.io.enq.bits   := Cat(ddr.map(_.rdata)).asTypeOf(Vec(nrBeat, UInt(beatBits.W)))

    /*
     * Send Resp To CHI RxDat
     */
    sendBeatNumReg      := sendBeatNumReg + io.sn.rx.data.fire.asUInt
    io.sn.rx.data.valid := rRespQ.io.deq.valid & rDatQ.io.deq.valid
    rxDat               := rRespQ.io.deq.bits
    rxDat.Data          := rDatQ.io.deq.bits(sendBeatNumReg)
    rxDat.DataID        := toDataID(sendBeatNumReg)

    rRespQ.io.deq.ready := sendBeatNumReg === (nrBeat - 1).U & io.sn.rx.data.fire.asUInt
    rDatQ.io.deq.ready  := sendBeatNumReg === (nrBeat - 1).U & io.sn.rx.data.fire.asUInt



// ----------------------------------------------------- Assertion ------------------------------------------------------ //
    assert(Mux(io.sn.tx.req.valid, txReq.Addr(offsetBits - 1, 0) === 0.U, true.B))

    assert(Mux(io.sn.tx.req.valid, txReq.Size === log2Ceil(djparam.blockBytes).U, true.B))

    assert(Mux(rDatQ.io.enq.valid, rDatQ.io.enq.ready, true.B))

}
