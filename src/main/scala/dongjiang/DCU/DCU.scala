package DONGJIANG.DCU

import zhujiang.chi._
import xijiang.Node
import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIOp.REQ._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import xijiang.router.SnIcn
import xs.utils.sram._


object DCUWState {
    val width = 2
    // commom
    val Free            = "b00".U
    val SendDBIDResp    = "b01".U
    val WaitData        = "b10".U
    val WriteData       = "b11".U // Write Data To DataStroage
}

class DCUWEntry(implicit p: Parameters) extends DJBundle {
    val state           = UInt(DCUWState.width.W)
    val datas           = Vec(nrBeat, Valid(UInt(beatBits.W)))
    val dsIndex         = UInt(dsIndexBits.W)
    val dsBank          = UInt(dsBankBits.W)
    val srcID           = UInt(djparam.nodeIdBits.W)

    def getDSIndex(x: UInt) = {
        this.dsIndex    := x(dsIndexBits + dsBankBits - 1, dsBankBits)
        this.dsBank     := x(dsBankBits - 1, 0)
    }
}

// TODO: DMT
// TODO: DWT
class DCU(node: Node)(implicit p: Parameters) extends DJModule {
// ------------------------------------------ IO declaration --------------------------------------------- //
    val io = IO(new Bundle {
        val sn  = Flipped(new SnIcn(node))
    })

    // TODO: Del it
    io <> DontCare

//// ----------------------------------------- Reg and Wire declaration ------------------------------------ //
//    val reqIsW          = Wire(Bool())
//    val wBufReg         = RegInit(VecInit(Seq.fill(djparam.nrDCUWBuf) { 0.U.asTypeOf(new DCUWEntry) }))
//    val rReqQ           = Module(new Queue(new ReqFlit(), entries = djparam.nrDCURQ, flow = true, pipe = true))
//    val rRespQ          = Module(new Queue(new RespFlit(), entries = 4, flow = false, pipe = true))
//    val rxDat           = WireInit(0.U.asTypeOf(new DataFlit))
//    val ds              = Seq.fill(djparam.nrDSBank) {
//                            Seq.fill(nrBeat) {
//                                Module(new SRAMTemplate(UInt(beatBits.W), nrDSEntry, singlePort = true, multicycle = djparam.dcuMulticycle, holdMcp = djparam.dcuHoldMcp)) } }
//
//    rReqQ.io <> DontCare
//    rRespQ.io <> DontCare
//    ds.foreach(_.foreach(_.io <> DontCare))
//
//// ---------------------------------------------------------------------------------------------------------------------- //
//// ------------------------------------------------ S0: Receive Req From CHI -------------------------------------------- //
//// ---------------------------------------------------------------------------------------------------------------------- //
//    /*
//     * Receive Req
//     */
//    val txReq           = io.sn.tx.req.bits.asTypeOf(new ReqFlit())
//    val wBufFreeVec     = wBufReg.map(_.state === DCUWState.Free)
//    val selRecWID       = PriorityEncoder(wBufFreeVec)
//    reqIsW              := isWriteX(txReq.Opcode)
//    when(reqIsW) {
//        io.sn.tx.req.ready  := wBufFreeVec.reduce(_ | _)
//    }.otherwise {
////        rReqQ.io.enq        <> io.sn.tx.req
//    }
//
//    /*
//     * Write Req Buf
//     */
//    wBufReg.zipWithIndex.foreach {
//        case(w, i) =>
//            switch(w.state) {
//                is(DCUWState.Free) {
//                    val hit = io.sn.tx.req.fire & reqIsW & selRecWID === i.U
//                    when(hit) {
//                        w.state := DCUWState.SendDBIDResp
//                        w.getDSIndex(txReq.Addr)
//                        w.srcID := txReq.SrcID
//                    }
//                }
//            }
//    }
//
//    /*
//     * Send DBID To Src
//     */
//    val wBufSDBIDVec    = wBufReg.map(_.state === DCUWState.SendDBIDResp)
//    val selSDBID        = PriorityEncoder(wBufSDBIDVec)
//
//    io.sn.rx.resp.valid := wBufSDBIDVec.reduce(_ | _)



// ----------------------------------------- For Test ------------------------------------ //

}
