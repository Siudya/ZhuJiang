package DONGJIANG.PCU

import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIOp.REQ._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.FastArb._
import xs.utils._

/*
 * Req Retry:             [Free]  -----> [Req2Slice] -----> [WaitSliceResp] --retry--> [Req2Slice]
 * Req:                   [Free]  -----> [Req2Slice] -----> [WaitSliceResp] -----> [RCDB] -----> [Resp2Node] -----> [WaitCompAck] -----> [Resp2Slice]
 * Req Nest By Snp case:            ^                  ^
 *                             waitSnpDone        WaitSnpDone
 *
 *
 * Write Retry:           [Free]  -----> [GetDBID] -----> [WaitDBID] -----> [DBIDResp2Node] -----> [WaitData] -----> [Req2Slice] -----> [WaitSliceResp] --retry--> [Req2Slice]
 * Write:                 [Free]  -----> [GetDBID] -----> [WaitDBID] -----> [DBIDResp2Node] -----> [WaitData] -----> [Req2Slice] -----> [WaitSliceResp]
 * Write Nest By Snp case:          ^                 ^                     ^                 ^                  ^                        ^
 *                              waitWBDone       waitWBDone             waitWBDone        waitWBDone  ----->  trans2Snp                trans2Snp
 *
 *
 * Snoop:                 [Free]  -----> [GetDBID] -----> [WaitDBID] -----> [Snp2Node] -----> [WaitSnpResp] -----> [Resp2Slice]
 * Snoop Nest Write 0:    [Free]  -----> [Snp2Node] -----> [WaitSnpResp] -----> [SendResp2Slice]
 * Snoop Nest Write 1:    [Free]  -----> [Resp2Slice] * only need to snp one node
 *
 *
 * * Req Nest By Snp:
 * [------] <- Req0x0 | [Req0x0]            | [Req0x0] need to wait snoop done
 * [------]           | [------] <-Snp 0x0  | [Snp0x0]
 * [------]           | [------]            | [------]
 * [------]           | [------]            | [------]
 *
 * * Write Nest By Snp:
 * [------] <- Wri0x0 | [Wri0x0] <-Snp 0x0  | [Wri0x0] need to wait write done and transfer write to snoop
 * [------]           | [------]            | [------]
 * [------]           | [------]            | [------]
 * [------]           | [------]            | [------]
 *
 */

object PCURS {
  val width = 4
  // commom
  val Free            = "b0000".U
  val Req2Slice       = "b0001".U
  val WaitSliceResp   = "b0010".U
  val RCDB            = "b0011".U // Read & Clean DataBuffer
  val Resp2Node       = "b0100".U
  val WaitCompAck     = "b0101".U
  val GetDBID         = "b0111".U
  val WaitDBID        = "b1000".U
  val DBIDResp2Node   = "b1001".U
  val WaitData        = "b1010".U
  val Snp2Node        = "b1011".U
  val WaitSnpResp     = "b1100".U
}

class PCURSEntry(param: InterfaceParam)(implicit p: Parameters) extends DJBundle with HasAddr {
  val state         = UInt(PCURS.width.W)
  // TODO: Setting the Sort ID
  val nestMes       = new Bundle {
    val waitSnpDone = Bool()
    val waitWBDone  = Bool()
    val trans2Snp   = Bool()
  }
  val reqMes        = new DJBundle with HasFromIDBits with HasMSHRWay {
    val opcode      = UInt(6.W)
    val txnId       = UInt(djparam.txnidBits.W)
    val tgtId       = UInt(djparam.nodeIdBits.W)
    val srcId       = UInt(djparam.nodeIdBits.W)
    val snpRetToSrc    = Bool()
    val snpDoNotGoToSD = Bool()
  }

  def isFree        = state === PCURS.Free
  def isReqBeSend   = state === PCURS.Req2Slice & !nestMes.asUInt.orR
}




class RnSlavePCU(rnSlvId: Int, param: InterfaceParam)(implicit p: Parameters) extends PCUBaseIO(isSlv = true, hasReq2Slice = true, hasDBRCReq = true) {
  // Del it
  io <> DontCare
  dontTouch(io)
// --------------------- Reg and Wire declaration ------------------------//
  val pcus          = RegInit(VecInit(Seq.fill(param.nrPCUEntry) { 0.U.asTypeOf(new PCURSEntry(param)) }))
  // PCU Receive Req ID
  val pcuGetReqID   = Wire(UInt(param.pcuIdBits.W))
  // PCU Send Req To Slice
  val pcuSendReqID  = Wire(UInt(param.pcuIdBits.W))
  // req from slice or txreq
  val req2Node      = WireInit(0.U.asTypeOf(pcus(0).reqMes))
  val reqFTxReq     = WireInit(0.U.asTypeOf(pcus(0).reqMes))
  val addrSaveInPCU = Wire(UInt(addressBits.W))
  val reqSaveInPCU  = WireInit(0.U.asTypeOf(pcus(0).reqMes))





// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- PCU: State Transfer ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  pcus.zipWithIndex.foreach {
    case(pcu, i) =>
      switch(pcu.state) {
        // State: Free
        is(PCURS.Free) {
          val reqHit    = io.chi.txreq.fire & !isWriteX(io.chi.txreq.bits.Opcode) & pcuGetReqID === i.U
          val snpHit    = io.req2Node.fire & pcuGetReqID === i.U
          val writeHit  = io.chi.txreq.fire & isWriteX(io.chi.txreq.bits.Opcode) & pcuGetReqID === i.U
          pcu.state     := Mux(reqHit, PCURS.Req2Slice,
                            Mux(snpHit, PCURS.Snp2Node,
                              Mux(writeHit, PCURS.GetDBID, pcu.state)))
        }
        // State: Req2Slice
        is(PCURS.Req2Slice) {
          val hit   = io.req2Slice.fire & pcuSendReqID === i.U
          pcu.state := Mux(hit, PCURS.WaitSliceResp, pcu.state)
        }
      }
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------- PCU: Update MHSR Table Value  ----------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  pcus.zipWithIndex.foreach {
    case (pcu, i) =>
      /*
       * Receive New Req
       */
      when((io.chi.txreq.fire | io.req2Node.fire) & pcuGetReqID === i.U) {
        pcu.addr    := addrSaveInPCU
        pcu.reqMes  := reqSaveInPCU
        assert(pcu.state === PCURS.Free)
      }
  }



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------ Receive Req From CHITXREQ or Req2Node and Save In PCU---------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Receive req2Node(Snoop)
   */
  req2Node.opcode     := io.req2Node.bits.opcode
  req2Node.mshrWay    := io.req2Node.bits.mshrWay
  req2Node.useEvict   := io.req2Node.bits.useEvict
  req2Node.from       := io.req2Node.bits.from
  req2Node.tgtId      := io.req2Node.bits.tgtId
  req2Node.txnId      := io.req2Node.bits.txnId
  req2Node.srcId      := io.req2Node.bits.srcId
  req2Node.snpRetToSrc    := io.req2Node.bits.retToSrc
  req2Node.snpDoNotGoToSD := io.req2Node.bits.doNotGoToSD

  /*
   * Receive CHITXREQ(Read / Dataless / Atomic / CMO)
   */
  reqFTxReq.opcode    := io.chi.txreq.bits.Opcode
  reqFTxReq.txnId     := io.chi.txreq.bits.TxnID
  reqFTxReq.srcId     := io.chi.txreq.bits.SrcID

  /*
   * Set PCU Value
   */
  val pcuFreeVec      = pcus.map(_.isFree)
  val pcuFreeNum      = PopCount(pcuFreeVec)
  pcuGetReqID         := PriorityEncoder(pcuFreeVec)
  addrSaveInPCU       := Mux(io.req2Node.valid, io.req2Node.bits.addr, io.chi.txreq.bits.Addr)
  reqSaveInPCU        := Mux(io.req2Node.valid, req2Node, reqFTxReq)

  /*
   * Set Ready Value
   */
  io.chi.txreq.ready  := pcuFreeNum >= 1.U & !io.req2Node.valid
  io.req2Node.ready   := pcuFreeNum > 0.U





// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------- Select PCU send Req to Slice --------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Select one PCU
   */
  val reqBeSendVec  = pcus.map(_.isReqBeSend)
  pcuSendReqID      := PriorityEncoder(reqBeSendVec)

  /*
   * Send Req To Node
   */
  io.req2Slice.valid            := reqBeSendVec.reduce(_ | _)
  io.req2Slice.bits.opcode      := pcus(pcuSendReqID).reqMes.opcode
  io.req2Slice.bits.addr        := pcus(pcuSendReqID).addr
  io.req2Slice.bits.isSnp       := false.B
  io.req2Slice.bits.srcID       := pcus(pcuSendReqID).reqMes.srcId
  io.req2Slice.bits.txnID       := pcus(pcuSendReqID).reqMes.txnId
  // IdMap
  io.req2Slice.bits.to.idL1     := pcus(pcuSendReqID).mBank
  io.req2Slice.bits.to.idL2     := DontCare
  io.req2Slice.bits.from.idL1   := rnSlvId.U
  io.req2Slice.bits.from.idL2   := pcuSendReqID
  // Use in RnMaster
  io.req2Slice.bits.retToSrc    := DontCare
  io.req2Slice.bits.doNotGoToSD := DontCare




// ---------------------------  For Test  -------------------------------- //
//  io.chi.rxdat.valid        := io.chi.txreq.valid | RegNext(io.chi.txreq.valid)
//  io.chi.rxdat.bits         := DontCare
//  io.chi.rxdat.bits.Opcode  := CHIOp.DAT.CompData
//  io.chi.rxdat.bits.TgtID   := pcus(0).reqMes.srcId
//  io.chi.rxdat.bits.SrcID   := hnfNodeId.U
//  io.chi.rxdat.bits.TxnID   := pcus(0).reqMes.txnId
//  io.chi.rxdat.bits.DBID    := 0.U
//  io.chi.rxdat.bits.HomeNID := 0.U
//  io.chi.rxdat.bits.Resp    := ChiResp.UC
//  io.chi.rxdat.bits.DataID  := Mux(io.chi.txreq.valid, "b00".U, "b10".U)
//  io.chi.rxdat.bits.Data    := 0.U
//  io.chi.rxdat.bits.BE      := Fill(io.chi.rxdat.bits.BE.getWidth, 1.U(1.W))
//
//  io.chi.txrsp.ready        := true.B

// ---------------------------  Assertion  -------------------------------- //
  val cntReg = RegInit(VecInit(Seq.fill(param.nrPCUEntry) { 0.U(64.W) }))
  cntReg.zip(pcus).foreach { case(c, p) => c := Mux(p.isFree, 0.U, c + 1.U) }
  cntReg.zipWithIndex.foreach { case(c, i) => assert(c < TIMEOUT_RSPCU.U, "RNSLV PCU[0x%x] ADDR[0x%x] OP[0x%x] TIMEOUT", i.U, pcus(i).addr, pcus(i).reqMes.opcode) }
}