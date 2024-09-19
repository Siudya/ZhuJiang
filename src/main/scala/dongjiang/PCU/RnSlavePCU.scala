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
 * ************************************************************** State transfer ***********************************************************************************
 *
 * Req Retry:             [Free]  -----> [Req2Slice] -----> [WaitSliceAck] ---retry---> [Req2Slice]
 * Req Receive:           [Free]  -----> [Req2Slice] -----> [WaitSliceAck] --receive--> [Free]
 * Req Nest By Snp case:            ^                  ^
 *                             waitSnpDone        WaitSnpDone
 *
 *
 * Resp Need Read DB:     [Free]  -----> [RCDB] -----> [Resp2Node] -----> [WaitCompAck] -----> [Resp2Slice]
 * Resp Not Need Read DB: [Free]         ----->        [Resp2Node] -----> [WaitCompAck] -----> [Resp2Slice]
 *
 *
 *
 * Write Retry:           [Free]  -----> [GetDBID] -----> [WaitDBID] -----> [DBIDResp2Node] -----> [WaitData] -----> [Req2Slice] -----> [WaitSliceAck] ---retry---> [Req2Slice]
 * Write:                 [Free]  -----> [GetDBID] -----> [WaitDBID] -----> [DBIDResp2Node] -----> [WaitData] -----> [Req2Slice] -----> [WaitSliceAck] --receive--> [Free]
 * Write Nest By Snp case:          ^                 ^                     ^                 ^                  ^                 ^
 *                              waitWBDone       waitWBDone             waitWBDone        waitWBDone  ----->  trans2Snp        trans2Snp
 *
 *
 * Snoop Need Data:       [Free]  -----> [GetDBID] -----> [WaitDBID] -----> [Snp2Node] -----> [WaitSnpResp] -----> [Resp2Slice]
 * Snoop No Need Data:    [Free]                   ----->                   [Snp2Node] -----> [WaitSnpResp] -----> [Resp2Slice]
 * Snoop Nest Write 0:    [Free]  -----> [Snp2Node] -----> [WaitSnpResp] -----> [SendResp2Slice]
 * Snoop Nest Write 1:    [Free]  -----> [Resp2Slice] * only need to snp one node
 *
 *
 *
 * ************************************************************** PCU Entry Transfer ********************************************************************************
 * Req:
 * [------] <- Req0x0 | [Req0x0] -> Req to slice | [------] <- Resp from slice | [Rsp0x0] -> resp to node
 * [------]           | [------]                 | [------]                    | [------]
 * [------]           | [------]                 | [------]                    | [------]
 * [------]           | [------]                 | [------]                    | [------]
 *
 *
 * Write:
 * [------] <- Wri0x0 | [Wri0x0] -> Wri to slice | [Wri0x0] <- Resp from slice | [------]
 * [------]           | [------]                 | [------]                    | [------]
 * [------]           | [------]                 | [------]                    | [------]
 * [------]           | [------]                 | [------]                    | [------]
 *
 *
 * Snoop:
 * [Snp0x0] <- Snp0x0 | [Snp0x0] -> Snp to node  | [Snp0x0] <- Resp from node  | [------] -> resp to slice
 * [------]           | [------]                 | [------]                    | [------]
 * [------]           | [------]                 | [------]                    | [------]
 * [------]           | [------]                 | [------]                    | [------]
 *
 *
 * * Req Nest By Snp:
 * [------] <- Req0x0 | [Req0x0]                 | [Req0x0] need to wait snoop done
 * [------]           | [------] <-Snp0x0        | [Snp0x0]
 * [------]           | [------]                 | [------]
 * [------]           | [------]                 | [------]
 *
 *
 * * Write Nest By Snp:
 * [------] <- Wri0x0 | [Wri0x0] <-Snp0x0        | [Wri0x0] need to wait write done and transfer write to snoop
 * [------]           | [------]                 | [------]
 * [------]           | [------]                 | [------]
 * [------]           | [------]                 | [------]
 *
 */

object PCURS {
  val width = 4
  // commom
  val Free            = "b0000".U
  val Req2Slice       = "b0001".U
  val WaitSliceAck    = "b0010".U
  val WaitSliceResp   = "b0011".U
  val RCDB            = "b0100".U // Read & Clean DataBuffer
  val Resp2Node       = "b0101".U
  val WaitCompAck     = "b0110".U
  val GetDBID         = "b0111".U
  val WaitDBID        = "b1000".U
  val DBIDResp2Node   = "b1001".U
  val WaitData        = "b1010".U
  val Snp2Node        = "b1011".U
  val WaitSnpResp     = "b1100".U
}

class PCURSEntry(param: InterfaceParam)(implicit p: Parameters) extends DJBundle  {
  val state         = UInt(PCURS.width.W)
  val nid           = UInt(param.pcuIdBits.W)
  val indexMes      = new DJBundle with HasAddr with HasFromIncoID with HasMSHRWay
  val nestMes       = new Bundle {
    val waitSnpDone = Bool()
    val waitWBDone  = Bool()
    val trans2Snp   = Bool()
  }
  val chiMes        = new DJBundle {
    val opcode      = UInt(6.W)
    val txnId       = UInt(djparam.chiTxnidBits.W)
    val tgtId       = UInt(djparam.chiNodeIdBits.W)
    val srcId       = UInt(djparam.chiNodeIdBits.W)
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
  val pcus            = RegInit(VecInit(Seq.fill(param.nrPCUEntry) { 0.U.asTypeOf(new PCURSEntry(param)) }))
  // PCU Receive Task ID
  val pcuFreeID       = Wire(UInt(param.pcuIdBits.W))
  // PCU Send Req To Slice
  val pcuSendReqID    = Wire(UInt(param.pcuIdBits.W))
  // req from slice or txreq
  val taskNID         = Wire(UInt(param.pcuIdBits.W))
  val taskSaveInPCU   = WireInit(0.U.asTypeOf(pcus(0).chiMes))
  val indexSaveInPCU  = WireInit(0.U.asTypeOf(pcus(0).indexMes))






// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- PCU: State Transfer ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  pcus.zipWithIndex.foreach {
    case(pcu, i) =>
      switch(pcu.state) {
        // State: Free
        is(PCURS.Free) {
          val hit       = pcuFreeID === i.U
          val reqHit    = io.chi.txreq.fire & !isWriteX(io.chi.txreq.bits.Opcode) & hit
          val writeHit  = io.chi.txreq.fire & isWriteX(io.chi.txreq.bits.Opcode) & hit
          val snpHit    = io.req2Node.fire & hit
          val respHit   = io.resp2Node.fire & hit
          val rDB       = io.resp2Node.bits.needReadDB; assert(Mux(hit, !rDB, true.B), "TODO")
          pcu.state     := Mux(reqHit, PCURS.Req2Slice,
                            Mux(snpHit, PCURS.Snp2Node,
                              Mux(writeHit, PCURS.GetDBID,
                                Mux(respHit & rDB, PCURS.RCDB,
                                  Mux(respHit & !rDB, PCURS.Resp2Node, pcu.state)))))
          assert(PopCount(Seq(reqHit, writeHit, snpHit, respHit)) <= 1.U)
        }
        // State: Req2Slice
        is(PCURS.Req2Slice) {
          val hit       = io.req2Slice.fire & pcuSendReqID === i.U
          pcu.state     := Mux(hit, PCURS.WaitSliceAck, pcu.state)
        }
        // State: WaitSliceAck
        is(PCURS.WaitSliceAck) {
          val hit       = io.reqAck2Node.fire & io.reqAck2Node.bits.pcuId === i.U
          pcu.state     := Mux(hit, Mux(io.reqAck2Node.bits.retry, PCURS.Req2Slice, PCURS.Free), pcu.state)
        }
      }
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------- PCU: Update PCU Entry Value  ----------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  pcus.zipWithIndex.foreach {
    case (pcu, i) =>
      /*
       * Receive New Req
       */
      when((io.chi.txreq.fire | io.req2Node.fire | io.resp2Node.fire) & pcuFreeID === i.U) {
        pcu.indexMes  := indexSaveInPCU
        pcu.chiMes    := taskSaveInPCU
        pcu.nid       := taskNID
        pcu.nestMes   := 0.U.asTypeOf(pcu.nestMes)
        assert(pcu.state === PCURS.Free)
      }
  }



// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------- Receive Req From CHITXREQ, Req2Node From Slice or Resp From Slice------------------------------ //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Get PCU Free ID
   */
  val pcuFreeVec  = pcus.map(_.isFree)
  val pcuFreeNum  = PopCount(pcuFreeVec)
  pcuFreeID       := PriorityEncoder(pcuFreeVec)

  /*
   * Receive req2Node(Snoop)
   */
  val reqVal        = io.chi.txreq.valid
  val snpVal        = io.req2Node.valid
  val respVal       = io.resp2Node.valid
  val addrMatchVec  = pcus.map(_.indexMes.addr(addressBits-1, offsetBits) === indexSaveInPCU.addr(addressBits-1, offsetBits))
  val taskMatchVec  = pcuFreeVec.zip(addrMatchVec).map{ case(a, b) => !a & b }
  //                                  | RESP                                | SNP                                     | REQ
  taskNID                       := Mux(respVal, 0.U,                        Mux(snpVal, 0.U,                          PopCount(taskMatchVec)))
  indexSaveInPCU.from           := Mux(respVal, io.resp2Node.bits.from,     Mux(snpVal, io.req2Node.bits.from,        DontCare))
  indexSaveInPCU.mshrWay        := Mux(respVal, io.resp2Node.bits.mshrWay,  Mux(snpVal, io.req2Node.bits.mshrWay,     DontCare))
  indexSaveInPCU.useEvict       := Mux(respVal, io.resp2Node.bits.useEvict, Mux(snpVal, io.req2Node.bits.useEvict,    DontCare))
  taskSaveInPCU.opcode          := Mux(respVal, io.resp2Node.bits.opcode,   Mux(snpVal, io.req2Node.bits.opcode,      io.chi.txreq.bits.Opcode))
  taskSaveInPCU.tgtId           := Mux(respVal, io.resp2Node.bits.tgtID,    Mux(snpVal, io.req2Node.bits.tgtId,       DontCare))
  taskSaveInPCU.txnId           := Mux(respVal, io.resp2Node.bits.txnID,    Mux(snpVal, io.req2Node.bits.txnId,       io.chi.txreq.bits.TxnID))
  taskSaveInPCU.srcId           := Mux(respVal, io.resp2Node.bits.srcID,    Mux(snpVal, io.req2Node.bits.srcId,       io.chi.txreq.bits.SrcID))
  taskSaveInPCU.snpRetToSrc     := Mux(respVal, DontCare,                   Mux(snpVal, io.req2Node.bits.retToSrc,    DontCare))
  taskSaveInPCU.snpDoNotGoToSD  := Mux(respVal, DontCare,                   Mux(snpVal, io.req2Node.bits.doNotGoToSD, DontCare))
  assert(Mux(reqVal | snpVal | respVal, !taskMatchVec.reduce(_ | _), true.B), "TODO")

  /*
   * Set Ready Value
   */
  io.chi.txreq.ready    := pcuFreeNum >= param.nrPCUEvictEntry.U & !io.req2Node.valid
  io.req2Node.ready     := pcuFreeNum > 0.U & !io.resp2Node.valid
  io.resp2Node.ready    := pcuFreeNum > 0.U
  io.reqAck2Node.ready  := true.B


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
  io.req2Slice.bits.opcode      := pcus(pcuSendReqID).chiMes.opcode
  io.req2Slice.bits.addr        := pcus(pcuSendReqID).indexMes.addr
  io.req2Slice.bits.isSnp       := false.B
  io.req2Slice.bits.srcID       := pcus(pcuSendReqID).chiMes.srcId
  io.req2Slice.bits.txnID       := pcus(pcuSendReqID).chiMes.txnId
  // IdMap
  io.req2Slice.bits.to.IncoId   := pcus(pcuSendReqID).indexMes.mBank
  io.req2Slice.bits.from.IncoId := rnSlvId.U
  io.req2Slice.bits.pcuId       := pcuSendReqID
  // Use in RnMaster
  io.req2Slice.bits.retToSrc    := false.B
  io.req2Slice.bits.doNotGoToSD := false.B





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
  cntReg.zipWithIndex.foreach { case(c, i) => assert(c < TIMEOUT_RSPCU.U, "RNSLV PCU[0x%x] ADDR[0x%x] OP[0x%x] TIMEOUT", i.U, pcus(i).indexMes.addr, pcus(i).chiMes.opcode) }
}