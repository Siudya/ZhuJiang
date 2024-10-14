package DONGJIANG.PCU

import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIOp.REQ._
import DONGJIANG.CHI.CHIOp.RSP._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.Encoder._
import xs.utils._

/*
 * ************************************************************** State transfer ***********************************************************************************
 *
 * Req Retry:             [Free]  -----> [Req2Slice] -----> [WaitSliceAck] ---retry---> [Req2Slice]
 * Req Receive:           [Free]  -----> [Req2Slice] -----> [WaitSliceAck] --receive--> [Free]
 *
 *
 * Resp Need Read DB:     [Free]  -----> [RCDB] -----> [Resp2Node] -----> [WaitCompAck]
 * Resp Not Need Read DB: [Free]         ----->        [Resp2Node] -----> [WaitCompAck]
 *
 *
 * Write Retry:           [Free]  -----> [GetDBID] -----> [WaitDBID] -----> [DBIDResp2Node] -----> [WaitData] -----> [Req2Slice] -----> [WaitSliceAck] ---retry---> [Req2Slice]
 * Write:                 [Free]  -----> [GetDBID] -----> [WaitDBID] -----> [DBIDResp2Node] -----> [WaitData] -----> [Req2Slice] -----> [WaitSliceAck] --receive--> [Free]
 * Write Nest By Snp case:          ^                 ^                     ^                 ^                  ^                 ^
 *                              waitWBDone       waitWBDone             waitWBDone        waitWBDone  ----->  trans2Snp        trans2Snp
 *
 *
 * Snoop Need Data:       [Free]  -----> [GetDBID] -----> [WaitDBID] -----> [Snp2Node] -----> ([Snp2NodeIng]) -----> [WaitSnpResp] -----> [Resp2Slice]
 * Snoop No Need Data:    [Free]                   ----->                   [Snp2Node] -----> ([Snp2NodeIng]) -----> [WaitSnpResp] -----> [Resp2Slice]
 * Snoop Nest Write 0:    [XXXX]                   ----->                   [Snp2Node] -----> ([Snp2NodeIng]) -----> [WaitSnpResp] -----> [Resp2Slice]
 * Snoop Nest Write 1:    [XXXX]                   ----->                                                                                 [Resp2Slice] * only need to snp one node
 *                                                   ^
 *                                                trans2Snp
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
  val Resp2Slice      = "b0111".U
  val GetDBID         = "b1000".U
  val WaitDBID        = "b1001".U
  val DBIDResp2Node   = "b1010".U
  val WaitData        = "b1011".U
  val Snp2Node        = "b1100".U
  val Snp2NodeIng     = "b1101".U
  val WaitSnpResp     = "b1110".U
}

class PCURSEntry(param: InterfaceParam)(implicit p: Parameters) extends DJBundle  {
  val state         = UInt(PCURS.width.W)
  val nid           = UInt(param.pcuIdBits.W)
  val indexMes      = new DJBundle with HasAddr with HasFromIncoID with HasMSHRWay with HasDBID
  val nestMes       = new Bundle {
    val waitWBDone  = Bool()
    val trans2Snp   = Bool()
  }
  val chiMes        = new RNSLVCHIMesBundle()
  val hasData       = Bool()
  val getDataNum    = UInt(beatNumBits.W)
  val getSnpRespOH  = UInt(nrRnfNode.W)

  def isFree        = state === PCURS.Free
  def isReqBeSend   = state === PCURS.Req2Slice & !nestMes.asUInt.orR & nid === 0.U
  def isRspBeSend   = (state === PCURS.Resp2Node & chiMes.isRsp) | state === PCURS.DBIDResp2Node
  def isDatBeSend   = state === PCURS.Resp2Node & chiMes.isDat
  def isGetDBID     = state === PCURS.GetDBID
  def isSendSnp     = state === PCURS.Snp2Node
  def isSendSnpIng  = state === PCURS.Snp2NodeIng
  def isLastBeat    = getDataNum === (nrBeat - 1).U
}




class RnSlavePCU(djBankId: Int, rnSlvId: Int, param: InterfaceParam)(implicit p: Parameters) extends PCUBaseIO(isSlv = true, hasReq2Slice = true, hasDBRCReq = true) {
  // Del it
  io <> DontCare
  dontTouch(io)
// --------------------- Reg and Wire declaration ------------------------//
  val pcus            = RegInit(VecInit(Seq.fill(param.nrPCUEntry) { 0.U.asTypeOf(new PCURSEntry(param)) }))
  // PCU Receive Task ID
  val pcuFreeID       = Wire(UInt(param.pcuIdBits.W))
  // PCU Send Req To Slice
  val pcuSendReqID    = Wire(UInt(param.pcuIdBits.W))
  // PCU Get DBID ID
  val pcuGetDBID      = Wire(UInt(param.pcuIdBits.W))
  // PCU Receive DBID ID
  val pcuRecDBID      = Wire(UInt(param.pcuIdBits.W))
  // PCU Send Snp ID
  val pcuSendSnpID    = Wire(UInt(param.pcuIdBits.W))
  // PCU Receive TxRsp ID
  val pcuRecChiRspID  = Wire(UInt(param.pcuIdBits.W))
  // PCU Receive TxDat ID
  val pcuRecChiDatID  = Wire(UInt(param.pcuIdBits.W))
  // PCU Send Resp To Slice ID
  val pcuResp2SliceID = Wire(UInt(param.pcuIdBits.W))
  // req from slice or txreq
  val taskNID         = Wire(UInt(param.pcuIdBits.W))
  val taskSaveInPCU   = WireInit(0.U.asTypeOf(pcus(0).chiMes))
  val indexSaveInPCU  = WireInit(0.U.asTypeOf(pcus(0).indexMes))
  // snp to node
  val snpIsLast       = Wire(Bool())
  val snpAlreadySendVecReg = RegInit(0.U(nrRnfNode.W))



// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------- PCU: Update PCU Entry Value ------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  pcus.zipWithIndex.foreach {
    case (pcu, i) =>
      // ------------------------------------------- Update PCU Values -------------------------------------------------- //
      /*
       * Receive New Req
       */
      when((io.chi.txreq.fire | io.req2Node.fire | io.resp2Node.fire) & pcuFreeID === i.U) {
        pcu.nid           := 0.U
        pcu.indexMes      := indexSaveInPCU
        pcu.chiMes        := taskSaveInPCU
        pcu.nestMes       := 0.U.asTypeOf(pcu.nestMes)
        assert(pcu.state === PCURS.Free, "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
      /*
       * Receive DBID From DataBuffer
       */
      }.elsewhen(io.dbSigs.wResp.fire & pcuRecDBID === i.U) {
        pcu.hasData       := true.B
        pcu.indexMes.dbid := io.dbSigs.wResp.bits.dbid
        assert(pcu.state === PCURS.WaitDBID, "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
      /*
       * Receive CHI TX Rsp
       */
      }.elsewhen(io.chi.txrsp.fire & pcuRecChiRspID === i.U){
        pcu.chiMes.resp   := Mux(pcu.chiMes.isSnp & pcu.chiMes.retToSrc, io.chi.txrsp.bits.Resp, pcu.chiMes.resp)
        when(io.chi.txrsp.bits.Opcode === CHIOp.RSP.CompAck) { assert(pcu.state === PCURS.WaitCompAck, "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state) }
        .otherwise                                           { assert(pcu.state === PCURS.Snp2NodeIng | pcu.state === PCURS.WaitSnpResp, "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state) }
      /*
       * Receive CHI TX Dat
       */
      }.elsewhen(io.chi.txdat.fire & pcuRecChiDatID === i.U) {
        pcu.getDataNum    := pcu.getDataNum + 1.U
        pcu.chiMes.resp   := io.chi.txdat.bits.Resp
        assert(Mux(pcu.chiMes.isSnp & pcu.chiMes.retToSrc, pcu.state === PCURS.Snp2NodeIng | pcu.state === PCURS.WaitSnpResp, pcu.state === PCURS.WaitData), "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
      /*
       * Clean PCU Entry When Its Free
       */
      }.elsewhen(pcu.isFree) {
        pcu               := 0.U.asTypeOf(pcu)
      }


      // ---------------------------------------------------- Set Task NID ------------------------------------------------- //
      /*
       * Set New NID
       * TODO: Advance judgment to avoid generating multiple repetitive logic
       */
      when((io.chi.txreq.fire | io.req2Node.fire | io.resp2Node.fire) & pcuFreeID === i.U) {
        val snp2PCUHit    = io.req2Node.fire    & io.req2Node.bits.addrNoOff                         === indexSaveInPCU.addrNoOff
        val resp2SliceHit = io.resp2Slice.fire  & pcus(pcuResp2SliceID).indexMes.addrNoOff           === indexSaveInPCU.addrNoOff
        val reqAckHit     = io.reqAck2Node.fire & pcus(io.reqAck2Node.bits.pcuId).indexMes.addrNoOff === indexSaveInPCU.addrNoOff & !io.reqAck2Node.bits.retry
        // req
        when(io.chi.txreq.fire) {
          pcu.nid         := taskNID - resp2SliceHit.asUInt - reqAckHit.asUInt
          // assert
          assert(taskNID >= (resp2SliceHit.asTypeOf(taskNID) + reqAckHit.asTypeOf(taskNID)), "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
          assert(!(snp2PCUHit & resp2SliceHit), "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
          assert(Mux(taskSaveInPCU.isReq & CHIOp.REQ.isWriteX(taskSaveInPCU.opcode), !snp2PCUHit & !resp2SliceHit, true.B), "TODO")
        // snp or resp
        }.otherwise {
          pcu.nid         := 0.U
          assert(taskNID === 0.U)
        }
      /*
       * Modify NID
       */
      }.elsewhen(!pcu.isFree & pcu.chiMes.isReq) {
        val snp2PCUHit    = io.req2Node.fire    & io.req2Node.bits.addrNoOff                         === pcu.indexMes.addrNoOff
        val resp2SliceHit = io.resp2Slice.fire  & pcus(pcuResp2SliceID).indexMes.addrNoOff           === pcu.indexMes.addrNoOff
        val reqAckHit     = io.reqAck2Node.fire & pcus(io.reqAck2Node.bits.pcuId).indexMes.addrNoOff === pcu.indexMes.addrNoOff & !io.reqAck2Node.bits.retry & io.reqAck2Node.bits.pcuId =/= i.U
        pcu.nid           := pcu.nid + snp2PCUHit.asUInt - resp2SliceHit.asUInt - reqAckHit.asUInt
        // assert
        assert((pcu.nid + snp2PCUHit.asUInt) >= (resp2SliceHit.asTypeOf(taskNID) + reqAckHit.asTypeOf(taskNID)), "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
        assert(!(snp2PCUHit & resp2SliceHit), "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
        assert(Mux(pcu.chiMes.isReq & CHIOp.REQ.isWriteX(pcu.chiMes.opcode), !snp2PCUHit & !resp2SliceHit, true.B), "TODO")
      /*
       * Reset NID
       */
      }.elsewhen(pcu.isFree) {
        pcu.nid           := 0.U
      }


      // ---------------------------------------------- Record Snp Resp --------------------------------------------------- //
      when(pcu.chiMes.isSnp) {
        when(pcu.state === PCURS.Snp2NodeIng | pcu.state === PCURS.WaitSnpResp) {
          val rspHit        = io.chi.txrsp.fire & pcuRecChiRspID === i.U
          val datHit        = io.chi.txdat.fire & pcuRecChiDatID === i.U & pcu.isLastBeat
          val rspId         = getMetaIdByNodeID(io.chi.txrsp.bits.SrcID)
          val datId         = getMetaIdByNodeID(io.chi.txdat.bits.SrcID)
          pcu.getSnpRespOH  := pcu.getSnpRespOH | (UIntToOH(rspId) & rspHit) | (UIntToOH(datId) & datHit)
          // assert
          val getSnpRespVec = Wire(Vec(nrRnfNode, Bool()))
          val tgtSnpVec     = Wire(Vec(nrRnfNode, Bool()))
          getSnpRespVec     := pcu.getSnpRespOH.asBools
          tgtSnpVec         := pcu.chiMes.tgtID(nrRnfNode - 1, 0).asBools
          assert(Mux(rspHit, !getSnpRespVec(rspId), true.B), "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
          assert(Mux(datHit, !getSnpRespVec(datId), true.B), "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
          assert(Mux(rspHit, tgtSnpVec(rspId),      true.B), "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
          assert(Mux(datHit, tgtSnpVec(datId),      true.B), "RNSLV PCU[0x%x] STATE[0x%x]", i.U, pcu.state)
        }.otherwise {
          pcu.getSnpRespOH  := 0.U
        }
      }
  }



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
          val ret2Src   = io.req2Node.bits.retToSrc
          val rDB       = io.resp2Node.bits.needReadDB; assert(Mux(hit, !rDB, true.B), "TODO")
          pcu.state     := Mux(reqHit, PCURS.Req2Slice,
                            Mux(snpHit & ret2Src, PCURS.GetDBID,
                              Mux(snpHit & !ret2Src, PCURS.Snp2Node,
                                Mux(writeHit, PCURS.GetDBID,
                                  Mux(respHit & rDB, PCURS.RCDB,
                                    Mux(respHit & !rDB, PCURS.Resp2Node, pcu.state))))))
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
        // State: Resp2Node
        is(PCURS.Resp2Node) {
          val rxDatHit  = io.chi.rxdat.fire & io.chi.rxdat.bits.DBID === i.U & toBeatNum(io.chi.rxdat.bits.DataID) === (nrBeat - 1).U
          val rxRspHit  = io.chi.rxrsp.fire & io.chi.rxrsp.bits.DBID === i.U
          val expAck    = pcu.chiMes.expCompAck
          pcu.state     := Mux(rxDatHit | rxRspHit, Mux(expAck, PCURS.WaitCompAck, PCURS.Free), pcu.state)
        }
        // State: WaitCompAck
        is(PCURS.WaitCompAck) {
          val hit       = io.chi.txrsp.fire & io.chi.txrsp.bits.TxnID === i.U
          assert(Mux(hit, io.chi.txrsp.bits.Opcode === CompAck, true.B))
          pcu.state     := Mux(hit, PCURS.Free, pcu.state)
        }
        // State: GetDBID
        is(PCURS.GetDBID) {
          val hit       = io.dbSigs.wReq.fire & pcuGetDBID === i.U & pcu.nid === 0.U // TODO: Consider Write can go no sorting required
          pcu.state     := Mux(hit, PCURS.WaitDBID, pcu.state)
        }
        // State: WaitDBID
        is(PCURS.WaitDBID) {
          val hit       = io.dbSigs.wResp.fire & pcuRecDBID === i.U
          pcu.state     := Mux(hit, Mux(pcu.chiMes.isSnp, PCURS.Snp2Node, PCURS.DBIDResp2Node), pcu.state)
        }
        // State: DBIDResp2Node
        is(PCURS.DBIDResp2Node) {
          val hit       = io.chi.rxrsp.fire & io.chi.rxrsp.bits.DBID === i.U
          pcu.state     := Mux(hit, PCURS.WaitData, pcu.state)
        }
        // State: WaitData
        is(PCURS.WaitData) {
          val hit       = io.chi.txdat.fire & io.chi.txdat.bits.TxnID === i.U
          pcu.state     := Mux(hit & pcu.isLastBeat, PCURS.Req2Slice, pcu.state)
        }
        // State: Snp2Node
        is(PCURS.Snp2Node) {
          val hit       = io.chi.rxsnp.fire & pcuSendSnpID === i.U
          pcu.state     := Mux(hit, Mux(snpIsLast, PCURS.WaitSnpResp, PCURS.Snp2NodeIng), pcu.state)
        }
        // State: Snp2NodeIng
        is(PCURS.Snp2NodeIng) {
          val hit       = io.chi.rxsnp.fire & pcuSendSnpID === i.U
          pcu.state     := Mux(hit & snpIsLast, PCURS.WaitSnpResp, pcu.state)
        }
        // State: WaitSnpResp
        is(PCURS.WaitSnpResp) {
          val rspHit    = io.chi.txrsp.fire & pcuRecChiRspID === i.U
          val datHit    = io.chi.txdat.fire & pcuRecChiDatID === i.U
          val isLastRsp = PopCount(pcu.getSnpRespOH ^ pcu.chiMes.tgtID) === 1.U
          pcu.state     := Mux(rspHit & isLastRsp, PCURS.Resp2Slice,
                            Mux(datHit & pcu.isLastBeat & isLastRsp, PCURS.Resp2Slice, pcu.state))
        }
        // State: Resp2Slice
        is(PCURS.Resp2Slice) {
          val hit       = io.resp2Slice.fire & pcuResp2SliceID === i.U
          pcu.state     := Mux(hit, PCURS.Free, pcu.state)
        }
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
  val snpVal        = io.req2Node.valid; assert(Mux(snpVal, io.req2Node.bits.isSnp, true.B))
  val respVal       = io.resp2Node.valid
  val addrMatchVec  = pcus.map(_.indexMes.addrNoOff === indexSaveInPCU.addrNoOff)
  val taskMatchVec  = pcuFreeVec.zip(addrMatchVec).map{ case(a, b) => !a & b }
  //                                  | RESP                                  | SNP                                     | REQ
  taskNID                       := Mux(respVal, 0.U,                          Mux(snpVal, 0.U,                          PopCount(taskMatchVec)))
  indexSaveInPCU.addr           := Mux(respVal, io.resp2Node.bits.addr,       Mux(snpVal, io.req2Node.bits.addr,        io.chi.txreq.bits.Addr))
  indexSaveInPCU.from           := Mux(respVal, io.resp2Node.bits.from,       Mux(snpVal, io.req2Node.bits.from,        DontCare))
  indexSaveInPCU.mshrWay        := Mux(respVal, DontCare,                     Mux(snpVal, io.req2Node.bits.mshrWay,     DontCare))
  indexSaveInPCU.dbid           := Mux(respVal, io.resp2Node.bits.dbid,       Mux(snpVal, 0.U,                          0.U))
  taskSaveInPCU.opcode          := Mux(respVal, io.resp2Node.bits.opcode,     Mux(snpVal, io.req2Node.bits.opcode,      io.chi.txreq.bits.Opcode))
  taskSaveInPCU.tgtID           := Mux(respVal, io.resp2Node.bits.tgtID,      Mux(snpVal, io.req2Node.bits.tgtID,       DontCare))
  taskSaveInPCU.txnID           := Mux(respVal, io.resp2Node.bits.txnID,      Mux(snpVal, io.req2Node.bits.txnID,       io.chi.txreq.bits.TxnID))
  taskSaveInPCU.srcID           := Mux(respVal, io.resp2Node.bits.srcID,      Mux(snpVal, io.req2Node.bits.srcID,       io.chi.txreq.bits.SrcID))
  taskSaveInPCU.retToSrc        := Mux(respVal, DontCare,                     Mux(snpVal, io.req2Node.bits.retToSrc,    DontCare))
  taskSaveInPCU.doNotGoToSD     := Mux(respVal, DontCare,                     Mux(snpVal, io.req2Node.bits.doNotGoToSD, DontCare))
  taskSaveInPCU.channel         := Mux(respVal, io.resp2Node.bits.channel,    Mux(snpVal, CHIChannel.SNP,               CHIChannel.REQ))
  taskSaveInPCU.resp            := Mux(respVal, io.resp2Node.bits.resp,       Mux(snpVal, 0.U,                          0.U))
  taskSaveInPCU.expCompAck      := Mux(respVal, io.resp2Node.bits.expCompAck, Mux(snpVal, false.B,                      io.chi.txreq.bits.ExpCompAck))

  /*
   * Set Ready Value
   */
  io.chi.txreq.ready    := pcuFreeNum >= param.nrPCUEvictEntry.U & !io.req2Node.valid & !io.resp2Node.valid
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
  pcuSendReqID      := RREncoder(reqBeSendVec)

  /*
   * Send Req To Node
   */
  io.req2Slice.valid            := reqBeSendVec.reduce(_ | _)
  io.req2Slice.bits.channel     := pcus(pcuSendReqID).chiMes.channel
  io.req2Slice.bits.opcode      := pcus(pcuSendReqID).chiMes.opcode
  io.req2Slice.bits.addr        := pcus(pcuSendReqID).indexMes.addr
  io.req2Slice.bits.srcID       := pcus(pcuSendReqID).chiMes.srcID
  io.req2Slice.bits.txnID       := pcus(pcuSendReqID).chiMes.txnID
  io.req2Slice.bits.expCompAck  := pcus(pcuSendReqID).chiMes.expCompAck
  // IdMap
  io.req2Slice.bits.to.IncoId   := pcus(pcuSendReqID).indexMes.mBank
  io.req2Slice.bits.from.IncoId := rnSlvId.U
  io.req2Slice.bits.pcuId       := pcuSendReqID
  io.req2Slice.bits.dbid        := pcus(pcuSendReqID).indexMes.dbid
  // Use in RnMaster
  io.req2Slice.bits.retToSrc    := false.B
  io.req2Slice.bits.doNotGoToSD := false.B



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------- Select PCU send Resp to Node --------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Select one PCU to Send RxDat
   */
  val datBeSendVec  = pcus.map { case p => p.isDatBeSend & p.indexMes.dbid === io.dbSigs.dataFDB.bits.dbid }
  val datSelId      = PriorityEncoder(datBeSendVec)

  io.chi.rxdat.valid        := datBeSendVec.reduce(_ | _)
  io.chi.rxdat.bits         := DontCare
  io.chi.rxdat.bits.Opcode  := pcus(datSelId).chiMes.opcode
  io.chi.rxdat.bits.TgtID   := pcus(datSelId).chiMes.srcID
  io.chi.rxdat.bits.SrcID   := hnfNodeIdSeq(djBankId).U
  io.chi.rxdat.bits.TxnID   := pcus(datSelId).chiMes.txnID
  io.chi.rxdat.bits.HomeNID := hnfNodeIdSeq(djBankId).U
  io.chi.rxdat.bits.DBID    := datSelId
  io.chi.rxdat.bits.Resp    := pcus(datSelId).chiMes.resp
  io.chi.rxdat.bits.DataID  := io.dbSigs.dataFDB.bits.dataID
  io.chi.rxdat.bits.Data    := io.dbSigs.dataFDB.bits.data
  io.chi.rxdat.bits.BE      := Fill(io.chi.rxdat.bits.BE.getWidth, 1.U(1.W))

  io.dbSigs.dataFDB.ready   := io.chi.rxdat.ready


  /*
   * Select one pcu to Send RxRsp
   */
  val rspBeSendVec          = pcus.map { case p => p.isRspBeSend}
  val rspSelId              = PriorityEncoder(rspBeSendVec)

  io.chi.rxrsp.valid        := rspBeSendVec.reduce(_ | _)
  io.chi.rxrsp.bits         := DontCare
  io.chi.rxrsp.bits.Opcode  := Mux(pcus(rspSelId).chiMes.isRsp, pcus(rspSelId).chiMes.opcode, CompDBIDResp)
  io.chi.rxrsp.bits.TgtID   := Mux(pcus(rspSelId).chiMes.isRsp, pcus(rspSelId).chiMes.srcID,  pcus(rspSelId).chiMes.srcID)
  io.chi.rxrsp.bits.SrcID   := Mux(pcus(rspSelId).chiMes.isRsp, hnfNodeIdSeq(djBankId).U,     hnfNodeIdSeq(djBankId).U)
  io.chi.rxrsp.bits.TxnID   := Mux(pcus(rspSelId).chiMes.isRsp, pcus(rspSelId).chiMes.txnID,  pcus(rspSelId).chiMes.txnID)
  io.chi.rxrsp.bits.DBID    := Mux(pcus(rspSelId).chiMes.isRsp, rspSelId,                     rspSelId)
  io.chi.rxrsp.bits.Resp    := Mux(pcus(rspSelId).chiMes.isRsp, pcus(rspSelId).chiMes.resp,   DontCare)



// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------- Receive Rsp Or Dat From Node ------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Get PCU ID
   */
  pcuRecChiRspID                  := io.chi.txrsp.bits.TxnID(param.pcuIdBits - 1, 0)
  pcuRecChiDatID                  := io.chi.txdat.bits.TxnID(param.pcuIdBits - 1, 0)

  /*
   * Send Data To DataBuffer
   */
  io.dbSigs.dataTDB.valid         := io.chi.txdat.valid
  io.dbSigs.dataTDB.bits.dbid     := pcus(pcuRecChiDatID).indexMes.dbid
  io.dbSigs.dataTDB.bits.data     := io.chi.txdat.bits.Data
  io.dbSigs.dataTDB.bits.dataID   := io.chi.txdat.bits.DataID


  /*
   * Set ready value
   */
  io.chi.txrsp.ready              := true.B
  io.chi.txdat.ready              := io.dbSigs.dataTDB.ready


// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------- Get DBID From DataBuffer and Wait DataBuffer Resp ---------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Send Get DBID Req To From DataBuffer
   */
  val pcuGetDBIDVec         = pcus.map(_.isGetDBID)
  pcuGetDBID                := PriorityEncoder(pcuGetDBIDVec)

  /*
   * Set DataBuffer Req Value
   */
  io.dbSigs.wReq.valid            := pcuGetDBIDVec.reduce(_ | _)
  io.dbSigs.wReq.bits.from.IncoId := rnSlvId.U
  io.dbSigs.wReq.bits.pcuId       := pcuGetDBID

  /*
   * Receive DBID From DataBuffer
   */
  pcuRecDBID                      := io.dbSigs.wResp.bits.pcuId
  io.dbSigs.wResp.ready           := true.B



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------- Select PCU send Snp to Node ---------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Get PCU ID
   */
  val pcuSendSnpVec     = pcus.map(_.isSendSnp)
  val pcuSendSnpIngVec  = pcus.map(_.isSendSnpIng)
  pcuSendSnpID          := Mux(pcuSendSnpIngVec.reduce(_ | _), PriorityEncoder(pcuSendSnpIngVec), PriorityEncoder(pcuSendSnpVec))

  /*
   * Get Tgt ID
   */
  val snpShouldSendVec  = pcus(pcuSendSnpID).chiMes.snpMetaVec
  val snpBeSendVec      = snpShouldSendVec ^ snpAlreadySendVecReg
  val snpTgtID          = getNodeIDByMetaId(PriorityEncoder(snpBeSendVec), 0)
  snpIsLast             := PopCount(snpBeSendVec.asBools) === 1.U; dontTouch(snpIsLast)
  snpAlreadySendVecReg  := Mux(io.chi.rxsnp.fire, Mux(snpIsLast, 0.U, snpAlreadySendVecReg | UIntToOH(getMetaIdByNodeID(snpTgtID))), snpAlreadySendVecReg)

  /*
   * Send Snp to Node
   */
  io.chi.rxsnp.valid          := pcuSendSnpVec.reduce(_ | _) | pcuSendSnpIngVec.reduce(_ | _)
  io.chi.rxsnp.bits           := DontCare
  io.chi.rxsnp.bits.Addr      := pcus(pcuSendSnpID).indexMes.addr(addressBits - 1, 3)
  io.chi.rxsnp.bits.Opcode    := pcus(pcuSendSnpID).chiMes.opcode
  io.chi.rxsnp.bits.TgtID     := snpTgtID
  io.chi.rxsnp.bits.SrcID     := hnfNodeIdSeq(djBankId).U
  io.chi.rxsnp.bits.TxnID     := pcuSendSnpID
  io.chi.rxsnp.bits.FwdNID    := pcus(pcuSendSnpID).chiMes.srcID
  io.chi.rxsnp.bits.FwdTxnID  := pcus(pcuSendSnpID).chiMes.txnID
  io.chi.rxsnp.bits.RetToSrc  := pcus(pcuSendSnpID).chiMes.retToSrc & snpAlreadySendVecReg === 0.U
  io.chi.rxsnp.bits.DoNotGoToSD := pcus(pcuSendSnpID).chiMes.doNotGoToSD


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------ Select PCU send Resp to Slice --------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Select one PCU to Send RxDat
   */
  val respBeSendVec                 = pcus.map { case p => p.state === PCURS.Resp2Slice }
  pcuResp2SliceID                   := PriorityEncoder(respBeSendVec)

  io.resp2Slice.valid               := respBeSendVec.reduce(_ | _)
  io.resp2Slice.bits.from.IncoId    := rnSlvId.U
  io.resp2Slice.bits.to             := pcus(pcuResp2SliceID).indexMes.from
  io.resp2Slice.bits.mshrSet        := pcus(pcuResp2SliceID).indexMes.mSet
  io.resp2Slice.bits.mshrWay        := pcus(pcuResp2SliceID).indexMes.mshrWay
  io.resp2Slice.bits.dbid           := pcus(pcuResp2SliceID).indexMes.dbid
  io.resp2Slice.bits.isSnpResp      := true.B
  io.resp2Slice.bits.isReqResp      := false.B
  io.resp2Slice.bits.hasData        := pcus(pcuResp2SliceID).hasData
  io.resp2Slice.bits.resp           := pcus(pcuResp2SliceID).chiMes.resp
  io.resp2Slice.bits.fwdState.valid := CHIOp.SNP.isSnpXFwd(pcus(pcuResp2SliceID).chiMes.opcode)
  io.resp2Slice.bits.fwdState.bits  := pcus(pcuResp2SliceID).chiMes.fwdState






// ---------------------------  Assertion  -------------------------------- //
  val cntReg = RegInit(VecInit(Seq.fill(param.nrPCUEntry) { 0.U(64.W) }))
  cntReg.zip(pcus).foreach { case(c, p) => c := Mux(p.isFree, 0.U, c + 1.U) }
  cntReg.zipWithIndex.foreach { case(c, i) => assert(c < TIMEOUT_RSPCU.U, "RNSLV PCU[0x%x] ADDR[0x%x] CHANNEL[%x] OP[0x%x] TIMEOUT", i.U, pcus(i).indexMes.addr, pcus(i).chiMes.channel, pcus(i).chiMes.opcode) }
}