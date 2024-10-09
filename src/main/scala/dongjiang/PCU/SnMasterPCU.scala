package DONGJIANG.PCU

import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIOp.REQ._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.FastArb._

/*
 * Read Req:  [Free] -----> [GetDBID] -----> [WaitDBID] -----> [Req2Node] -----> [WaitNodeData] -----> [Resp2Slice]
 *
 * Write Req: [Free] -----> [Req2Node] -----> [WaitNodeDBID] -----> [RCDB] -----> [WriteData2Node] -----> [WaitNodeComp] -----> [Resp2Slice]
 *
 * Replace:   [Free] -----> [Req2Node] -----> [WaitNodeDBID] -----> [Replace2Node] -----> [WaitReplDBID] -----> [RCDB] -----> [WriteData2Node] -----> [WaitNodeComp] -----> [Resp2Slice]
 */

object PCUSM {
  val width = 4
  // commom
  val Free            = "b0000".U
  val GetDBID         = "b0001".U
  val WaitDBID        = "b0010".U
  val Req2Node        = "b0011".U
  val WaitNodeData    = "b0100".U
  val Resp2Slice      = "b0101".U
  val WaitNodeDBID    = "b0110".U
  val RCDB            = "b0111".U
  val WriteData2Node  = "b1000".U
  val WaitNodeComp    = "b1001".U
  val Replace2Node    = "b1010".U
  val WaitReplDBID    = "b1011".U
}

object SMType {
  val width = 2
  val RDCU  = "b00".U
  val RDDR  = "b01".U
  val WRITE = "b10".U // Write DCU
  val REPL  = "b11".U // Read DCU And Write DDRC
}

class PCUSMEntry(param: InterfaceParam)(implicit p: Parameters) extends DJBundle {
  val state         = UInt(PCUSM.width.W)
  val indexMes      = new DJBundle with HasAddr with HasFromIncoID with HasMSHRWay with HasDBID {
    val selfWay     = UInt(sWayBits.W)
  }
  val chiMes        = new SNMASCHIMesBundle()
  val hasData       = Bool()
  val getDataNum    = UInt(beatNumBits.W)
  val alrGetComp    = Bool() // Already Get Comp

  def reqAddr(tgtId: UInt): UInt = {
    val addr        = Wire(UInt(addressBits.W))
    when(tgtId === ddrcNodeId.U) { addr := indexMes.addr
    }.otherwise { addr := getDCUAddress(indexMes.addr, indexMes.selfWay) }
    addr
  }

  def isFree        = state === PCUSM.Free
  def isGetDBID     = state === PCUSM.GetDBID
  def isReq2Node    = state === PCUSM.Req2Node
  def isRepl2Node   = state === PCUSM.Replace2Node
  def isResp2Slice  = state === PCUSM.Resp2Slice
  def isWaitDBData  = state === PCUSM.WriteData2Node
  def isRCDB        = state === PCUSM.RCDB
  def isLastBeat    = getDataNum === (nrBeat - 1).U
  def isRead        = isReadX(chiMes.opcode)
  def isWrite       = isWriteX(chiMes.opcode)
  def isRepl        = isReplace(chiMes.opcode)
}

class SnMasterPCU(snMasId: Int, param: InterfaceParam)(implicit p: Parameters) extends PCUBaseIO(isSlv = false, hasReq2Slice = false, hasDBRCReq = true) {
  // Del it
  io <> DontCare
  dontTouch(io)
// --------------------- Reg and Wire declaration ------------------------//
  val pcus            = RegInit(VecInit(Seq.fill(param.nrPCUEntry) { 0.U.asTypeOf(new PCUSMEntry(param)) })); dontTouch(pcus)
  // PCU Receive Req ID
  val pcuGetReqID     = Wire(UInt(param.pcuIdBits.W))
  // PCU Get DBID ID
  val pcuGetDBID      = Wire(UInt(param.pcuIdBits.W))
  // PCU Receive DBID ID
  val pcuRecDBID      = Wire(UInt(param.pcuIdBits.W))
  // PCU Req To Node ID
  val pcuReq2NodeID   = Wire(UInt(param.pcuIdBits.W))
  // PCU Send Resp To Slice ID
  val pcuResp2SliceID = Wire(UInt(param.pcuIdBits.W))
  // PCU Send RC Req To DataBuufer ID
  val pcuRCDBID       = Wire(UInt(param.pcuIdBits.W))
  // req from slice
  val indexSaveInPCU  = WireInit(0.U.asTypeOf(pcus(0).indexMes))
  val reqSaveInPCU    = WireInit(0.U.asTypeOf(pcus(0).chiMes))


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------- PCU: Update MHSR Table Value  ----------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  pcus.zipWithIndex.foreach {
    case (pcu, i) =>
      /*
       * Receive New Req
       */
      when(io.req2Node.fire & pcuGetReqID === i.U) {
        pcu               := 0.U.asTypeOf(pcu) // Clean PCU Entry When ReAlloc
        pcu.indexMes      := indexSaveInPCU
        pcu.chiMes        := reqSaveInPCU
        assert(pcu.state === PCUSM.Free, "SNMAS PCU[0x%x] STATE[0x%x] OP[0x%x] ADDR[0x%x] TGTID[0x%x]", i.U, pcu.state, pcu.chiMes.opcode, pcu.indexMes.addr, pcu.chiMes.tgtID)
      /*
       * Receive DBID From DataBuffer
       */
      }.elsewhen(io.dbSigs.wResp.fire & pcuRecDBID === i.U) {
        pcu.hasData       := true.B
        pcu.indexMes.dbid := io.dbSigs.wResp.bits.dbid
        assert(pcu.state === PCUSM.WaitDBID, "SNMAS PCU[0x%x] STATE[0x%x] OP[0x%x] ADDR[0x%x] TGTID[0x%x]", i.U, pcu.state, pcu.chiMes.opcode, pcu.indexMes.addr, pcu.chiMes.tgtID)
      /*
       * Receive Data And Resp From CHI RxDat
       */
      }.elsewhen(io.chi.rxdat.fire & io.chi.rxdat.bits.TxnID === i.U) {
        pcu.getDataNum    := pcu.getDataNum + 1.U
        pcu.chiMes.resp   := io.chi.rxdat.bits.Resp
        assert(io.chi.rxdat.bits.Opcode === CHIOp.DAT.CompData, "SNMAS PCU[0x%x] STATE[0x%x] OP[0x%x] ADDR[0x%x] TGTID[0x%x]", i.U, pcu.state, pcu.chiMes.opcode, pcu.indexMes.addr, pcu.chiMes.tgtID)
        assert(pcu.isRead, "SNMAS PCU[0x%x] STATE[0x%x] OP[0x%x] ADDR[0x%x] TGTID[0x%x]", i.U, pcu.state, pcu.chiMes.opcode, pcu.indexMes.addr, pcu.chiMes.tgtID)
        assert(pcu.state === PCUSM.WaitNodeData, "SNMAS PCU[0x%x] STATE[0x%x] OP[0x%x] ADDR[0x%x] TGTID[0x%x]", i.U, pcu.state, pcu.chiMes.opcode, pcu.indexMes.addr, pcu.chiMes.tgtID)
      /*
       * Receive DBID or Comp From CHI RxRsp
       */
      }.elsewhen(io.chi.rxrsp.fire & io.chi.rxrsp.bits.TxnID === i.U) {
        when(io.chi.rxrsp.bits.Opcode === CHIOp.RSP.DBIDResp) {
          pcu.chiMes.chiDBID := io.chi.rxrsp.bits.DBID
          assert(pcu.state === PCUSM.WaitReplDBID | pcu.state === PCUSM.WaitNodeDBID, "SNMAS PCU[0x%x] STATE[0x%x] OP[0x%x] ADDR[0x%x] TGTID[0x%x]", i.U, pcu.state, pcu.chiMes.opcode, pcu.indexMes.addr, pcu.chiMes.tgtID)
        }
        when(io.chi.rxrsp.bits.Opcode === CHIOp.RSP.Comp) {
          pcu.alrGetComp  := true.B
          assert(Mux(!pcu.isRepl | pcu.alrGetComp, pcu.state === PCUSM.WaitNodeComp,
            pcu.state === PCUSM.WaitReplDBID | pcu.state === PCUSM.RCDB | pcu.state === PCUSM.WriteData2Node |  pcu.state === PCUSM.WaitNodeComp),
            "SNMAS PCU[0x%x] STATE[0x%x] OP[0x%x] ADDR[0x%x] TGTID[0x%x]", i.U, pcu.state, pcu.chiMes.opcode, pcu.indexMes.addr, pcu.chiMes.tgtID)
        }

      /*
       * Receive Data From DataBuffer
       */
      }.elsewhen(io.dbSigs.dataFDB.fire & pcu.isWaitDBData & io.dbSigs.dataFDB.bits.dbid === pcu.indexMes.dbid) {
        pcu.getDataNum    := pcu.getDataNum + 1.U
        assert(pcu.state === PCUSM.WriteData2Node, "SNMAS PCU[0x%x] STATE[0x%x] OP[0x%x] ADDR[0x%x] TGTID[0x%x]", i.U, pcu.state, pcu.chiMes.opcode, pcu.indexMes.addr, pcu.chiMes.tgtID)
      /*
       * Clean PCU Entry When Its Free
       */
      }.elsewhen(pcu.isFree) {
        pcu               := 0.U.asTypeOf(pcu)
      }
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- PCU: State Transfer ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  pcus.zipWithIndex.foreach {
    case (pcu, i) =>
      switch(pcu.state) {
        // State: Free
        is(PCUSM.Free) {
          val reqHit    = io.req2Node.fire & isReadX(io.req2Node.bits.opcode) & pcuGetReqID === i.U; assert(!reqHit | io.req2Node.bits.opcode === ReadNoSnp)
          val writeHit  = io.req2Node.fire & isWriteX(io.req2Node.bits.opcode) & pcuGetReqID === i.U; assert(!writeHit | io.req2Node.bits.opcode === WriteNoSnpFull)
          val replHit   = io.req2Node.fire & isReplace(io.req2Node.bits.opcode) & pcuGetReqID === i.U; assert(!replHit | io.req2Node.bits.opcode === Replace)
          pcu.state     := Mux(reqHit, PCUSM.GetDBID,
                            Mux(writeHit, PCUSM.Req2Node,
                              Mux(replHit, PCUSM.Req2Node, pcu.state)))
        }
        // State: GetDBID
        is(PCUSM.GetDBID) {
          val hit       = io.dbSigs.wReq.fire & pcuGetDBID === i.U
          pcu.state     := Mux(hit, PCUSM.WaitDBID, pcu.state)
          assert(pcu.isRead | !hit)
        }
        // State: WaitDBID
        is(PCUSM.WaitDBID) {
          val hit       = io.dbSigs.wResp.fire & pcuRecDBID === i.U
          pcu.state     := Mux(hit, PCUSM.Req2Node, pcu.state)
        }
        // State: Req2Node
        is(PCUSM.Req2Node) {
          val hit       = io.chi.txreq.fire & pcuReq2NodeID === i.U
          pcu.state     := Mux(hit, Mux(pcu.isRead, PCUSM.WaitNodeData, PCUSM.WaitNodeDBID), pcu.state)
        }
        // State: WaitNodeResp
        is(PCUSM.WaitNodeData) {
          val rxDatHit  = io.chi.rxdat.fire & io.chi.rxdat.bits.TxnID === i.U
          pcu.state     := Mux(rxDatHit & pcu.isLastBeat, PCUSM.Resp2Slice, pcu.state)
        }
        // State: Resp2Slice
        is(PCUSM.Resp2Slice) {
          val hit       = io.resp2Slice.fire & pcuResp2SliceID === i.U
          pcu.state     := Mux(hit, PCUSM.Free, pcu.state)
        }
        // State: WaitNodeDBID
        is(PCUSM.WaitNodeDBID) {
          val hit       = io.chi.rxrsp.fire & io.chi.rxrsp.bits.TxnID === i.U
          pcu.state     := Mux(hit, Mux(pcu.isRepl, PCUSM.Replace2Node, PCUSM.RCDB), pcu.state)
        }
        // State: RCDB
        is(PCUSM.RCDB) {
          val hit       = io.dbSigs.dbRCReq.fire & pcuRCDBID === i.U
          pcu.state     := Mux(hit, PCUSM.WriteData2Node, pcu.state)
        }
        // State: WriteData2Node
        is(PCUSM.WriteData2Node) {
          val hit       = io.chi.txdat.fire & pcu.isLastBeat & io.dbSigs.dataFDB.bits.dbid === pcu.indexMes.dbid
          pcu.state     := Mux(hit, PCUSM.WaitNodeComp, pcu.state)
        }
        // State: WaitNodeComp
        is(PCUSM.WaitNodeComp) {
          val hit       = io.chi.rxrsp.fire & io.chi.rxrsp.bits.TxnID === i.U
          val canGo     = !pcu.isRepl | pcu.alrGetComp
          pcu.state     := Mux(hit & canGo,  PCUSM.Resp2Slice, pcu.state)
        }
        // State: Replace2Node
        is(PCUSM.Replace2Node) {
          val hit       = io.chi.txreq.fire & pcuReq2NodeID === i.U; assert(!hit | pcu.isRepl, "SNMAS PCU[0x%x] STATE[0x%x] OP[0x%x] ADDR[0x%x] TGTID[0x%x]", i.U, pcu.state, pcu.chiMes.opcode, pcu.indexMes.addr, pcu.chiMes.tgtID)
          pcu.state     := Mux(hit, PCUSM.WaitReplDBID, pcu.state)
        }
        // State: WaitReplDBID
        is(PCUSM.WaitReplDBID) {
          val hit       = io.chi.rxrsp.fire & io.chi.rxrsp.bits.TxnID === i.U
          pcu.state     := Mux(hit, PCUSM.RCDB, pcu.state)
        }
      }
  }


  // ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------ Receive Req From CHITXREQ or Req2Node and Save In PCU --------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Receive req2Node(Snoop)
   */
  indexSaveInPCU.addr     := io.req2Node.bits.addr
  indexSaveInPCU.selfWay  := io.req2Node.bits.selfWay
  indexSaveInPCU.mshrWay  := io.req2Node.bits.mshrWay
  indexSaveInPCU.from     := io.req2Node.bits.from
  indexSaveInPCU.dbid     := io.req2Node.bits.dbid
  reqSaveInPCU.resp       := io.req2Node.bits.resp
  reqSaveInPCU.opcode     := io.req2Node.bits.opcode
  reqSaveInPCU.tgtID      := io.req2Node.bits.tgtID
  reqSaveInPCU.expCompAck := io.req2Node.bits.expCompAck
  assert(Mux(io.req2Node.valid, !io.req2Node.bits.expCompAck, true.B))

  /*
   * Set PCU Value
   */
  val pcuFreeVec        = pcus.map(_.isFree)
  val pcuFreeNum        = PopCount(pcuFreeVec)
  pcuGetReqID           := PriorityEncoder(pcuFreeVec)

  /*
   * Set Ready Value
   */
  io.req2Node.ready     := pcuFreeNum > 0.U



// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------- Get DBID From DataBuffer and Wait DataBuffer Resp ---------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Send Get DBID Req To From DataBuffer
   */
  val pcuGetDBIDVec     = pcus.map(_.isGetDBID)
  pcuGetDBID            := PriorityEncoder(pcuGetDBIDVec)

  /*
   * Set DataBuffer Req Value
   */
  io.dbSigs.wReq.valid            := pcuGetDBIDVec.reduce(_ | _)
  io.dbSigs.wReq.bits.from.IncoId := snMasId.U
  io.dbSigs.wReq.bits.pcuId       := pcuGetDBID

  /*
   * Receive DBID From DataBuffer
   */
  pcuRecDBID := io.dbSigs.wResp.bits.pcuId
  io.dbSigs.wResp.ready := true.B



// ---------------------------------------------------------------------------------------------------------------------- //
// -------------------------------------------------- Send Req To Node -------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  val pcuReq2NodeVec            = pcus.map { case p => p.isReq2Node | p.isRepl2Node }
  pcuReq2NodeID                 := PriorityEncoder(pcuReq2NodeVec)

  val writeDDR                  = pcus(pcuReq2NodeID).isReq2Node & pcus(pcuReq2NodeID).isRepl

  io.chi.txreq.valid            := pcuReq2NodeVec.reduce(_ | _)
  io.chi.txreq.bits.Addr        := pcus(pcuReq2NodeID).reqAddr(io.chi.txreq.bits.TgtID)
  //                                             Send Write To DDR                    Send Req To DDR / DCU
  io.chi.txreq.bits.Opcode      := Mux(writeDDR, WriteNoSnpFull,                      pcus(pcuReq2NodeID).chiMes.opcode)
  io.chi.txreq.bits.TgtID       := Mux(writeDDR, ddrcNodeId.U,                        pcus(pcuReq2NodeID).chiMes.tgtID)
  io.chi.txreq.bits.TxnID       := pcuReq2NodeID
  io.chi.txreq.bits.SrcID       := hnfNodeId.U
  io.chi.txreq.bits.Size        := log2Ceil(djparam.blockBytes).U
  io.chi.txreq.bits.MemAttr     := pcus(pcuReq2NodeID).chiMes.resp // Multiplex MemAttr to transfer CHI State // Use in Read Req
  // only use in replace
  io.chi.txreq.bits.ReturnNID   := ddrcNodeId.U
  io.chi.txreq.bits.ReturnTxnID := pcus(pcuReq2NodeID).chiMes.chiDBID


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------- Receive CHI DBID From From Node ------------------------------------------ //
// ---------------------------------------------------------------------------------------------------------------------- //
  io.chi.rxrsp.ready            := true.B
  assert(Mux(io.chi.rxrsp.fire, io.chi.rxrsp.bits.Opcode === CHIOp.RSP.DBIDResp | io.chi.rxrsp.bits.Opcode === CHIOp.RSP.Comp, true.B))


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------- Read And Clean DataBuffer --------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  val pcuRCDBIDVec                  = pcus.map(_.isRCDB)
  pcuRCDBID                         := PriorityEncoder(pcuRCDBIDVec)

  io.dbSigs.dbRCReq.valid           := pcuRCDBIDVec.reduce(_ | _)
  io.dbSigs.dbRCReq.bits.isRead     := true.B
  io.dbSigs.dbRCReq.bits.isClean    := true.B
  io.dbSigs.dbRCReq.bits.dbid       := pcus(pcuRCDBID).indexMes.dbid
  io.dbSigs.dbRCReq.bits.to.IncoId  := snMasId.U


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------- Receive Data From DataBuffer And Send Data To Node ----------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  val pcuSendDatVec             = pcus.map { case p => p.state === PCUSM.WriteData2Node & p.indexMes.dbid === io.dbSigs.dataFDB.bits.dbid }
  val pcuSendDatID              = PriorityEncoder(pcuSendDatVec)
  assert(Mux(io.chi.txdat.valid, PopCount(pcuSendDatVec) === 1.U, true.B))

  io.chi.txdat.valid            := io.dbSigs.dataFDB.valid
  io.chi.txdat.bits             := DontCare
  io.chi.txdat.bits.Opcode      := CHIOp.DAT.NonCopyBackWrData
  io.chi.txdat.bits.TgtID       := pcus(pcuSendDatID).chiMes.tgtID
  io.chi.txdat.bits.SrcID       := hnfNodeId.U
  io.chi.txdat.bits.TxnID       := pcus(pcuSendDatID).chiMes.chiDBID
  io.chi.txdat.bits.DataID      := io.dbSigs.dataFDB.bits.dataID
  io.chi.txdat.bits.Data        := io.dbSigs.dataFDB.bits.data
  io.chi.txdat.bits.BE          := Fill(io.chi.rxdat.bits.BE.getWidth, 1.U(1.W))

  io.dbSigs.dataFDB.ready       := io.chi.txdat.ready

// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------- Receive Data From CHI DAT And Send It To DataBuffer -------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  io.dbSigs.dataTDB.valid       := io.chi.rxdat.valid
  io.dbSigs.dataTDB.bits.dbid   := pcus(io.chi.rxdat.bits.TxnID(param.pcuIdBits-1, 0)).indexMes.dbid
  io.dbSigs.dataTDB.bits.data   := io.chi.rxdat.bits.Data
  io.dbSigs.dataTDB.bits.dataID := io.chi.rxdat.bits.DataID
  io.chi.rxdat.ready            := io.dbSigs.dataTDB.ready
  assert(Mux(io.chi.rxdat.valid, io.chi.rxdat.bits.TxnID <= param.nrPCUEntry.U, true.B))



// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------------------------ Send Resp To Slice -------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  val pcuResp2SliceVec            = pcus.map(_.isResp2Slice)
  pcuResp2SliceID                 := PriorityEncoder(pcuResp2SliceVec)

  io.resp2Slice.valid             := pcuResp2SliceVec.reduce(_ | _)
  io.resp2Slice.bits              := DontCare
  io.resp2Slice.bits.isReqResp    := pcus(pcuResp2SliceID).isRead
  io.resp2Slice.bits.isWriResp    := pcus(pcuResp2SliceID).isWrite | pcus(pcuResp2SliceID).isRepl
  io.resp2Slice.bits.mshrSet      := pcus(pcuResp2SliceID).indexMes.mSet
  io.resp2Slice.bits.mshrWay      := pcus(pcuResp2SliceID).indexMes.mshrWay
  io.resp2Slice.bits.from.IncoId  := snMasId.U
  io.resp2Slice.bits.to           := pcus(pcuResp2SliceID).indexMes.from
  io.resp2Slice.bits.hasData      := pcus(pcuResp2SliceID).hasData
  io.resp2Slice.bits.dbid         := pcus(pcuResp2SliceID).indexMes.dbid
  io.resp2Slice.bits.resp         := pcus(pcuResp2SliceID).chiMes.resp



// ---------------------------  Assertion  --------------------------------//
  val cntReg = RegInit(VecInit(Seq.fill(param.nrPCUEntry) {
    0.U(64.W)
  }))
  cntReg.zip(pcus).foreach { case (c, p) => c := Mux(p.isFree, 0.U, c + 1.U) }
  cntReg.zipWithIndex.foreach { case (c, i) => assert(c < TIMEOUT_SMPCU.U, "SNMAS PCU[0x%x] STATE[0x%x] ADDR[0x%x] OP[0x%x] TIMEOUT", i.U, pcus(i).state, pcus(i).indexMes.addr, pcus(i).chiMes.opcode) }
}