package DONGJIANG.PCU

import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIOp.REQ._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.FastArb._

/*
 * Read Req:  [Free] -----> [GetDBID] -----> [WaitDBID] -----> [Req2Node] -----> [WaitNodeResp] -----> [Resp2Slice]
 *
 * Write Req: [Free] -----> [Req2Node] -----> [WaitNodeDBID] -----> [RCDB] -----> [WriteData2Node] -----> [Resp2Slice]
 *
 * Replace:   [Free] -----> [Req2Node] -----> [WaitNodeDBID] -----> [Req2Node] -----> [WaitNodeResp_WriteData2Node] -----> [WriteData2Node] -----> [Resp2Slice]
 */

object PCUSM {
  val width = 4
  // commom
  val Free            = "b0000".U
  val GetDBID         = "b0001".U
  val WaitDBID        = "b0010".U
  val Req2Node        = "b0011".U
  val WaitNodeResp    = "b0100".U
  val Resp2Slice      = "b0101".U
  val WaitNodeDBID    = "b0110".U
  val RCDB            = "b0111".U
  val WriteData2Node  = "b1000".U
  val WaitNodeResp_WriteData2Node = "b1001".U
}

object SMType {
  val width = 2
  val RDCU  = "b00".U
  val RDDR  = "b01".U
  val WRITE = "b10".U
  val REPL  = "b11".U
}

trait HasSMType extends Bundle { this: Bundle =>
  val smType  = UInt(PipeID.width.W)

  def isReadDCU   = smType === SMType.RDCU
  def isReadDDR   = smType === SMType.RDDR
  def isWrite     = smType === SMType.WRITE
  def isRepl      = smType === SMType.REPL
}

class PCUSMEntry(param: InterfaceParam)(implicit p: Parameters) extends DJBundle with HasAddr with HasMSHRWay with HasSMType {
  val state         = UInt(PCUSM.width.W)
  val reqMes        = new DJBundle with HasFromIDBits with HasMSHRWay {
    val opcode      = UInt(6.W)
  }
  val respMes       = new DJBundle {
    val resp        = UInt(ChiResp.width.W)
    val dbid        = Valid(UInt(djparam.chiDBIDBits.W))
  }
  val dbid          = Valid(UInt(dbIdBits.W))
  val getDataNum    = UInt(beatNumBits.W)

  def isFree        = state === PCUSM.Free
  def isGetDBID     = state === PCUSM.GetDBID
  def isReq2Node    = state === PCUSM.Req2Node
  def isResp2Slice  = state === PCUSM.Resp2Slice
  def isLastBeat    = getDataNum === (nrBeat - 1).U
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
  // PCU Req To Node ID
  val pcuResp2SliceID = Wire(UInt(param.pcuIdBits.W))
  // req from slice
  val addrSaveInPCU   = Wire(UInt(addressBits.W))
  val reqSaveInPCU    = WireInit(0.U.asTypeOf(pcus(0).reqMes))


// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- PCU: State Transfer ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  pcus.zipWithIndex.foreach {
    case (pcu, i) =>
      switch(pcu.state) {
        // State: Free
        is(PCUSM.Free) {
          val reqHit    = io.req2Node.fire & isReadX(io.req2Node.bits.opcode) & pcuGetReqID === i.U
          val writeHit  = io.req2Node.fire & isWriteX(io.req2Node.bits.opcode) & pcuGetReqID === i.U
          val replHit   = io.req2Node.fire & io.req2Node.bits.replace & pcuGetReqID === i.U
          pcu.state     := Mux(reqHit, PCUSM.GetDBID,
                            Mux(writeHit, PCUSM.Req2Node,
                              Mux(replHit, PCUSM.Req2Node, pcu.state)))
          pcu.smType    := Mux(reqHit, Mux(io.req2Node.bits.ReadDCU, SMType.RDCU, SMType.RDDR),
                            Mux(writeHit, SMType.WRITE,
                              Mux(replHit, SMType.REPL, pcu.smType)))
        }
        // State: GetDBID
        is(PCUSM.GetDBID) {
          val hit       = io.dbSigs.wReq.fire & pcuGetDBID === i.U
          pcu.state     := Mux(hit, PCUSM.WaitDBID, pcu.state)
        }
        // State: WaitDBID
        is(PCUSM.WaitDBID) {
          val hit       = io.dbSigs.wResp.fire & pcuRecDBID === i.U
          pcu.state     := Mux(hit, PCUSM.Req2Node, pcu.state)
        }
        // State: Req2Node
        is(PCUSM.Req2Node) {
          val hit       = io.chi.txreq.fire & pcuReq2NodeID === i.U
          pcu.state     := Mux(hit, PCUSM.WaitNodeResp, pcu.state)
        }
        // State: WaitNodeResp
        is(PCUSM.WaitNodeResp) {
          val rxDathit  = io.chi.rxdat.fire & io.chi.rxdat.bits.TxnID === i.U
          pcu.state     := Mux(rxDathit & pcu.isLastBeat, PCUSM.Resp2Slice, pcu.state)
        }
        // State: Resp2Slice
        is(PCUSM.Resp2Slice) {
          val hit       = io.resp2Slice.fire & pcuResp2SliceID === i.U
          pcu.state     := Mux(hit, PCUSM.Free, pcu.state)
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
      when(io.req2Node.fire & pcuGetReqID === i.U) {
        pcu.addr    := addrSaveInPCU
        pcu.reqMes  := reqSaveInPCU
        assert(pcu.state === PCUSM.Free)
      /*
       * Receive DBID From DataBuffer
       */
      }.elsewhen(io.dbSigs.wResp.fire & pcuRecDBID === i.U) {
        pcu.dbid.valid  := true.B
        pcu.dbid.bits   := io.dbSigs.wResp.bits.dbid
        assert(pcu.state === PCUSM.WaitDBID)
      /*
       * Receive Data And Resp From CHI RxDat
       */
      }.elsewhen(io.chi.rxdat.fire & io.chi.rxdat.bits.TxnID === i.U) {
        pcu.getDataNum    := pcu.getDataNum + 1.U
        pcu.respMes.resp  := io.chi.rxdat.bits.Resp
        /*
         * Clean PCU Entry When Its Free
         */
      }.elsewhen(pcu.isFree) {
        pcu               := 0.U.asTypeOf(pcu)
      }
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------ Receive Req From CHITXREQ or Req2Node and Save In PCU --------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Receive req2Node(Snoop)
   */
  addrSaveInPCU         := io.req2Node.bits.addr
  reqSaveInPCU.opcode   := io.req2Node.bits.opcode
  reqSaveInPCU.mshrWay  := io.req2Node.bits.mshrWay
  reqSaveInPCU.useEvict := io.req2Node.bits.useEvict
  reqSaveInPCU.from     := io.req2Node.bits.from
  reqSaveInPCU.mshrWay  := io.req2Node.bits.mshrWay
  reqSaveInPCU.useEvict := io.req2Node.bits.useEvict

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
  val pcuReq2NodeVec  = pcus.map(_.isReq2Node)
  pcuReq2NodeID       := PriorityEncoder(pcuReq2NodeVec)

  io.chi.txreq.valid            := pcuReq2NodeVec.reduce(_ | _)
  io.chi.txreq.bits.Addr        := pcus(pcuReq2NodeID).addr
  io.chi.txreq.bits.Opcode      := pcus(pcuReq2NodeID).reqMes.opcode
  io.chi.txreq.bits.TgtID       := ddrcNodeId.U
  io.chi.txreq.bits.SrcID       := hnfNodeId.U
  io.chi.txreq.bits.TxnID       := pcuReq2NodeID
  io.chi.txreq.bits.Size        := log2Ceil(djparam.blockBytes).U
  io.chi.txreq.bits.MemAttr     := MemAttr(false.B, true.B, false.B, false.B).asUInt
  io.chi.txreq.bits.ExpCompAck  := false.B
  assert(Mux(io.chi.txreq.valid, pcus(pcuReq2NodeID).isReadDDR, true.B), "TODO")



// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------- Receive Data From CHI DAT And Send It To DataBuffer -------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  io.dbSigs.dataTDB.valid       := io.chi.rxdat.valid
  io.dbSigs.dataTDB.bits.dbid   := pcus(io.chi.rxdat.bits.TxnID(param.pcuIdBits-1, 0)).dbid.bits
  io.dbSigs.dataTDB.bits.data   := io.chi.rxdat.bits.Data
  io.dbSigs.dataTDB.bits.dataID := io.chi.rxdat.bits.DataID
  io.chi.rxdat.ready            := io.dbSigs.dataTDB.ready
  assert(Mux(io.chi.rxdat.valid, io.chi.rxdat.bits.TxnID <= param.nrPCUEntry.U, true.B))



// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------- Receive Data From CHI DAT And Send It To DataBuffer -------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  val pcuResp2SliceVec            = pcus.map(_.isResp2Slice)
  pcuResp2SliceID                 := PriorityEncoder(pcuResp2SliceVec)

  io.resp2Slice.valid             := pcuResp2SliceVec.reduce(_ | _)
  io.resp2Slice.bits              := DontCare
  io.resp2Slice.bits.isReqResp    := true.B
  io.resp2Slice.bits.mshrSet      := pcus(pcuResp2SliceID).mSet
  io.resp2Slice.bits.mshrWay      := pcus(pcuResp2SliceID).mshrWay
  io.resp2Slice.bits.useEvict     := pcus(pcuResp2SliceID).useEvict
  io.resp2Slice.bits.from.IncoId  := snMasId.U
  io.resp2Slice.bits.hasData      := pcus(pcuResp2SliceID).dbid.valid
  io.resp2Slice.bits.dbid         := pcus(pcuResp2SliceID).dbid.bits
  io.resp2Slice.bits.resp         := pcus(pcuResp2SliceID).respMes.resp



// ---------------------------  Assertion  --------------------------------//
  val cntReg = RegInit(VecInit(Seq.fill(param.nrPCUEntry) {
    0.U(64.W)
  }))
  cntReg.zip(pcus).foreach { case (c, p) => c := Mux(p.isFree, 0.U, c + 1.U) }
  cntReg.zipWithIndex.foreach { case (c, i) => assert(c < TIMEOUT_SMPCU.U, "SNMAS PCU[0x%x] ADDR[0x%x] OP[0x%x] TIMEOUT", i.U, pcus(i).addr, pcus(i).reqMes.opcode) }
}