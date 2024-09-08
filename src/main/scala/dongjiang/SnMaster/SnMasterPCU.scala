package DONGJIANG.SNMASTER

import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIOp.REQ._
import DONGJIANG.IdL0.INTF
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.FastArb._
import Utils.IDConnector.idSelDec2DecVec

/*
 * Read Req:  [Free] -----> [GetDBID] -----> [WaitDBID] -----> [Req2Node] -----> [WaitNodeResp] -----> [Resp2Slice]
 *
 * Write Req: [Free] -----> [Req2Node] -----> [WaitNodeDBID] -----> [RCDB] -----> [WriteData2Node] -----> [Resp2Slice]
 *
 * Replace:   [Free] -----> [Req2Node] -----> [WaitNodeDBID] -----> [Req2Node] -----> [WaitNodeResp_WriteData2Node] -----> [WriteData2Node] -----> [Resp2Slice]
 */




object PCUState {
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

class PCUEntry(param: InterfaceParam)(implicit p: Parameters) extends DJBundle with HasAddr {
  val state         = UInt(PCUState.width.W)
  val reqMes        = new DJBundle with HasFromIDBits with HasMSHRWay {
    val opcode      = UInt(6.W)
  }
  val DBID          = Valid(Bool())

  def isFree        = state === PCUState.Free
  def isGetDBID     = state === PCUState.Free
}

class SnMasterPCU(snMasId: Int, param: InterfaceParam)(implicit p: Parameters) extends PCUBaseIO(isSlv = false, hasReq2Slice = false, hasDBRCReq = true) {
  io <> DontCare
// --------------------- Reg and Wire declaration ------------------------//
  val pcus          = RegInit(VecInit(Seq.fill(param.nrPCUEntry) { 0.U.asTypeOf(new PCUEntry(param)) })); dontTouch(pcus)
  // PCU Receive Req ID
  val pcuGetReqID   = Wire(UInt(param.pcuIdBits.W))
  // PCU Get DBID ID
  val pcuGetDBID    = Wire(UInt(param.pcuIdBits.W))
  // PCU Receive DBID ID
  val pcuRecDBID    = Wire(UInt(param.pcuIdBits.W))
  // req from slice
  val addrSaveInPCU = Wire(UInt(addressBits.W))
  val reqSaveInPCU  = WireInit(0.U.asTypeOf(pcus(0).reqMes))


// ---------------------------------------------------------------------------------------------------------------------- //
// --------------------------------------------------- PCU: State Transfer ---------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  pcus.zipWithIndex.foreach {
    case (pcu, i) =>
      switch(pcu.state) {
        // State: Free
        is(PCUState.Free) {
          val reqHit    = io.req2Node.fire & isReadX(io.req2Node.bits.opcode) & pcuGetReqID === i.U
          val writeHit  = io.req2Node.fire & isWriteX(io.req2Node.bits.opcode) & pcuGetReqID === i.U
          val replHit   = io.req2Node.fire & io.req2Node.bits.replace & pcuGetReqID === i.U
          pcu.state     := Mux(reqHit, PCUState.GetDBID,
                            Mux(writeHit, PCUState.Req2Node,
                              Mux(replHit, PCUState.Req2Node, pcu.state)))
        }
        // State: GetDBID
        is(PCUState.GetDBID) {
          val hit       = io.dbSigs.wReq.fire & pcuGetDBID === i.U
          pcu.state     := Mux(hit, PCUState.GetDBID, pcu.state)
        }
        // State: WaitDBID
        is(PCUState.WaitDBID) {
          val hit       = io.dbSigs.wResp.fire & pcuRecDBID === i.U
          pcu.state     := Mux(hit, PCUState.Req2Node, pcu.state)
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
        assert(pcu.state === PCUState.Free)
      }.elsewhen(io.dbSigs.wResp.fire & pcuRecDBID === i.U) {
        pcu.DBID.valid  := true.B
        pcu.DBID.bits   := io.dbSigs.wResp.bits.dbid
        assert(pcu.state === PCUState.WaitDBID)
      }
  }


// ---------------------------------------------------------------------------------------------------------------------- //
// ------------------------------ Receive Req From CHITXREQ or Req2Node and Save In PCU---------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Receive req2Node(Snoop)
   */
  addrSaveInPCU         := io.req2Node.bits.addr
  reqSaveInPCU.opcode   := io.req2Node.bits.opcode
  reqSaveInPCU.mshrWay  := io.req2Node.bits.mshrWay
  reqSaveInPCU.useEvict := io.req2Node.bits.useEvict
  reqSaveInPCU.from     := io.req2Node.bits.from

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
// --------------------------------- Get DBID From DataBuffer and Wait DataBuffer Resp----------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  /*
   * Send Get DBID Req To From DataBuffer
   */
  val pcuGetDBIDVec     = pcus.map(_.isGetDBID)
  pcuGetDBID            := PriorityEncoder(pcuGetDBIDVec)

  /*
   * Set DataBuffer Req Value
   */
  io.dbSigs.wReq.valid          := pcuGetDBIDVec.reduce(_ | _)
  io.dbSigs.wReq.bits.from.idL0 := INTF.U
  io.dbSigs.wReq.bits.from.idL1 := snMasId.U
  io.dbSigs.wReq.bits.from.idL2 := pcuGetDBID

  /*
   * Receive DBID From DataBuffer
   */
  pcuRecDBID := io.dbSigs.wResp.bits.to.pcuId
  io.dbSigs.wResp.ready := true.B


// ---------------------------  Assertion  --------------------------------//
  val cntReg = RegInit(VecInit(Seq.fill(param.nrPCUEntry) {
    0.U(64.W)
  }))
  cntReg.zip(pcus).foreach { case (c, p) => c := Mux(p.isFree, 0.U, c + 1.U) }
  cntReg.zipWithIndex.foreach { case (c, i) => assert(c < TIMEOUT_PCU.U, "SNMAS PCU[0x%x] ADDR[0x%x] OP[0x%x] FROM[0x%x] TIMEOUT", i.U, pcus(i).addr, pcus(i).reqMes.opcode, pcus(i).reqMes.from.idL0) }
}