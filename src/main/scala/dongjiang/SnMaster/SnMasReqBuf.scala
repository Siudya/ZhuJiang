package DONGJIANG.SNMASTER

import DONGJIANG._
import DONGJIANG.IdL0._
import DONGJIANG.CHI._
import chisel3._
import org.chipsalliance.cde.config._
import chisel3.util.{Cat, Decoupled, PopCount, RegEnable, Valid, ValidIO, log2Ceil}

class RBFSMState(implicit p: Parameters) extends Bundle {
  // schedule
  val s_req       = Bool() // to CHI REQ { Read / Write }
  val s_rcDB      = Bool() // when need to write back data or snpFwd need retToSrc
  val s_data      = Bool() // to CHI DAT when write back data or snpFwd resp compData

  val s_getDBID   = Bool() // when need to send Read
  val s_reqUdpMSHR  = Bool() // to Slice when send CompAck

  // wait
  val w_dbidResp  = Bool() // from CHI RSP
  val w_dbData    = Bool()

  val w_dbid      = Bool()
  val w_snResp    = Bool() // from CHI RSP or DAT
}


class SnMasReqBuf(snMasId: Int, reqBufId: Int, param: InterfaceParam)(implicit p: Parameters) extends NodeBase(isSlv = false, hasFree = true, hasDBRCReq = true) {
  /*
   * Connect Unuse CHI Channels
   */
  io.chi.txrsp <> DontCare
  io.chi.rxsnp <> DontCare

// --------------------- Reg and Wire declaration ------------------------//
  // reqBuf Ctrl
  val freeReg       = RegInit(true.B)
  val fsmReg        = RegInit(0.U.asTypeOf(new RBFSMState))
  // req reg
  val reqReg        = RegInit(0.U.asTypeOf(new DJBundle with HasFromIDBits {
    val addr        = UInt(addressBits.W)
    val opcode      = UInt(6.W)
  }))
  // req from slice or txreq
  val req2Node      = WireInit(0.U.asTypeOf(reqReg))
  val reqIsWrite    = WireInit(false.B)
  // data crtl
  val getDBNumReg       = RegInit(0.U(log2Ceil(nrBeat + 1).W))
  val getRxDatNumReg    = RegInit(0.U(log2Ceil(nrBeat + 1).W))
  val getAllData        = WireInit(false.B) // get all Data from DataBuffer or TxDat
  val dbidReg           = RegInit(0.U(dbIdBits.W))
  val dbidBankIdReg     = RegInit(0.U(dbIdBits.W)) // dbid from which bank
  val sendTxDatNumReg   = RegInit(0.U(log2Ceil(nrBeat * 2 + 1).W))
  val reqSendAllData    = WireInit(false.B) // Send all Data to HN when need to send WriteData
  // req resp to slice req
  val reqRespDBIDReg    = RegInit(0.U(chiParams.dbidBits.W))


// ---------------------------  ReqBuf State release/alloc/set logic --------------------------------//
  /*
   * ReqBuf release logic
   */
  val alloc = io.req2Node.fire
  val release = fsmReg.asUInt === 0.U // all s_task / w_task done
  freeReg := Mux(release & !alloc, true.B, Mux(alloc, false.B, freeReg))
  io.free := freeReg

  /*
   * Alloc or Set state
   */
  when(io.req2Node.fire & reqIsWrite) {
    // send
    fsmReg.s_req      := true.B
    fsmReg.s_rcDB     := true.B
    fsmReg.s_data     := true.B
    fsmReg.s_reqUdpMSHR := true.B
    // wait
    fsmReg.w_dbidResp := true.B
    fsmReg.w_dbData   := true.B
  }.elsewhen(io.req2Node.fire) {
    // send
    fsmReg.s_req      := true.B
    fsmReg.s_getDBID  := true.B
    fsmReg.s_reqUdpMSHR := true.B
    // wait
    fsmReg.w_dbid     := true.B
    fsmReg.w_snResp   := true.B
  }.otherwise {
    /*
     * Commmon
     */
    fsmReg.s_req      := Mux(io.chi.txreq.fire,                 false.B, fsmReg.s_req)
    fsmReg.s_reqUdpMSHR := Mux(io.resp2Slice.fire,              false.B, fsmReg.s_reqUdpMSHR)


    /*
     * Write Req
     */
    fsmReg.s_data     := Mux(io.chi.txdat.fire & reqSendAllData,    false.B, fsmReg.s_data)
    fsmReg.s_rcDB     := Mux(io.dbSigs.dbRCReq.fire,                false.B, fsmReg.s_rcDB)
    fsmReg.w_dbidResp := Mux(io.chi.rxrsp.fire,                     false.B, fsmReg.w_dbidResp)
    fsmReg.w_dbData   := Mux(io.dbSigs.dataFDB.valid & getAllData,  false.B, fsmReg.w_dbData)



    /*
     * Read Req
     */
    fsmReg.s_getDBID  := Mux(io.dbSigs.wReq.fire,                   false.B, fsmReg.s_getDBID)
    fsmReg.w_dbid     := Mux(io.dbSigs.wResp.fire,                  false.B, fsmReg.w_dbid)
    fsmReg.w_snResp   := Mux(io.chi.rxrsp.fire | (io.chi.rxdat.fire & getAllData), false.B, fsmReg.w_snResp)
  }


// ---------------------------  Receive Req(TxReq and req2Node) Logic --------------------------------//
  /*
   * Receive req2Node(Read / Dataless / Atomic / CMO / Write)
   */
  req2Node.addr       := io.req2Node.bits.addr
  req2Node.opcode     := io.req2Node.bits.opcode
  req2Node.from       := io.req2Node.bits.from
  reqIsWrite          := CHIOp.REQ.isWriteX(io.req2Node.bits.opcode)

  /*
   * Save req2Node
   */
  reqReg              := Mux(io.req2Node.fire, req2Node, reqReg)


// ---------------------------  Receive CHI Resp(RxRsp and RxDat) Logic --------------------------------//
  /*
   * Receive RxRsp or RxDat
   */
  // Receive RxRsp
  when(io.chi.rxrsp.fire & fsmReg.w_snResp) {
    reqRespDBIDReg    := io.chi.rxrsp.bits.dbID
  }
  // Receive RxDat
  getRxDatNumReg      := Mux(release, 0.U, getRxDatNumReg + io.chi.rxdat.fire.asUInt)
  // Set Ready value
  io.chi.rxrsp.ready  := true.B
  io.chi.rxdat.ready  := true.B


// ---------------------------  Send CHI Req or Resp(TxReq and TxDat) Logic --------------------------------//
  /*
   * Send TxReq
   */
  io.chi.txreq.valid        := fsmReg.s_req & !fsmReg.w_dbid
  io.chi.txreq.bits         := DontCare
  io.chi.txreq.bits.tgtID   := 0.U
  io.chi.txreq.bits.srcID   := snMasId.U
  io.chi.txreq.bits.txnID   := reqBufId.U
  io.chi.txreq.bits.opcode  := reqReg.opcode
  io.chi.txreq.bits.size    := log2Ceil(djparam.blockBytes).U
  io.chi.txreq.bits.addr    := reqReg.addr
  io.chi.txreq.bits.memAttr := MemAttr(false.B, true.B, false.B, false.B)
  io.chi.txreq.bits.expCompAck := false.B


  /*
   * Send TxDat(WriteData or CompData or SnpRespData)
   */
  io.chi.txdat.valid        := fsmReg.s_data & fsmReg.w_dbData & io.dbSigs.dataFDB.valid
  io.chi.txdat.bits         := DontCare
  io.chi.txdat.bits.opcode  := CHIOp.DAT.NonCopyBackWrData
  io.chi.txdat.bits.tgtID   := 0.U
  io.chi.txdat.bits.srcID   := snMasId.U
  io.chi.txdat.bits.txnID   := reqRespDBIDReg
  io.chi.txdat.bits.respErr := RespErr.NormalOkay // TODO: Complete data error indicate
  // Count
  sendTxDatNumReg           := Mux(release, 0.U, sendTxDatNumReg + io.chi.txdat.fire)
  reqSendAllData            := sendTxDatNumReg === nrBeat.U       | (sendTxDatNumReg === (nrBeat - 1).U & io.chi.txdat.fire)


// --------------------------- Send resp2Slice --------------------------------//
  /*
   * Send Resp To Slice
   * send update MSHR and send snoop resp also use resp2Slice
   */
  io.resp2Slice.valid           := fsmReg.s_reqUdpMSHR & PopCount(fsmReg.asUInt) === 1.U // only udpMSHR need to do
  io.resp2Slice.bits            := DontCare
  io.resp2Slice.bits.resp       := ChiResp.UC
  io.resp2Slice.bits.isSnpResp  := false.B // TODO
  io.resp2Slice.bits.hasData    := true.B
  io.resp2Slice.bits.dbid       := dbidReg
  io.resp2Slice.bits.mshrSet    := parseMSHRAddress(reqReg.addr)._1
  io.resp2Slice.bits.fwdState   := DontCare
  // IdMap
  io.resp2Slice.bits.to         := reqReg.from
  io.resp2Slice.bits.from.idL0  := SNMAS
  io.resp2Slice.bits.from.idL1  := snMasId.U
  io.resp2Slice.bits.from.idL2  := reqBufId.U


// -----------------------------------------------  DataBuffer Ctrl Signals  ------------------------------------------//
  /*
   * Send Read and Clean Req to DataBuffer
   */
  //                            WriteBack Data
  io.dbSigs.dbRCReq.valid          := fsmReg.s_rcDB & fsmReg.s_data & !fsmReg.w_dbidResp
  io.dbSigs.dbRCReq.bits.isRead    := true.B
  io.dbSigs.dbRCReq.bits.isClean   := true.B
  io.dbSigs.dbRCReq.bits.to        := reqReg.from
  io.dbSigs.dbRCReq.bits.from.idL0 := SNMAS
  io.dbSigs.dbRCReq.bits.from.idL1 := snMasId.U
  io.dbSigs.dbRCReq.bits.from.idL2 := reqBufId.U
  io.dbSigs.dbRCReq.bits.mshrSet   := parseMSHRAddress(reqReg.addr)._2
  io.dbSigs.dbRCReq.bits.dbid      := DontCare
  io.dbSigs.dbRCReq.bits.useDBID   := false.B

  /*
   * Send Data To DataBuffer
   */
  io.dbSigs.dataTDB.valid           := io.chi.rxdat.valid
  io.dbSigs.dataTDB.bits.data       := DontCare
  io.dbSigs.dataTDB.bits.dataID     := DontCare
  io.dbSigs.dataTDB.bits.dbid       := dbidReg
  io.dbSigs.dataTDB.bits.to.idL0    := IdL0.SLICE
  io.dbSigs.dataTDB.bits.to.idL1    := dbidBankIdReg
  io.dbSigs.dataTDB.bits.to.idL2    := DontCare

  /*
   * Send wReq to get dbid
   */
  io.dbSigs.wReq.valid           := fsmReg.s_getDBID
  // IdMap
  io.dbSigs.wReq.bits.to.idL0    := IdL0.SLICE
  io.dbSigs.wReq.bits.to.idL1    := parseAddress(reqReg.addr)._2 // Remap in Xbar
  io.dbSigs.wReq.bits.to.idL2    := DontCare
  io.dbSigs.wReq.bits.from.idL0  := SNMAS
  io.dbSigs.wReq.bits.from.idL1  := snMasId.U
  io.dbSigs.wReq.bits.from.idL2  := reqBufId.U

  /*
   * Receive dbid from wResp
   */
  dbidReg       := Mux(io.dbSigs.wResp.fire, io.dbSigs.wResp.bits.dbid, dbidReg)
  dbidBankIdReg := Mux(io.dbSigs.wResp.fire, io.dbSigs.wResp.bits.from.idL1, dbidBankIdReg)

  /*
  * Count data get from DataBuffer number
  */
  getDBNumReg   := Mux(release, 0.U, getDBNumReg + io.dbSigs.dataFDB.valid.asUInt)


// ---------------------------  Other Signals  --------------------------------//
  /*
   * getAllData logic
   */
  getAllData := getRxDatNumReg === nrBeat.U     | (getRxDatNumReg === (nrBeat - 1).U & io.chi.rxdat.fire) |   //get data from rxdat
                getDBNumReg === nrBeat.U        | (getDBNumReg === (nrBeat - 1).U & io.dbSigs.dataFDB.valid)  //get data from db


  /*
   * Set io ready value
   */
  io.chi.rxrsp.ready      := true.B
  io.chi.rxdat.ready      := io.dbSigs.dataTDB.ready
  io.req2Node.ready       := true.B
  io.dbSigs.wResp.ready   := true.B
  io.dbSigs.dataFDB.ready := io.chi.txdat.ready


  // ---------------------------  Assertion  --------------------------------//
  // when it is free, it can receive or send mes
  assert(Mux(io.free, !io.chi.txreq.valid, true.B))
  assert(Mux(io.free, !io.chi.txdat.valid, true.B))
  assert(Mux(io.free, !io.chi.rxdat.valid, true.B))
  assert(Mux(io.free, !io.chi.rxrsp.valid, true.B))
  assert(Mux(io.free, !io.resp2Slice.valid, true.B))
  assert(Mux(io.free, !io.dbSigs.dbRCReq.valid, true.B))
  assert(Mux(io.free, !io.dbSigs.wReq.valid, true.B))
  assert(Mux(io.free, !io.dbSigs.wResp.valid, true.B))
  assert(Mux(io.free, !io.dbSigs.dataFDB.valid, true.B))
  assert(Mux(io.free, !io.dbSigs.dataTDB.valid, true.B))

  assert(Mux(io.req2Node.valid, io.req2Node.bits.opcode === CHIOp.REQ.ReadNoSnp | io.req2Node.bits.opcode === CHIOp.REQ.WriteNoSnpFull, true.B))

  assert(Mux(!freeReg, !io.req2Node.valid, true.B), "When ReqBuf valid, it cant input new req")
  assert(Mux(io.req2Node.valid, io.free, true.B), "Reqbuf cant block req input")
  when(release) {
    assert(fsmReg.asUInt === 0.U, "when ReqBuf release, all task should be done")
  }
  assert(Mux(getDBNumReg === (nrBeat * 2).U, !io.dbSigs.dataFDB.valid, true.B), "ReqBuf get data from DataBuf overflow")
  assert(Mux(io.dbSigs.dataFDB.valid, fsmReg.s_data & fsmReg.w_dbData, true.B), "When dbDataValid, ReqBuf should set s_data and w_dbData")

  val cntReg = RegInit(0.U(64.W))
  cntReg := Mux(io.free, 0.U, cntReg + 1.U)
  assert(cntReg < TIMEOUT_RB.U, "REQBUF[0x%x] ADDR[0x%x] OP[0x%x] SNP[0x%x] TIMEOUT", reqBufId.U, reqReg.addr, reqReg.opcode, reqReg.from.isSLICE.asUInt)



















}