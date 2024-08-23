package DONGJIANG.RNSLAVE

import DONGJIANG. _
import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.FastArb._
import Utils.IDConnector.idSelDec2DecVec

class RnSlave(rnSlvId: Int, param: InterfaceParam)(implicit p: Parameters) extends NodeBase(isSlv = true, hasReq2Slice = true, hasDBRCReq = false) {
// --------------------- Modules declaration ------------------------//
  def createReqBuf(id: Int) = { val reqBuf = Module(new RnSlvReqBuf(rnSlvId, id, param)); reqBuf }
  val reqBufs               = (0 until param.nrReqBuf).map(i => createReqBuf(i))
  val reqSel                = Module(new ReqBufSelector(param))
//  val nestCtl         = Module(new NestCtl()) // TODO: Nest Ctrl

// --------------------- Wire declaration ------------------------//
  val reqSelId0   = Wire(UInt(param.reqBufIdBits.W)) // Priority
  val reqSelId1   = Wire(UInt(param.reqBufIdBits.W))

  val canReceive0 = Wire(Bool())
  val canReceive1 = Wire(Bool())

// ------------------------ Connection ---------------------------//
  /*
   * ReqBufSelector idle input
   */
  reqSel.io.idle  := reqBufs.map(_.io.free)
  reqSelId0       := reqSel.io.out0
  reqSelId1       := reqSel.io.out0
  canReceive0     := reqSel.io.idleNum > 0.U
  canReceive1     := reqSel.io.idleNum > 1.U



  /*
   * connect io.chi.tx <-> reqBufs.chi.tx
   */
  reqBufs.map(_.io.chi).zipWithIndex.foreach {
    case(reqBuf, i) =>
      // txreq
      reqBuf.txreq.valid  := io.chi.txreq.valid & reqSelId1 === i.U & canReceive1
      reqBuf.txreq.bits   := io.chi.txreq.bits
      // txrsp
      reqBuf.txrsp.valid  := io.chi.txrsp.valid & io.chi.txrsp.bits.txnID === i.U
      reqBuf.txrsp.bits   := io.chi.txrsp.bits
      // txdat
      reqBuf.txdat.valid  := io.chi.txdat.valid & io.chi.txrsp.bits.txnID === i.U
      reqBuf.txdat.bits   := io.chi.txdat.bits
      reqBuf.txdat.bits.data    := DontCare
      reqBuf.txdat.bits.dataID  := DontCare
  }

  // Set io.chi.tx_xxx.ready value
  io.chi.txreq.ready  := canReceive1
  io.chi.txrsp.ready  := true.B
  io.chi.txdat.ready  := true.B


  /*
   * connect io.chi.rx <-> reqBufs.chi.rx
   */
  fastArbDec2Dec(reqBufs.map(_.io.chi.rxsnp), io.chi.rxsnp)
  fastArbDec2Dec(reqBufs.map(_.io.chi.rxrsp), io.chi.rxrsp)
  fastArbDec2Dec(reqBufs.map(_.io.chi.rxdat), io.chi.rxdat)

  /*
   * Connect slice DataBuffer signals
   */
  fastArbDec2Dec(reqBufs.map(_.io.dbSigs.wReq), io.dbSigs.wReq)
  idSelDec2DecVec(io.dbSigs.wResp, reqBufs.map(_.io.dbSigs.wResp), level = 2)
  fastArbDec2Dec(reqBufs.map(_.io.dbSigs.dataTDB), io.dbSigs.dataTDB)
  idSelDec2DecVec(io.dbSigs.dataFDB, reqBufs.map(_.io.dbSigs.dataFDB), level = 2)

  io.dbSigs.dataTDB.bits.data   := io.chi.txdat.bits.data
  io.dbSigs.dataTDB.bits.dataID := io.chi.txdat.bits.dataID

  io.chi.rxdat.bits.data        := io.dbSigs.dataFDB.bits.data
  io.chi.rxdat.bits.dataID      := io.dbSigs.dataFDB.bits.dataID

  /*
   * Connect Slice Ctrl Signals
   */
  fastArbDec2Dec(reqBufs.map(_.io.req2Slice), io.req2Slice)
  idSelDec2DecVec(io.resp2Node, reqBufs.map(_.io.resp2Node), level = 2)
  fastArbDec2Dec(reqBufs.map(_.io.resp2Slice), io.resp2Slice)
  reqBufs.zipWithIndex.foreach {
    case (reqBuf, i) =>
      reqBuf.io.req2Node.valid := io.req2Node.valid & reqSelId0 === i.U & canReceive0
      reqBuf.io.req2Node.bits  := io.req2Node.bits
  }
  io.req2Node.ready := canReceive0



// --------------------- Assertion ------------------------------- //
  assert(Mux(io.chi.txrsp.valid, PopCount(reqBufs.map(_.io.chi.txrsp.fire)) === 1.U, true.B))
  assert(Mux(io.chi.txdat.valid, PopCount(reqBufs.map(_.io.chi.txdat.fire)) === 1.U, true.B))

}