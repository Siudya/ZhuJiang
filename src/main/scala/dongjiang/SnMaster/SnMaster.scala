package DONGJIANG.SNMASTER

import DONGJIANG. _
import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.FastArb._
import Utils.IDConnector.idSelDec2DecVec

class SnMaster(snMasId: Int, param: InterfaceParam)(implicit p: Parameters) extends NodeBase(isSlv = false, hasReq2Slice = false, hasDBRCReq = true) {
// --------------------- Modules declaration ------------------------//
  def createReqBuf(id: Int) = { val reqBuf = Module(new SnMasReqBuf(snMasId, id, param)); reqBuf }
  val reqBufs               = (0 until param.nrReqBuf).map(i => createReqBuf(i))

// --------------------- Wire declaration ------------------------//
  val reqSelId              = Wire(UInt(param.reqBufIdBits.W))
  val canReceive            = Wire(Bool())

// ------------------------ Connection ---------------------------//
  /*
   * Connect Unuse CHI Channels
   */
  io.chi.txrsp <> DontCare
  io.chi.rxsnp <> DontCare


  /*
   * ReqBuf Select
   */
  reqSelId    := PriorityEncoder(reqBufs.map(_.io.free))
  canReceive  := reqBufs.map(_.io.free).reduce(_ | _)

  /*
   * connect io.chi.rx <-> reqBufs.chi.rx
   */
  reqBufs.map(_.io.chi).zipWithIndex.foreach {
    case(reqBuf, i) =>
      reqBuf.rxsnp        <> DontCare
      // rxrsp
      reqBuf.rxrsp.valid  := io.chi.rxrsp.valid & io.chi.rxrsp.bits.txnID === i.U
      reqBuf.rxrsp.bits   := io.chi.rxrsp.bits
      // rxdat
      reqBuf.rxdat.valid  := io.chi.rxdat.valid & io.chi.rxdat.bits.txnID === i.U
      reqBuf.rxdat.bits   := io.chi.rxdat.bits
      reqBuf.rxdat.bits.data    := DontCare
      reqBuf.rxdat.bits.dataID  := DontCare
  }

  // Set io.chi.rx_xxx.ready value
  io.chi.rxrsp.ready  := true.B
  io.chi.rxdat.ready  := true.B


  /*
   * connect io.chi.tx <-> reqBufs.chi.tx
   */
  reqBufs.map(_.io.chi.txrsp <> DontCare)
  fastArbDec2Dec(reqBufs.map(_.io.chi.txreq), io.chi.txreq)
  fastArbDec2Dec(reqBufs.map(_.io.chi.txdat), io.chi.txdat)


  /*
   * Connect slice DataBuffer signals
   */
  fastArbDec2Dec(reqBufs.map(_.io.dbSigs.dbRCReq), io.dbSigs.dbRCReq)
  fastArbDec2Dec(reqBufs.map(_.io.dbSigs.wReq), io.dbSigs.wReq)
  idSelDec2DecVec(io.dbSigs.wResp, reqBufs.map(_.io.dbSigs.wResp), level = 2)
  fastArbDec2Dec(reqBufs.map(_.io.dbSigs.dataTDB), io.dbSigs.dataTDB)
  idSelDec2DecVec(io.dbSigs.dataFDB, reqBufs.map(_.io.dbSigs.dataFDB), level = 2)

  io.dbSigs.dataTDB.bits.data   := io.chi.rxdat.bits.data
  io.dbSigs.dataTDB.bits.dataID := io.chi.rxdat.bits.dataID

  io.chi.txdat.bits.data      := io.dbSigs.dataFDB.bits.data
  io.chi.txdat.bits.dataID    := io.dbSigs.dataFDB.bits.dataID

  /*
   * Connect Slice Ctrl Signals
   */
  fastArbDec2Dec(reqBufs.map(_.io.resp2Slice), io.resp2Slice)
  reqBufs.zipWithIndex.foreach {
    case (reqBuf, i) =>
      reqBuf.io.req2Node.valid := io.req2Node.valid & reqSelId === i.U & canReceive
      reqBuf.io.req2Node.bits  := io.req2Node.bits
  }
  io.req2Node.ready := canReceive



// --------------------- Assertion ------------------------------- //
  if (param.addressIdBits.getOrElse(0) > 0) {
    assert(Mux(io.req2Node.valid, io.req2Node.bits.addr(addressBits - 1, addressBits - param.addressIdBits.get) === param.addressId.get.U, true.B))
  }

  assert(Mux(io.chi.rxrsp.valid, PopCount(reqBufs.map(_.io.chi.rxrsp.fire)) === 1.U, true.B))
  assert(Mux(io.chi.rxdat.valid, PopCount(reqBufs.map(_.io.chi.rxdat.fire)) === 1.U, true.B))

}