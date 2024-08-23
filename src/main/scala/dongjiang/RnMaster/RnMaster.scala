package DONGJIANG.RNMASTER

import DONGJIANG. _
import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.FastArb._
import Utils.IDConnector.idSelDec2DecVec

class RnMaster(rnMasId: Int, param: InterfaceParam)(implicit p: Parameters) extends NodeBase(isSlv = false, hasReq2Slice = true, hasDBRCReq = true) {
// --------------------- IO declaration ------------------------//
  val chi = IO(CHIBundleDecoupled(chiParams))

// --------------------- Modules declaration ------------------------//
  def createReqBuf(id: Int) = { val reqBuf = Module(new RnMasReqBuf(rnMasId, id, param)); reqBuf }
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
   * connect io.chi.rx <-> reqBufs.chi.rx
   */
  reqBufs.map(_.io.chi).zipWithIndex.foreach {
    case(reqBuf, i) =>
      // rxsnp
      reqBuf.rxsnp.valid  := io.chi.rxsnp.valid & reqSelId0 === i.U & canReceive0
      reqBuf.rxsnp.bits   := io.chi.rxsnp.bits
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
  io.chi.rxsnp.ready  := canReceive0
  io.chi.rxrsp.ready  := true.B
  io.chi.rxdat.ready  := true.B


  /*
   * connect io.chi.tx <-> reqBufs.chi.tx
   */
  fastArbDec2Dec(reqBufs.map(_.io.chi.txreq), io.chi.txreq)
  fastArbDec2Dec(reqBufs.map(_.io.chi.txrsp), io.chi.txrsp)
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
  fastArbDec2Dec(reqBufs.map(_.io.req2Slice), io.req2Slice)
  idSelDec2DecVec(io.resp2Node, reqBufs.map(_.io.resp2Node), level = 2)
  fastArbDec2Dec(reqBufs.map(_.io.resp2Slice), io.resp2Slice)
  reqBufs.zipWithIndex.foreach {
    case (reqBuf, i) =>
      reqBuf.io.req2Node.valid := io.req2Node.valid & reqSelId1 === i.U & canReceive1
      reqBuf.io.req2Node.bits  := io.req2Node.bits
  }
  io.req2Node.ready := canReceive1



// --------------------- Assertion ------------------------------- //
  if(param.addressIdBits.getOrElse(0) > 0) {
    assert(Mux(io.chi.rxsnp.valid, io.chi.rxsnp.bits.addr(addressBits - 3 - 1, addressBits - 3 - param.addressIdBits.get) === param.addressId.get.U, true.B))
    assert(Mux(io.req2Node.valid, io.req2Node.bits.addr(addressBits - 1, addressBits - param.addressIdBits.get) === param.addressId.get.U, true.B))
  }

  assert(Mux(io.chi.rxrsp.valid, PopCount(reqBufs.map(_.io.chi.rxrsp.fire)) === 1.U, true.B))
  assert(Mux(io.chi.rxdat.valid, PopCount(reqBufs.map(_.io.chi.rxdat.fire)) === 1.U, true.B))

}