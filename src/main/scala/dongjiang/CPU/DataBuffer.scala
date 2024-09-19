package DONGJIANG.CPU

import DONGJIANG._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.Encoder.RREncoder


object DBState {
  val width       = 3
  // FREE -> ALLOC -> FREE
  // FREE -> ALLOC -> READING(needClean) -> READ(needClean) -> FREE
  // FREE -> ALLOC -> READING(!needClean) -> READ(!needClean) -> READ_DONE -> READING(needClean) -> READ(needClean) -> FREE
  val FREE        = "b000".U
  val ALLOC       = "b001".U
  val READ        = "b010".U // Ready to read
  val READING     = "b011".U // Already partially read
  val READ_DONE   = "b100".U // Has been read all beat
}


class DBEntry(implicit p: Parameters) extends DJBundle with HasToIDBits {
  val state       = UInt(DBState.width.W)
  val beatRNum    = UInt(log2Ceil(nrBeat).W)
  val needClean   = Bool()
  val beats       = Vec(nrBeat, UInt(beatBits.W))

  def getBeat     = beats(beatRNum)
  def toDataID: UInt = {
    if (nrBeat == 1) { 0.U }
    else if (nrBeat == 2) { Mux(beatRNum === 0.U, "b00".U, "b10".U) }
    else { beatRNum }
  }

  def isFree      = state === DBState.FREE
  def isAlloc     = state === DBState.ALLOC
}


class DataBuffer()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(Flipped(new DBBundle(hasDBRCReq = true)))

  // Del it
  io <> DontCare
  dontTouch(io)


// --------------------- Reg and Wire declaration ------------------------//
  val entrys    = RegInit(VecInit(Seq.fill(djparam.nrDatBuf) { 0.U.asTypeOf(new DBEntry()) }))
  // wResp
  val wRespQ    = Module(new Queue(new DBWResp, 1, flow = false, pipe = true))




// ---------------------------------------------------------------------------------------------------------------------- //
// ----------------------------------------------------- WREQ & WRESP --------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  val dbFreeVec             = entrys.map(_.isFree)
  val wReqId                = PriorityEncoder(dbFreeVec)

  io.wReq.ready             := wRespQ.io.enq.ready & dbFreeVec.reduce(_ | _)
  wRespQ.io.enq.valid       := io.wReq.valid & dbFreeVec.reduce(_ | _)
  wRespQ.io.enq.bits.dbid   := wReqId
  wRespQ.io.enq.bits.to     := io.wReq.bits.from
  wRespQ.io.enq.bits.pcuId  := io.wReq.bits.pcuId

  io.wResp                  <> wRespQ.io.deq


// ---------------------------------------------------------------------------------------------------------------------- //
// ----------------------------------------------------- DATA TO DB ----------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  when(io.dataTDB.valid){ entrys(io.dataTDB.bits.dbid).beats(toBeatNum(io.dataTDB.bits.dataID)) := io.dataTDB.bits.data }
  io.dataTDB.ready := true.B


// ---------------------------------------------------------------------------------------------------------------------- //
// ---------------------------------------------------- RC REQ TO DB ---------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  when(io.dbRCReqOpt.get.valid) {
    entrys(io.dbRCReqOpt.get.bits.dbid).needClean := io.dbRCReqOpt.get.bits.isClean
    entrys(io.dbRCReqOpt.get.bits.dbid).to        := io.dbRCReqOpt.get.bits.to
  }
  io.dbRCReqOpt.get.ready := true.B


// ---------------------------------------------------------------------------------------------------------------------- //
// ----------------------------------------------------- State Transfer ------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  entrys.map(_.state).zipWithIndex.foreach {
    case (s, i) =>
      switch(s) {
        // FREE
        is(DBState.FREE) {
          val hit = wRespQ.io.enq.fire & wReqId === i.U
          s := Mux(hit, DBState.ALLOC, s)
        }
        // ALLOC
        is(DBState.ALLOC) {
          val hit   = io.dbRCReqOpt.get.fire & io.dbRCReqOpt.get.bits.dbid === i.U
          val read  = io.dbRCReqOpt.get.bits.isRead
          val clean = io.dbRCReqOpt.get.bits.isClean
          s := Mux(read, DBState.READ, Mux(clean, DBState.FREE, s))
        }
      }
  }


// ----------------------------------------------------- Assertion ---------------------------------------------------------- //
  when(io.dataTDB.valid){ assert(entrys(io.dataTDB.bits.dbid).isAlloc) }


}
















