package DONGJIANG.CPU

import DONGJIANG._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Utils.Encoder.RREncoder


object DBState {
  val width       = 3
  // FREE -> ALLOC -> WRITING -> WRITE_DONE -> FREE
  // FREE -> ALLOC -> WRITING -> WRITE_DONE -> READING(needClean) -> READ(needClean) -> FREE
  // FREE -> ALLOC -> WRITING -> WRITE_DONE -> READING(!needClean) -> READ(!needClean) -> READ_DONE -> READING(needClean) -> READ(needClean) -> FREE
  val FREE        = "b000".U
  val ALLOC       = "b001".U
  val WRITTING    = "b010".U // Has been written some beats
  val WRITE_DONE  = "b011".U // Has been written all beats
  val READ        = "b100".U // Ready to read
  val READING     = "b101".U // Already partially read
  val READ_DONE   = "b110".U // Has been read all beat
}


class DBEntry(implicit p: Parameters) extends DJBundle with HasToIDBits {
  val state       = UInt(DBState.width.W)
  val beatVals    = Vec(nrBeat, Bool())
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
  val dbFreeVec           = entrys.map(_.isFree)
  val wReqId              = PriorityEncoder(dbFreeVec)

  io.wReq.ready           := wRespQ.io.enq.ready & dbFreeVec.reduce(_ | _)
  wRespQ.io.enq.valid     := io.wReq.valid & dbFreeVec.reduce(_ | _)
  wRespQ.io.enq.bits.dbid := wReqId
  wRespQ.io.enq.bits.to   := io.wReq.bits.from

  io.wResp                <> wRespQ.io.deq


// ---------------------------------------------------------------------------------------------------------------------- //
// ----------------------------------------------------- DATA TO DB ----------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  when(io.dataTDB.valid){ entrys(io.dataTDB.bits.dbid).beats(toBeatNum(io.dataTDB.bits.dataID)) := io.dataTDB.bits.data }
  io.dataTDB.ready := true.B



// ---------------------------------------------------------------------------------------------------------------------- //
// ----------------------------------------------------- State Transfer ------------------------------------------------- //
// ---------------------------------------------------------------------------------------------------------------------- //
  entrys.map(_.state).zipWithIndex.foreach {
    case (s, i) =>
      switch(s) {
        is(DBState.FREE) {
          val hit = wRespQ.io.enq.fire & wReqId === i.U
          s := Mux(hit, DBState.ALLOC, s)
        }
      }
  }


// ----------------------------------------------------- Assertion ---------------------------------------------------------- //
  when(io.dataTDB.valid){ assert(entrys(io.dataTDB.bits.dbid).isAlloc) }


}
















