package DONGJIANG.SLICE

import DONGJIANG._
import _root_.DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.ReplacementPolicy
import xs.utils.ParallelPriorityMux
import xs.utils.sram.SRAMTemplate
import xs.utils.perf.{DebugOptions, DebugOptionsKey}

class DirectoryWrapper()(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
val io = IO(new Bundle {
  val sliceId     = Input(UInt(bankBits.W))

  val dirRead     = Flipped(Decoupled(new DirReadBundle()))
  val dirResp     = Valid(new DirRespBundle())
  val dirWrite    = Flipped(new DirWriteBundle())

  val readMshr    = Valid(UInt(mshrSetBits.W))
  val mshrResp    = Input(Vec(djparam.nrMSHRWays + djparam.nrEvictWays, Valid(UInt(addressBits.W))))
})




// -------------------------- Modules declaration ------------------------//
  val selfs = Seq.fill(djparam.nrDirBank) { Module(new DirectoryBase( tagBits = sTagBits,
                                                                      modBankBits = dirBankBits,
                                                                      sets = djparam.selfSets / djparam.nrDirBank,
                                                                      ways = djparam.selfWays,
                                                                      nrMetas = 1,
                                                                      replPolicy = djparam.selfReplacementPolicy,
                                                                      mcp = djparam.dirMulticycle,
                                                                      holdMcp = djparam.dirHoldMcp)) }


  val sfs   = Seq.fill(djparam.nrDirBank) { Module(new DirectoryBase( tagBits = sfTagBits,
                                                                      modBankBits = dirBankBits,
                                                                      sets = djparam.sfDirSets / djparam.nrDirBank,
                                                                      ways = djparam.sfDirWays,
                                                                      nrMetas = nrRnfNode,
                                                                      replPolicy = djparam.sfReplacementPolicy,
                                                                      mcp = djparam.dirMulticycle,
                                                                      holdMcp = djparam.dirHoldMcp)) }

  val holdMcpVal  = djparam.dirHoldMcp match { case true => 1; case false => 0 }

  val pipe  = Module(new Pipe(new Bundle { val pipeID = UInt(PipeID.width.W); val bankID = UInt(dirBankBits.W) }, djparam.dirMulticycle + holdMcpVal + 1))

  selfs.foreach(_.io <> DontCare)

  sfs.foreach(_.io <> DontCare)

  def getDirBank(x: UInt): UInt = parseAddress(x, dirBankBits, 0, 0)._3

// -------------------------- Reg and Wire declaration ------------------------//
  val rSReadyVec    = Wire(Vec(djparam.nrDirBank, Bool()))
  val rSFReadyVec   = Wire(Vec(djparam.nrDirBank, Bool()))

  val wSReadyVec    = Wire(Vec(djparam.nrDirBank, Bool()))
  val wSFReadyVec   = Wire(Vec(djparam.nrDirBank, Bool()))

  val respVec       = Wire(Vec(djparam.nrDirBank, new DirRespBundle()))

  val mshrReadVec   = Wire(Vec(djparam.nrDirBank, Valid(UInt(mshrSetBits.W))))

// --------------------------------- Connection -------------------------------//
  /*
   * Select Bank
   */
  val rBank   = getDirBank(io.dirRead.bits.addr)
  val wSBank  = getDirBank(io.dirWrite.s.bits.addr)
  val wSFBank = getDirBank(io.dirWrite.sf.bits.addr)


  /*
   * Connect IO READ / WRITE <-> Self Directory READ / WRITE
   */
  selfs.zipWithIndex.foreach {
    case(s, i) =>
      s.io.dirRead.valid    := io.dirRead.valid & rBank === i.U & sfs(i).io.dirRead.ready
      s.io.dirRead.bits     := io.dirRead.bits

      s.io.dirWrite.valid   := io.dirWrite.s.valid & wSBank === i.U
      s.io.dirWrite.bits    := io.dirWrite.s.bits
  }


  /*
   * Connect IO READ / WRITE <-> SF Directory READ / WRITE
   */
  sfs.zipWithIndex.foreach {
    case (sf, i) =>
      sf.io.dirRead.valid   := io.dirRead.valid & rBank === i.U & selfs(i).io.dirRead.ready
      sf.io.dirRead.bits    := io.dirRead.bits

      sf.io.dirWrite.valid  := io.dirWrite.sf.valid & wSFBank === i.U
      sf.io.dirWrite.bits   := io.dirWrite.sf.bits
  }

  /*
   * Set IO READ / WRITE ready
   */
  rSReadyVec            := selfs.map(_.io.dirRead.ready)
  rSFReadyVec           := sfs.map(_.io.dirRead.ready)
  io.dirRead.ready      := rSReadyVec(rBank) & rSFReadyVec(rBank)

  wSReadyVec            := selfs.map(_.io.dirWrite.ready)
  wSFReadyVec           := sfs.map(_.io.dirWrite.ready)
  io.dirWrite.s.ready   := wSReadyVec(wSBank)
  io.dirWrite.sf.ready  := wSReadyVec(wSFBank)


  /*
   * Set IO RESP
   */
  pipe.io.enq.valid       := io.dirRead.fire
  pipe.io.enq.bits.pipeID := io.dirRead.bits.pipeId
  pipe.io.enq.bits.bankID := rBank

  respVec.zip(selfs.map(_.io.dirResp.bits)).foreach { case(v, s) => v.s := s }
  respVec.zip(sfs.map(_.io.dirResp.bits)).foreach { case(v, sf) => v.sf := sf }
  respVec.foreach(_.pipeId := pipe.io.deq.bits.pipeID)
  io.dirResp.valid        := pipe.io.deq.valid
  io.dirResp.bits         := respVec(pipe.io.deq.bits.bankID)

  selfs.map(_.io.dirResp.ready).foreach(_ := true.B)
  sfs.map(_.io.dirResp.ready).foreach(_ := true.B)


  /*
   * Set IO readMshr / mshrResp
   */
  mshrReadVec.zip(selfs.map(_.io.readMshr)).foreach { case(v, r) => v <> r}
//  mshrReadVec.zip(sfs.map(_.io.readMshr)).foreach { case(v, r) => v <> r}
  io.readMshr.valid := mshrReadVec.map(_.valid).reduce(_ | _)
  io.readMshr.bits  := mshrReadVec(PriorityEncoder(mshrReadVec.map(_.valid))).bits


  selfs.foreach(_.io.mshrResp := io.mshrResp)
  sfs.foreach(_.io.mshrResp := io.mshrResp)




// --------------------------------- Assertion -------------------------------//
  assert(PopCount(selfs.map(_.io.dirRead.valid)) <= 1.U, "selfDirs dirRead: only one request can be entered at a time")
  assert(PopCount(sfs.map(_.io.dirRead.valid)) <= 1.U, "sfDirs dirRead: only one request can be entered at a time")
  assert(!(selfs.map(_.io.dirRead.fire).reduce(_ | _) ^ sfs.map(_.io.dirRead.fire).reduce(_ | _)), "selfDirs and sfDirs dirRead must be fire at the same time")

  assert(PopCount(selfs.map(_.io.dirWrite.valid)) <= 1.U, "selfDirs dirWrite: only one request can be entered at a time")
  assert(PopCount(sfs.map(_.io.dirWrite.valid)) <= 1.U, "sfDirs dirWrite: only one request can be entered at a time")

  assert(PopCount(selfs.map(_.io.dirResp.valid)) <= 1.U, "selfDirs dirResp: only one resp can be output at a time")
  assert(PopCount(sfs.map(_.io.dirResp.valid)) <= 1.U, "sfDirs dirResp: only one resp can be output at a time")
  assert(Mux(selfs.map(_.io.dirResp.valid).reduce(_ | _), selfs.map(_.io.dirResp.ready).reduce(_ & _), true.B), "selfDirs dirResp ready must be true when resp valid")
  assert(Mux(sfs.map(_.io.dirResp.valid).reduce(_ | _), sfs.map(_.io.dirResp.ready).reduce(_ & _), true.B), "sfDirs dirResp ready must be true when resp valid")


  val selfReadFire  = Wire(Vec(djparam.nrDirBank, Bool()))
  val sfReadFire    = Wire(Vec(djparam.nrDirBank, Bool()))
  selfReadFire      := selfs.map(_.io.dirRead.fire)
  sfReadFire        := sfs.map(_.io.dirRead.fire)
  assert(Mux(pipe.io.enq.fire, selfReadFire(rBank) & sfReadFire(rBank), true.B))

  val selfRespFire  = Wire(Vec(djparam.nrDirBank, Bool()))
  val sfRespFire    = Wire(Vec(djparam.nrDirBank, Bool()))
  selfRespFire      := selfs.map(_.io.dirResp.fire)
  sfRespFire        := sfs.map(_.io.dirResp.fire)
  assert(Mux(pipe.io.deq.fire, selfRespFire(pipe.io.deq.bits.bankID) & sfRespFire(pipe.io.deq.bits.bankID), true.B))

  assert(PopCount(mshrReadVec.map(_.valid)) <= 1.U)

}