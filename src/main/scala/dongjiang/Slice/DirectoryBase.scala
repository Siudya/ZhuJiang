package DONGJIANG.SLICE

import DONGJIANG._
import DONGJIANG.CHI._
import chisel3.{util, _}
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.sram.SRAMTemplate
import chisel3.util.random.LFSR
import freechips.rocketchip.util.ReplacementPolicy
import Utils.SramWrapper

class DirEntry(nrMetas: Int = 1)(implicit p: Parameters) extends DJBundle {
  val tag         = UInt(sTagBits.W)
  val bank        = UInt(bankBits.W)
  val metaVec     = Vec(nrMetas, new CHIStateBundle())
}

class DirEntryWithRepl(ways: Int, nrMetas: Int = 1, replWayBits: Int)(implicit p: Parameters) extends DJBundle {
  val metas       = Vec(ways, new DirEntry(nrMetas))
  val replMes     = UInt(replWayBits.W)
}

class DirectoryBase(
                      tagBits: Int,
                      modBankBits: Int,
                      sets: Int,
                      ways: Int = 4,
                      nrMetas: Int = 1,
                      replPolicy: String = "plru",
                      mcp: Int = 1,
                      holdMcp: Boolean = false,
                      nrSramBank: Int = 1)
  (implicit p: Parameters) extends DJModule {

  val repl        = ReplacementPolicy.fromString(replPolicy, ways)
  val useRepl     = replPolicy != "random"
  val replWayBits = if(useRepl) repl.nBits else 0
  val setBits     = log2Ceil(sets)
  val wayBits     = log2Ceil(ways)
  val holdMcpVal  = holdMcp match { case true => 1; case false => 0 }

  def parseDirAddress(x: UInt): (UInt, UInt, UInt, UInt, UInt) = parseAddress(x, modBankBits, setBits, tagBits)

// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val id        = Input(UInt(modBankBits.W))
    val dirRead   = Flipped(Decoupled(new DirReadBundle))
    val dirWrite  = Flipped(Decoupled(new DirWriteBaseBundle(ways, nrMetas, replWayBits)))
    val dirResp   = Decoupled(new DirRespBaseBundle(ways, nrMetas, replWayBits))
    val readMshr  = Valid(UInt(mshrSetBits.W))
    val mshrResp  = Input(Vec(djparam.nrMSHRWays, Valid(UInt(addressBits.W))))
  })

// --------------------- Modules declaration ------------------------//
  val metaArray     = Module(new SramWrapper(new DirEntry, sets, ways / nrSramBank, singlePort = true, shouldReset = true, multicycle = mcp, holdMcp = holdMcp))

  val replArrayOpt  = if(!useRepl) None else Some(Module(new SRAMTemplate(UInt(repl.nBits.W), sets, way = 1, singlePort = true, shouldReset = true)))

  val readPipe      = Module(new Pipe(new MSHRIndexBundle(), latency = mcp - 1 + holdMcpVal))

  val replPipeOpt   = if(!useRepl) None else Some(Module(new Pipe(UInt(repl.nBits.W), latency = mcp - 1 + holdMcpVal)))

  metaArray.io <> DontCare
  replArrayOpt.get.io <> DontCare
  readPipe.io <> DontCare
  replPipeOpt.get.io <> DontCare



//// ----------------------- Reg/Wire declaration --------------------------//
  // s2
  val valid_s2        = WireInit(false.B)
  val metaResp_s2     = Wire(Vec(ways, new DirEntry()))
  val replResp_s2     = WireInit(0.U(repl.nBits.W))
  val mshrIndex_s2    = WireInit(0.U.asTypeOf(new MSHRIndexBundle()))
  val mshrMes_s2      = Wire(Vec(djparam.nrMSHRWays, Valid(new Bundle {
    val tag           = UInt(tagBits.W)
    val bank          = UInt(bankBits.W)
  })))
  // s3
  val valid_s3_g      = RegInit(false.B)
  val metaResp_s3_g   = Reg(Vec(ways, new DirEntry()))
  val mshrIndex_s3_g  = RegInit(0.U.asTypeOf(new MSHRIndexBundle()))
  val mshrMes_s3_g    = Reg(Vec(djparam.nrMSHRWays, Valid(new Bundle{
    val tag           = UInt(tagBits.W)
    val bank          = UInt(bankBits.W)
  })))
  val replResp_s3_g   = RegInit(0.U(repl.nBits.W))
  val selInvWayVec    = Wire(Vec(ways, Bool()))
  val hitWayVec       = Wire(Vec(ways, Bool()))
  val replWay         = WireInit(0.U(wayBits.W))
  val tag_s3          = WireInit(0.U(tagBits.W))
  val set_s3          = WireInit(0.U(setBits.W))
  val bank_s3         = WireInit(0.U(bankBits.W))
  val unUseWayVec     = Wire(Vec(ways, Bool()))


// ------------------------------ S1: Read / Write SRAM -----------------------------------//
  /*
   * Read / Write Req SRAM
   */
  metaArray.io.req.valid      := io.dirRead.valid | io.dirWrite.valid
  metaArray.io.req.bits.index := parseDirAddress(Mux(io.dirWrite.valid, io.dirWrite.bits.addr, io.dirRead.bits.addr))._4
  metaArray.io.req.bits.wmode := io.dirWrite.valid
  metaArray.io.req.bits.wayMaskOpt.get := io.dirWrite.bits.wayOH

  io.dirRead.ready            := !io.dirWrite.valid & metaArray.io.req.ready
  io.dirWrite.ready           := metaArray.io.req.ready

  /*
   * Write Data To SRAM
   */
  metaArray.io.data.valid     := io.dirWrite.valid
  metaArray.io.data.bits.tag  := parseDirAddress(io.dirWrite.bits.addr)._5
  metaArray.io.data.bits.bank := parseDirAddress(io.dirWrite.bits.addr)._2
  metaArray.io.data.bits.metaVec := io.dirWrite.bits.metaVec

  /*
   * Read Repl SRAM
   */
  if(useRepl) {
    replArrayOpt.get.io.r.req.valid       := io.dirRead.fire
    replArrayOpt.get.io.r.req.bits.setIdx := parseDirAddress(io.dirRead.bits.addr)._4
  }

  /*
   * enter pipe
   */
  readPipe.io.enq.valid := io.dirRead.fire
  readPipe.io.enq.bits  := io.dirRead.bits


// ------------------------------ S2: Wait SRAM Resp and Read MSHR -----------------------------------//
  /*
   * Receive SRAM resp
   */
  valid_s2        := metaArray.io.resp.valid
  metaResp_s2     := metaArray.io.resp.bits

  /*
   * Receive sRepl resp
   */
  if (useRepl) {
    replPipeOpt.get.io.enq.valid  := RegNext(replArrayOpt.get.io.r.req.fire)
    replPipeOpt.get.io.enq.bits   := replArrayOpt.get.io.r.resp.data(0)
  }

  /*
   * Receive sRepl pipe deq
   */
  if (useRepl) {
    replResp_s2     := replPipeOpt.get.io.deq.bits
  }

  /*
   * Receive pipe deq
   */
  mshrIndex_s2      := readPipe.io.deq.bits

  /*
   * Read MSHR Set Mes
   */
  io.readMshr.valid := valid_s2
  io.readMshr.bits := mshrIndex_s2.mshrSet
  mshrMes_s2.zip(io.mshrResp).foreach {
    case (a, b) =>
      a.valid       := b.valid
      a.bits.tag    := parseDirAddress(b.bits)._5
      a.bits.bank   := parseDirAddress(b.bits)._2
  }



// ------------------------------ S3: Output DirResp -----------------------------------//
  /*
   * Receive S2
   */
  valid_s3_g      := valid_s2
  metaResp_s3_g   := metaResp_s2
  mshrIndex_s3_g  := mshrIndex_s2
  mshrMes_s3_g.zip(mshrMes_s2).foreach { case(a, b) => a := b }
  replResp_s3_g   := replResp_s3_g

  tag_s3          := mshrMes_s3_g(mshrIndex_s3_g.mshrWay).bits.tag
  set_s3          := mshrIndex_s3_g.mshrSet
  bank_s3         := mshrMes_s3_g(mshrIndex_s3_g.mshrWay).bits.bank
  val addr_s3     = Cat(tag_s3, set_s3, io.id, bank_s3)


  /*
   * Get Hit Vec and Hit State
   */
  val tagHitVec   = metaResp_s3_g.map(_.tag === tag_s3)
  val bankHitVec  = metaResp_s3_g.map(_.bank === bank_s3)
  val stateHitVec = metaResp_s3_g.map(_.metaVec.map(!_.isInvalid).reduce(_ | _))
  val hitMetaVec  = metaResp_s3_g(OHToUInt(hitWayVec)).metaVec
  val hit         = hitWayVec.asUInt.orR
  hitWayVec       := tagHitVec.zip(bankHitVec.zip(stateHitVec)).map{ case(t, (b, s)) => t & b & s }


  /*
   * Selet one invalid way
   */
  val invWayVec     = stateHitVec.map(!_)
  val hasInvWay     = invWayVec.reduce(_ | _)
  selInvWayVec      := PriorityEncoderOH(invWayVec)


  /*
   * Select one replace way
   */
  if (!useRepl) {
   replWay := LFSR(sWayBits) // random
  } else {
   replWay := repl.get_replace_way(replResp_s3_g) // replace
  }


  /*
   * repl way is conflict with unuse way
   */
  unUseWayVec         := metaResp_s3_g.map { case meta => mshrMes_s3_g.map { case mshr => mshr.valid & mshr.bits.tag === meta.tag & mshr.bits.bank === meta.bank }.reduce(_ | _) }
  val replWayIsUsing  = unUseWayVec(replWay)
  val selUnuseWay     = PriorityEncoder(unUseWayVec)
  val replRetry       = unUseWayVec.asUInt.andR


  /*
   * Output Resp
   */
  io.dirResp.valid          := valid_s3_g
  io.dirResp.bits.hit       := hit
  // [Resp Mes]                         [Hit Way Mes]                      [Invalid Way Mes]                        [Unuse Way Mes]                     [Replace Way Mes]
  io.dirResp.bits.wayOH     := Mux(hit, hitWayVec.asUInt,   Mux(hasInvWay, selInvWayVec.asUInt, Mux(replWayIsUsing, UIntToOH(selUnuseWay),              UIntToOH(replWay))))
  io.dirResp.bits.addr      := Mux(hit, addr_s3,            Mux(hasInvWay, 0.U,                 Mux(replWayIsUsing, metaResp_s3_g(selUnuseWay).tag,     metaResp_s3_g(replWay).tag)))
  io.dirResp.bits.metaVec   := Mux(hit, hitMetaVec,         Mux(hasInvWay, 0.U,                 Mux(replWayIsUsing, metaResp_s3_g(selUnuseWay).metaVec, metaResp_s3_g(replWay).metaVec)))
  io.dirResp.bits.replRetry := replRetry
  if(useRepl) { io.dirResp.bits.replMes := replResp_s3_g }



// ------------------------------ Update Replace SRAM Mes -----------------------------------//
  /*
   * PLRU: update replacer only when read hit or write Dir
   */
  if (replPolicy == "plru") {
    replArrayOpt.get.io.w.req.valid               := io.dirWrite.fire | (io.dirResp.fire & hit)
    replArrayOpt.get.io.w.req.bits.setIdx         := parseDirAddress(Mux(io.dirWrite.fire, io.dirWrite.bits.addr, addr_s3))._4
    replArrayOpt.get.io.w.req.bits.data.foreach(_ := Mux(io.dirWrite.fire,
                                                       repl.get_next_state(io.dirWrite.bits.replMes,  OHToUInt(io.dirWrite.bits.wayOH)),
                                                       repl.get_next_state(replResp_s3_g,             OHToUInt(io.dirResp.bits.wayOH))))
  } else if(replPolicy == "random") {
    // nothing to do
  } else {
    assert(false.B, "Dont support replacementPolicy except plru or random")
  }


// ------------------------------ Assertion -----------------------------------//
  // s1
  if(useRepl) {
    assert(!((metaArray.io.req.fire & !metaArray.io.req.bits.wmode) ^ replArrayOpt.get.io.r.req.fire), "Must read meta and repl at the same time in S1")
  }
  // s2
  if (useRepl) {
    assert(!(readPipe.io.deq.valid ^ replPipeOpt.get.io.deq.valid), "Must get meta and repl at the same time in S2")
  }
  // s3
  assert(PopCount(hitWayVec) <= 1.U)
  assert(Mux(io.dirResp.valid, io.dirResp.ready, true.B))
}