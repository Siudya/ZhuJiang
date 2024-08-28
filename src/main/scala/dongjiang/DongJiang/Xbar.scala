//package DONGJIANG
//
//import Utils.FastArb
//import chisel3.{Flipped, _}
//import chisel3.util._
//import org.chipsalliance.cde.config._
//import Utils.IDConnector._
//import Utils.FastArb._
//import xs.utils.FastArbiter
//
//// TODO: has some problem of XBar, req never enter slice_1
//
//class IdMap(implicit p: Parameters) extends DJModule {
//    val io = IO(new Bundle {
//        val bankVal = Input(Vec(djparam.nrBank, Bool()))
//        val inBank  = Input(UInt(bankBits.W))
//        val outBank = Output(UInt(bankBits.W))
//    })
//    val outBank = WireInit(0.U(bankBits.W))
//    val bankaValNum = WireInit(PopCount(io.bankVal).asUInt)
//
//    if (djparam.nrBank == 4) {
//        switch(bankaValNum) {
//            // Use Bank [0]
//            is(1.U) { outBank :=  0.U }
//            // Use Bank [0 1]
//            is(2.U) { outBank === io.inBank(0) }
//            // Use Bank [0 1 2 3]
//            is(4.U) { outBank === io.inBank }
//        }
//    } else if (djparam.nrBank == 2) {
//        switch(bankaValNum) {
//            // Use Bank [0]
//            is(1.U) { outBank === 0.U }
//            // Use Bank [0 1]
//            is(2.U) { outBank === io.inBank(0) }
//        }
//    } else {
//        // Use Bank [0]
//        outBank === 0.U
//    }
//    io.outBank := outBank
//    assert(bankaValNum === 1.U | bankaValNum === 2.U | bankaValNum === 4.U)
//}
//
//
//
//
//class RN2SliceXbar()(implicit p: Parameters) extends DJModule {
//// ------------------------------------------ IO declaration ----------------------------------------------//
//    val io = IO(new Bundle {
//        val bankVal = Input(Vec(djparam.nrBank, Bool()))
//        // slice ctrl signals
//        val req2Slice = new Bundle {
//            val in = Vec(nrRnNode, Flipped(Decoupled(new Req2SliceBundle())))
//            val out = Vec(djparam.nrBank, Decoupled(new Req2SliceBundle()))
//        }
//        val resp2Node = new Bundle {
//            val in = Vec(djparam.nrBank, Flipped(Decoupled(new Resp2NodeBundle())))
//            val out = Vec(nrRnNode, Decoupled(new Resp2NodeBundle()))
//        }
//        val req2Node = new Bundle {
//            val in = Vec(djparam.nrBank, Flipped(Decoupled(new Req2NodeBundle())))
//            val out = Vec(nrRnNode, Decoupled(new Req2NodeBundle()))
//        }
//        val resp2Slice = new Bundle {
//            val in = Vec(nrRnNode, Flipped(Decoupled(new Resp2SliceBundle())))
//            val out = Vec(djparam.nrBank, Decoupled(new Resp2SliceBundle()))
//        }
//        // slice DataBuffer signals
//        val dbSigs = new Bundle {
//            val in = Vec(nrRnNode, Flipped(new DBBundle(hasDBRCReq = true)))
//            val out = Vec(djparam.nrBank, new DBBundle(hasDBRCReq = true))
//        }
//    })
//
//    io <> DontCare
//
//// ------------------------------------------ Modules declaration And Connection ----------------------------------------------//
//    val req2SliceIdMaps     = Seq.fill(nrRnNode) { Module(new IdMap()) }
//    val wReq2SliceIdMaps    = Seq.fill(nrRnNode) { Module(new IdMap()) }
//
//    // --------------------- Wire declaration ------------------------//
//    val req2SliceReMap      = Wire(Vec(nrRnNode, Decoupled(new Req2SliceBundle())))
//    val wReqRemap           = Wire(Vec(nrRnNode, Decoupled(new DBWReq())))
//
//
//    // --------------------- Connection ------------------------//
//    // req2Slice bank ReMap
//    req2SliceReMap.zip(io.req2Slice.in).foreach{ case(reMap, in) => reMap <> in }
//    req2SliceIdMaps.zipWithIndex.foreach {
//        case(m, i) =>
//            m.io.bankVal <> io.bankVal
//            m.io.inBank := io.req2Slice.in(i).bits.to.idL1
//            req2SliceReMap(i).bits.to.idL1 := m.io.outBank
//    }
//
//    // wReqRemap bank reMap
//    wReqRemap.zip(io.dbSigs.in.map(_.wReq)).foreach { case(reMap, in) => reMap <> in }
//    wReq2SliceIdMaps.zipWithIndex.foreach {
//        case (m, i) =>
//            m.io.bankVal <> io.bankVal
//            m.io.inBank := io.dbSigs.in(i).wReq.bits.to.idL1
//            wReqRemap(i).bits.to.idL1 :=  m.io.outBank
//    }
//
//    // in ---> [queue0] ---> [idIndex] ---> [queue1] ---> [arbiter] ---> [queue2] ---> out
//    def interConnect[T <: Bundle with HasToIDBits](in: Seq[DecoupledIO[T]], q0: Int, q1: Int, q2: Int, out: Seq[DecoupledIO[T]]): Unit = {
//        val reDir = Seq.fill(in.size) { Seq.fill(out.size) { WireInit(0.U.asTypeOf(in(0))) }}
//        in.zipWithIndex.foreach { case (m, i) => idSelDec2DecVec(Queue(m, q0), reDir(i), level = 1) }
//        out.zipWithIndex.foreach { case (m, i) => m <> Queue(fastArbDec(reDir.map { case a => Queue(a(i), entries = q1) }), q2) }
//    }
//
//
//    interConnect(in = req2SliceReMap,                       q0 = 0, q1 = 0, q2 = 0, out = io.req2Slice.out)
//
//    interConnect(in = io.resp2Node.in,                      q0 = 0, q1 = 0, q2 = 0, out = io.resp2Node.out)
//
//    interConnect(in = io.req2Node.in,                       q0 = 0, q1 = 0, q2 = 0, out = io.req2Node.out)
//
//    interConnect(in = io.resp2Slice.in,                     q0 = 0, q1 = 0, q2 = 0, out = io.resp2Slice.out)
//
//    interConnect(in = io.dbSigs.in.map(_.dbRCReqOpt.get),   q0 = 0, q1 = 0, q2 = 0, out = io.dbSigs.out.map(_.dbRCReqOpt.get))
//
//    interConnect(in = wReqRemap,                            q0 = 0, q1 = 0, q2 = 0, out = io.dbSigs.out.map(_.wReq))
//
//    interConnect(in = io.dbSigs.out.map(_.wResp),           q0 = 0, q1 = 0, q2 = 0, out = io.dbSigs.in.map(_.wResp))
//
//    interConnect(in = io.dbSigs.out.map(_.dataFDB),         q0 = 0, q1 = 0, q2 = 0, out = io.dbSigs.in.map(_.dataFDB))
//
//    interConnect(in = io.dbSigs.in.map(_.dataTDB),          q0 = 0, q1 = 0, q2 = 0, out = io.dbSigs.out.map(_.dataTDB))
//
//}