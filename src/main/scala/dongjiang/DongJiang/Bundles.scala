package DONGJIANG

import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import scala.collection.immutable.ListMap
import scala.math.{max, min}


// ---------------------------------------------------------------- Xbar Id Bundle ----------------------------------------------------------------------------- //

object IdL0 {
    val width      = 2
    val SLICE      = "b00".U
    val RNSLV      = "b01".U
    val RNMAS      = "b10".U
    val SNMAS      = "b11".U
}

class IDBundle(implicit p: Parameters) extends DJBundle {
    val idL0 = UInt(IdL0.width.W)
    val idL1 = UInt(max(bankBits, rnslvIdBits).W)
    val idL2 = UInt(max(max(reqBufIdBits, mshrWayBits), nodeIdBits).W)

    def mshrWay  = idL2
    def reqBufId = idL2
    def nodeId   = idL2

    def isSLICE  = idL0 === IdL0.SLICE
    def isRNSLV  = idL0 === IdL0.RNSLV
    def isRNMAS  = idL0 === IdL0.RNMAS
    def isSNMAS  = idL0 === IdL0.SNMAS
}

trait HasFromIDBits extends DJBundle { this: Bundle => val from = new IDBundle() }

trait HasToIDBits extends DJBundle { this: Bundle => val to = new IDBundle() }

class ToIDBundle(implicit p: Parameters) extends DJBundle with HasToIDBits

trait HasIDBits extends DJBundle with HasFromIDBits with HasToIDBits

trait HasDBID extends DJBundle { this: Bundle => val dbid = UInt(dbIdBits.W) }

trait HasAddr extends DJBundle { this: Bundle => val addr = UInt(addressBits.W) }

trait HasMSHRSet extends DJBundle { this: Bundle => val mshrSet = UInt(mshrSetBits.W) }

trait HasMSHRWay extends DJBundle { this: Bundle => val mshrWay = UInt(mshrWayBits.W) }

class MSHRIndexBundle(implicit p: Parameters) extends DJBundle with HasMSHRSet with HasMSHRWay

// ---------------------------------------------------------------- Req To Slice Bundle ----------------------------------------------------------------------------- //
trait HasReqBaseMesBundle extends DJBundle { this: Bundle =>
    // CHI Id(Use in RnSlave)
    val srcID       = UInt(chiParams.nodeIdBits.W)
    val txnID       = UInt(chiParams.nodeIdBits.W)
    // Snp Mes(Use in RnMaster)
    val isSnp       = Bool()
    val doNotGoToSD = Bool()
    val retToSrc    = Bool()
    // CHI Mes(Common)
    val opcode      = UInt(6.W)
    // Other(Common)
    val willSnp     = Bool()
}

class ReqBaseMesBundle(implicit p: Parameters) extends DJBundle with HasReqBaseMesBundle with HasFromIDBits

trait HasReq2SliceBundle extends DJBundle with HasReqBaseMesBundle with HasAddr

class Req2SliceBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasReq2SliceBundle

class Req2SliceBundle(implicit p: Parameters) extends DJBundle with HasReq2SliceBundle with HasIDBits


// ---------------------------------------------------------------- Resp To Node Bundle ----------------------------------------------------------------------------- //

trait HasResp2NodeBundle extends DJBundle with HasCHIChannel { this: Bundle =>
    // CHI Id
    val srcID       = UInt(chiParams.nodeIdBits.W)
    val txnID       = UInt(chiParams.nodeIdBits.W)
    // CHI Mes
    val opcode      = UInt(6.W)
    // Indicate Snoopee final state
    val resp        = UInt(ChiResp.width.W)
    // Indicate Requster final state in DCT
    val fwdState    = UInt(ChiResp.width.W)
    // Let ReqBuf Req Send To Slice Retry
    val reqRetry    = Bool()
}

class Resp2NodeBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasResp2NodeBundle

class Resp2NodeBundle(implicit p: Parameters) extends DJBundle with HasResp2NodeBundle with HasIDBits


// ---------------------------------------------------------------- Req To Node Bundle ----------------------------------------------------------------------------- //
trait HasReq2NodeBundle extends DJBundle with HasAddr { this: Bundle =>
    // CHI Id
    val tgtId       = UInt(chiParams.nodeIdBits.W)
    val srcId       = UInt(chiParams.nodeIdBits.W)
    val txnId       = UInt(chiParams.nodeIdBits.W)
    // Snp Mes (Use in RnSlave)
    val retToSrc    = Bool()
    val doNotGoToSD = Bool()
    // CHI Mes (Common)
    val opcode      = UInt(6.W)
    // CHI Mes (Use in RnMaster)
    val resp        = UInt(ChiResp.width.W) // Use in write back
    val expCompAck  = Bool()
    val tgtID       = UInt(chiParams.nodeIdBits.W)
}

class Req2NodeBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasReq2NodeBundle

class Req2NodeBundle(implicit p: Parameters) extends DJBundle with HasReq2NodeBundle with HasIDBits


// ---------------------------------------------------------------- Resp To Slice Bundle ----------------------------------------------------------------------------- //
trait HasResp2SliceBundle extends DJBundle with HasDBID with HasMSHRSet { this: Bundle =>
    val isSnpResp   = Bool()
    val hasData     = Bool()
    // Indicate Snoopee final state
    val resp        = UInt(ChiResp.width.W)
    // Indicate Requster final state in DCT
    val fwdState    = Valid(UInt(ChiResp.width.W))
}

class Resp2SliceBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasResp2SliceBundle

class Resp2SliceBundle(implicit p: Parameters) extends DJBundle with HasResp2SliceBundle with HasIDBits


// ---------------------------------------------------------------- DataBuffer Base Bundle ----------------------------------------------------------------------------- //
trait HasDBRCOp extends DJBundle { this: Bundle =>
    val isRead = Bool()
    val isClean = Bool()
}
// Base Data Bundle
trait HasDBData extends DJBundle { this: Bundle =>
    val data = UInt(beatBits.W)
    val dataID = UInt(2.W)
    def beatNum: UInt = {
        if (nrBeat == 1) { 0.U }
        else if (nrBeat == 2) { Mux(dataID === 0.U, 0.U, 1.U) }
        else { dataID }
    }
    def isLast: Bool = beatNum === (nrBeat - 1).U
}

// DataBuffer Read/Clean Req
class DBRCReq(implicit p: Parameters)       extends DJBundle with HasDBRCOp with HasMSHRSet with HasDBID with HasIDBits { val useDBID = Bool()}
class DBWReq(implicit p: Parameters)        extends DJBundle                                             with HasIDBits
class DBWResp(implicit p: Parameters)       extends DJBundle                                with HasDBID with HasIDBits
class NodeFDBData(implicit p: Parameters)   extends DJBundle with HasDBData                              with HasToIDBits
class NodeTDBData(implicit p: Parameters)   extends DJBundle with HasDBData                 with HasDBID with HasToIDBits

class DBBundle(hasDBRCReq: Boolean = false)(implicit p: Parameters) extends DJBundle {
    val dbRCReqOpt  = if(hasDBRCReq) Some(Decoupled(new DBRCReq)) else None
    val wReq        = Decoupled(new DBWReq)
    val wResp       = Flipped(Decoupled(new DBWResp))
    val dataFDB     = Flipped(Decoupled(new NodeFDBData))
    val dataTDB     = Decoupled(new NodeTDBData)

    def dbRCReq     = dbRCReqOpt.get
}


// ---------------------------------------------------------------- DataBuffer Base Bundle ----------------------------------------------------------------------------- //
class MpTaskBundle(implicit p: Parameters) extends DJBundle with HasAddr {
    // TODO
}

class UpdateMSHRBundle(implicit p: Parameters) extends DJBundle

class SnpTaskBundle(implicit p: Parameters) extends DJBundle

class DSTaskBundle(implicit p: Parameters) extends DJBundle

class DirReadBundle(implicit p: Parameters) extends DJBundle with HasAddr with HasMSHRWay

class DirRespBaseBundle(nrWays: Int, nrMetas: Int, replWayBits: Int)(implicit p: Parameters) extends DJBundle with HasAddr {
    val hit         = Bool()
    val wayOH       = UInt(nrWays.W)
    val metaVec     = Vec(nrMetas, new CHIStateBundle())
    val replMes     = UInt(replWayBits.W)
    val replRetry   = Bool()
}

class DirRespBundle(implicit p: Parameters) extends DJBundle {
    val s   = new DirRespBaseBundle(djparam.selfWays, 1, sReplWayBits) // self
    val sf  = new DirRespBaseBundle(djparam.sfDirWays, nrRnSlv, sfReplWayBits) // snoop filter
}

class DirWriteBaseBundle(nrWays: Int, nrMetas: Int, replWayBits: Int)(implicit p: Parameters) extends DJBundle with HasAddr {
    val wayOH       = UInt(nrWays.W)
    val metaVec     = Vec(nrMetas, new CHIStateBundle())
    val replMes     = UInt(replWayBits.W)
}

class DirWriteBundle(implicit p: Parameters) extends DJBundle {
    val s   = Decoupled(new DirWriteBaseBundle(djparam.selfWays, 1, sReplWayBits)) // self
    val sf  = Decoupled(new DirWriteBaseBundle(djparam.sfDirWays, nrRnSlv, sfReplWayBits)) // snoop filter
}






