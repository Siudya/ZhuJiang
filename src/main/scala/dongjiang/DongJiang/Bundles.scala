package DONGJIANG

import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import scala.collection.immutable.ListMap
import scala.math.{max, min}

object ChipType {
    val width         = 1
    val Local         = "b0".U
    val CSN           = "b1".U
}

// ---------------------------------------------------------------- Xbar Id Bundle ----------------------------------------------------------------------------- //

object IdL0 {
    val width      = 1
    val SLICE      = 0
    val INTF       = 1
}

object IdL1 {
    val width      = 2
    val LOCALSLV   = 0
    val LOCALMAS   = 1
    val CSNSLV     = 2
    val CSNMAS     = 3
}

class IDBundle(implicit p: Parameters) extends DJBundle {
    val idL0 = UInt(IdL0.width.W)
    val idL1 = UInt(max(bankBits, nrIntfBits).W)
    val idL2 = UInt(reqBufIdBits.W)

    def mshrWay  = idL2
    def reqBufId = idL2

    def isSLICE  = idL0 === IdL0.SLICE.U
    def isINTF   = idL0 === IdL0.INTF.U

    def LOCALSLV = idL1 === IdL1.LOCALSLV.U
    def LOCALMAS = idL1 === IdL1.LOCALMAS.U
    def CSNSLV   = idL1 === IdL1.CSNSLV.U
    def CSNMAS   = idL1 === IdL1.CSNMAS.U
}

trait HasFromIDBits extends DJBundle { this: Bundle => val from = new IDBundle() }

trait HasToIDBits extends DJBundle { this: Bundle => val to = new IDBundle() }

class ToIDBundle(implicit p: Parameters) extends DJBundle with HasToIDBits

trait HasIDBits extends DJBundle with HasFromIDBits with HasToIDBits

trait HasDBID extends DJBundle { this: Bundle => val dbid = UInt(dbIdBits.W) }

trait HasAddr extends DJBundle {this: Bundle =>
    val addr    = UInt(addressBits.W);
    def mTag    = parseMSHRAddress(addr)._1
    def mSet    = parseMSHRAddress(addr)._2
    def mBank   = parseMSHRAddress(addr)._3
}

trait HasMSHRSet extends DJBundle { this: Bundle => val mshrSet = UInt(mshrSetBits.W) }

class MSHRSetBundle(implicit p: Parameters) extends DJBundle with HasMSHRSet

trait HasMSHRWay extends DJBundle { this: Bundle => val mshrWay = UInt(mshrWayBits.W); val useEvict = Bool(); def useMSHR = !useEvict } // UseEvictTable

trait HasMHSRIndex extends DJBundle with HasMSHRSet with HasMSHRWay { def mshrMatch(set: UInt, way: UInt): Bool = mshrSet === set & mshrWay === way }

class MSHRIndexBundle(implicit p: Parameters) extends DJBundle with HasMSHRSet with HasMSHRWay

object PipeID { val width = 1; val REQ = "b0".U; val RESP = "b1".U }

trait HasPipeID extends Bundle { this: Bundle => val pipeId = UInt(PipeID.width.W); def toReqPipe = pipeId === PipeID.REQ; def toRespPipe = pipeId === PipeID.RESP }

// ---------------------------------------------------------------- Req To Slice Bundle ----------------------------------------------------------------------------- //
// TODO: Rename it
trait HasReqBaseMesBundle extends DJBundle { this: Bundle =>
    // CHI Id(Use in RnSlave)
    val srcID       = UInt(djparam.nodeIdBits.W)
    val txnID       = UInt(djparam.nodeIdBits.W)
    // Snp Mes(Use in RnMaster)
    val isSnp       = Bool()
    val doNotGoToSD = Bool()
    val retToSrc    = Bool()
    // CHI Mes(Common)
    val opcode      = UInt(6.W)
}

class ReqBaseMesBundle(implicit p: Parameters) extends DJBundle with HasReqBaseMesBundle with HasFromIDBits

trait HasReq2SliceBundle extends DJBundle with HasReqBaseMesBundle with HasAddr

class Req2SliceBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasReq2SliceBundle

class Req2SliceBundle(implicit p: Parameters) extends DJBundle with HasReq2SliceBundle with HasIDBits


// ---------------------------------------------------------------- Resp To Node Bundle ----------------------------------------------------------------------------- //

trait HasResp2NodeBundle extends DJBundle with HasCHIChannel with HasMSHRWay { this: Bundle =>
    // CHI Id
    val srcID       = UInt(djparam.nodeIdBits.W)
    val txnID       = UInt(djparam.nodeIdBits.W)
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
trait HasReq2NodeBundle extends DJBundle with HasAddr with HasMSHRWay { this: Bundle =>
    // CHI Id
    val tgtId       = UInt(djparam.nodeIdBits.W)
    val srcId       = UInt(djparam.nodeIdBits.W)
    val txnId       = UInt(djparam.nodeIdBits.W)
    // Snp Mes (Use in RnSlave)
    val retToSrc    = Bool()
    val doNotGoToSD = Bool()
    // CHI Mes (Common)
    val opcode      = UInt(6.W)
    // CHI Mes (Use in RnMaster)
    val resp        = UInt(ChiResp.width.W) // Use in write back
    val expCompAck  = Bool()
}

class Req2NodeBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasReq2NodeBundle

class Req2NodeBundle(implicit p: Parameters) extends DJBundle with HasReq2NodeBundle with HasIDBits


// ---------------------------------------------------------------- Resp To Slice Bundle ----------------------------------------------------------------------------- //
trait HasResp2SliceBundle extends DJBundle with HasDBID with HasMHSRIndex { this: Bundle =>
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
class PipeTaskBundle(implicit p: Parameters) extends DJBundle with HasAddr with HasPipeID with HasMSHRWay {
    val readDir         = Bool()
    val reqMes          = new ReqBaseMesBundle()
    val respMes         = new Bundle {
        val slvResp     = Valid(UInt(ChiResp.width.W))
        val masResp     = Valid(UInt(ChiResp.width.W))
        val fwdState    = Valid(UInt(ChiResp.width.W))
        val slvDBID     = Valid(UInt(dbIdBits.W))
        val masDBID     = Valid(UInt(dbIdBits.W))
    }
}

object UpdMSHRType { val width = 2; val RETRY = "b00".U ; val UPD = "b01".U; val REPL = "b10".U; val EVICT = "b11".U }

trait HasMSHRUpdBundle extends Bundle {
    val updType     = UInt(UpdMSHRType.width.W)

    def isRetry     = updType === UpdMSHRType.RETRY
    def isUpdate    = updType === UpdMSHRType.UPD
    def isRepl      = updType === UpdMSHRType.REPL
    def isEvict     = updType === UpdMSHRType.EVICT
    def isReq       = isRepl | isEvict
}

class UpdateMSHRReqBundle(implicit p: Parameters) extends DJBundle with HasAddr with HasPipeID with HasMSHRWay with HasMSHRUpdBundle {
    val willUseWay      = UInt(2.W)
    val waitSlvVec      = Vec(nrSlvIntf, Bool()) // Wait Snoop Resp
    val waitMasVec      = Vec(nrMasIntf, Bool()) // Wait Req Resp
}

class UpdateMSHRRespBundle(implicit p: Parameters) extends DJBundle with HasPipeID with HasMSHRWay with HasMSHRUpdBundle {
    val retry           = Bool()
}

class DirReadBundle(implicit p: Parameters) extends DJBundle with HasAddr with HasPipeID

class DirRespBaseBundle(nrWays: Int, nrMetas: Int, replWayBits: Int)(implicit p: Parameters) extends DJBundle with HasAddr {
    val hit         = Bool()
    val wayOH       = UInt(nrWays.W)
    val metaVec     = Vec(nrMetas, new CHIStateBundle())
    val replMes     = UInt(replWayBits.W)
    val replRetry   = Bool()
}

class DirRespBundle(implicit p: Parameters) extends DJBundle with HasPipeID {
    val s   = new DirRespBaseBundle(djparam.selfWays, 1, sReplWayBits) // self
    val sf  = new DirRespBaseBundle(djparam.sfDirWays, nrRnfNode, sfReplWayBits) // snoop filter
}

class DirWriteBaseBundle(nrWays: Int, nrMetas: Int, replWayBits: Int)(implicit p: Parameters) extends DJBundle with HasAddr {
    val wayOH       = UInt(nrWays.W)
    val metaVec     = Vec(nrMetas, new CHIStateBundle())
    val replMes     = UInt(replWayBits.W)
}

class DirWriteBundle(implicit p: Parameters) extends DJBundle {
    val s   = Decoupled(new DirWriteBaseBundle(djparam.selfWays, 1, sReplWayBits)) // self
    val sf  = Decoupled(new DirWriteBaseBundle(djparam.sfDirWays, nrRnfNode, sfReplWayBits)) // snoop filter
}






