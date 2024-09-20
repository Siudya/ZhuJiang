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
// Interconnect ID
object IncoID {
    val width       = 2
    val LOCALSLV    = 0
    val LOCALMAS    = 1
    val CSNSLV      = 2
    val CSNMAS      = 3
}

class IDBundle(implicit p: Parameters) extends DJBundle {
    val IncoId      = UInt(max(bankBits, nrIntfBits).W)

    def intfId      = IncoId
    def bankId      = IncoId

    def LOCALSLV    = IncoId === IncoID.LOCALSLV.U
    def LOCALMAS    = IncoId === IncoID.LOCALMAS.U
    def CSNSLV      = IncoId === IncoID.CSNSLV.U
    def CSNMAS      = IncoId === IncoID.CSNMAS.U
}

trait HasFromIncoID extends DJBundle { this: Bundle => val from = new IDBundle() }

trait HasToIncoID extends DJBundle { this: Bundle => val to = new IDBundle() }

trait HasIncoID extends DJBundle with HasFromIncoID with HasToIncoID

trait HasDBID extends DJBundle { this: Bundle => val dbid = UInt(dbIdBits.W) }

trait HasAddr extends DJBundle {this: Bundle =>
    val addr    = UInt(addressBits.W);
    def mTag    = parseMSHRAddress(addr)._1
    def mSet    = parseMSHRAddress(addr)._2
    def mBank   = parseMSHRAddress(addr)._3
    def dirBank = getDirBank(addr)
}

trait HasMSHRSet extends DJBundle { this: Bundle => val mshrSet = UInt(mshrSetBits.W) }

class MSHRSetBundle(implicit p: Parameters) extends DJBundle with HasMSHRSet

trait HasMSHRWay extends DJBundle { this: Bundle => val mshrWay = UInt(mshrWayBits.W); val useEvict = Bool(); def useMSHR = !useEvict } // UseEvictTable

trait HasMHSRIndex extends DJBundle with HasMSHRSet with HasMSHRWay { def mshrMatch(set: UInt, way: UInt): Bool = mshrSet === set & mshrWay === way }

object PipeID { val width = 1; val RESP = "b0".U; val REQ = "b1".U }

trait HasPipeID extends Bundle { this: Bundle => val pipeId = UInt(PipeID.width.W); def toReqPipe = pipeId === PipeID.REQ; def toRespPipe = pipeId === PipeID.RESP }

trait HasPCUID extends DJBundle { this: Bundle => val pcuId = UInt(pcuIdBits.W) }

// ---------------------------------------------------------------- Req To Slice Bundle ----------------------------------------------------------------------------- //
// TODO: Rename it
trait HasReqBaseMesBundle extends DJBundle { this: Bundle =>
    // CHI Id(Use in RnSlave)
    val srcID       = UInt(djparam.chiNodeIdBits.W)
    val txnID       = UInt(djparam.chiNodeIdBits.W)
    // Snp Mes(Use in RnMaster)
    val isSnp       = Bool()
    val doNotGoToSD = Bool()
    val retToSrc    = Bool()
    // CHI Mes(Common)
    val opcode      = UInt(6.W)
    val expCompAck  = Bool()
}

class ReqBaseMesBundle(implicit p: Parameters) extends DJBundle with HasReqBaseMesBundle with HasFromIncoID

trait HasReq2SliceBundle extends DJBundle with HasReqBaseMesBundle with HasAddr

class Req2SliceBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasReq2SliceBundle

class Req2SliceBundle(implicit p: Parameters) extends DJBundle with HasReq2SliceBundle with HasIncoID with HasPCUID

// -------------------------------------------------------------- Req Ack To Node Bundle ---------------------------------------------------------------------------- //
class ReqAck2NodeBundle(implicit p: Parameters) extends DJBundle with HasToIncoID with HasPCUID { val retry = Bool(); def receive = !retry }

// ---------------------------------------------------------------- Resp To Node Bundle ----------------------------------------------------------------------------- //
trait HasResp2NodeBundle extends DJBundle with HasCHIChannel with HasMSHRWay with HasDBID { this: Bundle =>
    // CHI Id
    val tgtID       = UInt(djparam.chiNodeIdBits.W)
    val srcID       = UInt(djparam.chiNodeIdBits.W)
    val txnID       = UInt(djparam.chiNodeIdBits.W)
    // CHI Mes
    val opcode      = UInt(6.W)
    val expCompAck  = Bool()
    // Indicate state
    val resp        = UInt(ChiResp.width.W)
    // Indicate Requster final state in DCT
    val fwdState    = UInt(ChiResp.width.W)
    // Need Read DataBuffer
    val needReadDB  = Bool()
}

class Resp2NodeBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasResp2NodeBundle

class Resp2NodeBundle(implicit p: Parameters) extends DJBundle with HasResp2NodeBundle with HasIncoID


// ---------------------------------------------------------------- Req To Node Bundle ----------------------------------------------------------------------------- //
trait HasReq2NodeBundle extends DJBundle with HasAddr with HasMSHRWay { this: Bundle =>
    // CHI Id
    val tgtId       = UInt(djparam.chiNodeIdBits.W)
    val srcId       = UInt(djparam.chiNodeIdBits.W)
    val txnId       = UInt(djparam.chiNodeIdBits.W)
    // Snp Mes (Use in RnSlave)
    val retToSrc    = Bool()
    val doNotGoToSD = Bool()
    // CHI Mes (Common)
    val opcode      = UInt(6.W)
    // CHI Mes (Use in RnMaster)
    val resp        = UInt(ChiResp.width.W) // Use in write back
    val expCompAck  = Bool()
    // Replace (Use In SnMaster)
    val replace     = Bool()
    val ReadDCU     = Bool()
}

class Req2NodeBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasReq2NodeBundle

class Req2NodeBundle(implicit p: Parameters) extends DJBundle with HasReq2NodeBundle with HasIncoID


// ---------------------------------------------------------------- Resp To Slice Bundle ----------------------------------------------------------------------------- //
trait HasResp2SliceBundle extends DJBundle with HasDBID with HasMHSRIndex { this: Bundle =>
    val isSnpResp   = Bool()
    val isReqResp   = Bool()
    val hasData     = Bool()
    // Indicate Snoopee final state
    val resp        = UInt(ChiResp.width.W)
    // Indicate Requster final state in DCT
    val fwdState    = Valid(UInt(ChiResp.width.W))

    def isResp      = isSnpResp | isReqResp
    def isUpdate    = !isResp
}

class Resp2SliceBundleWitoutXbarId(implicit p: Parameters) extends DJBundle with HasResp2SliceBundle

class Resp2SliceBundle(implicit p: Parameters) extends DJBundle with HasResp2SliceBundle with HasIncoID


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
class DBRCReq(implicit p: Parameters)       extends DJBundle with HasDBRCOp with HasDBID with HasToIncoID
class DBWReq(implicit p: Parameters)        extends DJBundle                             with HasFromIncoID with HasPCUID
class DBWResp(implicit p: Parameters)       extends DJBundle                with HasDBID with HasToIncoID   with HasPCUID
class NodeFDBData(implicit p: Parameters)   extends DJBundle with HasDBData with HasDBID with HasToIncoID
class NodeTDBData(implicit p: Parameters)   extends DJBundle with HasDBData with HasDBID

class DBBundle(hasDBRCReq: Boolean = false)(implicit p: Parameters) extends DJBundle {
    val dbRCReqOpt  = if(hasDBRCReq) Some(Decoupled(new DBRCReq)) else None
    val wReq        = Decoupled(new DBWReq)
    val wResp       = Flipped(Decoupled(new DBWResp))
    val dataFDB     = Flipped(Decoupled(new NodeFDBData))
    val dataTDB     = Decoupled(new NodeTDBData)

    def dbRCReq     = dbRCReqOpt.get
}




