package DONGJIANG.CPU

import DONGJIANG._
import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import scala.collection.immutable.ListMap
import scala.math.{max, min}


// ---------------------------------------------------------------- Slice Base Bundle ----------------------------------------------------------------------------- //
class RespMesBundle(implicit p: Parameters) extends DJBundle {
    val slvResp         = Valid(UInt(ChiResp.width.W))
    val masResp         = Valid(UInt(ChiResp.width.W))
    val fwdState        = Valid(UInt(ChiResp.width.W))
    val slvDBID         = Valid(UInt(dbIdBits.W))
    val masDBID         = Valid(UInt(dbIdBits.W))
}

class PipeTaskBundle(implicit p: Parameters) extends DJBundle with HasAddr with HasPipeID with HasMSHRWay {
    val readDir         = Bool()
    val reqMes          = new MSHRCHIMesBundle()
    val respMes         = new RespMesBundle()
}

object UpdMSHRType { val width = 2; val RETRY = "b00".U ; val UPD = "b01".U; val REPL = "b10".U; val SNPEVICT = "b11".U }

class UpdateMSHRReqBundle(implicit p: Parameters) extends DJBundle with HasAddr with HasMSHRWay {
    val updType     = UInt(UpdMSHRType.width.W)
    val waitIntfVec = Vec(nrIntf, Bool())

    def isRetry     = updType === UpdMSHRType.RETRY
    def isUpdate    = updType === UpdMSHRType.UPD & waitIntfVec.reduce(_ | _)
    def isClean     = updType === UpdMSHRType.UPD & !waitIntfVec.reduce(_ | _)
    def isRepl      = updType === UpdMSHRType.REPL
    def isSnpEvict  = updType === UpdMSHRType.SNPEVICT

    def isReq       = isRepl | isSnpEvict
}

class DirReadBundle(implicit p: Parameters) extends DJBundle with HasAddr with HasMSHRWay with HasPipeID

class DirRespBaseBundle(nrWays: Int, nrMetas: Int, replWayBits: Int)(implicit p: Parameters) extends DJBundle with HasAddr with HasPipeID {
    val hit         = Bool()
    val wayOH       = UInt(nrWays.W)
    val metaVec     = Vec(nrMetas, new CHIStateBundle())
    val replMes     = UInt(replWayBits.W)
    val replRetry   = Bool()
}

class DirRespBundle(implicit p: Parameters) extends DJBundle with HasPipeID {
    val s           = new DirRespBaseBundle(djparam.selfWays, 1, sReplWayBits) // self
    val sf          = new DirRespBaseBundle(djparam.sfDirWays, nrRnfNode, sfReplWayBits) // snoop filter
}

class DirWriteBaseBundle(nrWays: Int, nrMetas: Int, replWayBits: Int)(implicit p: Parameters) extends DJBundle with HasAddr {
    val wayOH       = UInt(nrWays.W)
    val metaVec     = Vec(nrMetas, new CHIStateBundle())
    val replMes     = UInt(replWayBits.W)
}

class DirWriteBundle(implicit p: Parameters) extends DJBundle {
    val s           = Decoupled(new DirWriteBaseBundle(djparam.selfWays, 1, sReplWayBits)) // self
    val sf          = Decoupled(new DirWriteBaseBundle(djparam.sfDirWays, nrRnfNode, sfReplWayBits)) // snoop filter
}

trait HasDirBankID extends DJBundle { val dirBankId = UInt(dirBankBits.W) }

class DirReadMSHRBundle(implicit p: Parameters) extends DJBundle with HasPipeID with HasDirBankID with HasMSHRSet

class MSHRRespDirBundle(implicit p: Parameters) extends DJBundle with HasPipeID with HasDirBankID {
    val addrs       = Vec(djparam.nrMSHRWays, Valid(UInt(addressBits.W)))
}




