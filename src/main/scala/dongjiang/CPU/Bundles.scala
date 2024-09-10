package DONGJIANG.CPU

import DONGJIANG._
import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import scala.collection.immutable.ListMap
import scala.math.{max, min}


// ---------------------------------------------------------------- Slice Base Bundle ----------------------------------------------------------------------------- //
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






