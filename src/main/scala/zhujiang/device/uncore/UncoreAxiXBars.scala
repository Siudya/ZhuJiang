package zhujiang.device.uncore

import chisel3._
import org.chipsalliance.cde.config.Parameters
import zhujiang.HasZJParams
import zhujiang.axi.AxiParams
import zhujiang.chi.ReqAddrBundle
import zhujiang.device.crossbar.BaseAxiXbar

class AxiCfgXBar(icnAxiParams:AxiParams)(implicit val p: Parameters) extends BaseAxiXbar with HasZJParams {
  val misc = IO(new Bundle{
    val chip = Input(UInt(zjParams.nodeAidBits.W))
  })
  val mstParams = Seq(icnAxiParams)
  private def slvMatcher(internal:Boolean)(addr:UInt):Bool = {
    val reqAddr = addr.asTypeOf(new ReqAddrBundle)
    val matchRes = WireInit(reqAddr.chip === misc.chip && 0x3000_0000.U <= reqAddr.devAddr && reqAddr.devAddr < 0x4000_0000.U)
    if(internal) {
      matchRes
    } else {
      !matchRes
    }
  }
  val slvMatchersSeq = Seq(slvMatcher(internal = true), slvMatcher(internal = false))
  initialize()
}

class AxiDmaXBar(dmaAxiParams:Seq[AxiParams])(implicit val p: Parameters) extends BaseAxiXbar with HasZJParams {
  val mstParams = dmaAxiParams
  val slvMatchersSeq = Seq((_:UInt) => true.B)
  initialize()
}