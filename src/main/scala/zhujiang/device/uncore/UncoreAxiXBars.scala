package zhujiang.device.uncore

import chisel3._
import org.chipsalliance.cde.config.Parameters
import zhujiang.HasZJParams
import zhujiang.axi.{AxiParams, BaseAxiXbar}
import zhujiang.chi.ReqAddrBundle

class AxiCfgXBar(icnAxiParams: AxiParams)(implicit val p: Parameters) extends BaseAxiXbar(Seq(icnAxiParams)) with HasZJParams {
  val misc = IO(new Bundle {
    val chip = Input(UInt(zjParams.nodeAidBits.W))
  })
  private def slvMatcher(internal: Boolean)(addr: UInt): Bool = {
    val reqAddr = addr.asTypeOf(new ReqAddrBundle)
    val matchRes = WireInit(reqAddr.chip === misc.chip && 0x3800_0000.U <= reqAddr.devAddr && reqAddr.devAddr < 0x4000_0000.U)
    if(internal) {
      matchRes
    } else {
      !matchRes
    }
  }
  val slvMatchersSeq = Seq(slvMatcher(internal = true), slvMatcher(internal = false))
  initialize()
}

class AxiDmaXBar(dmaAxiParams: Seq[AxiParams])(implicit val p: Parameters) extends BaseAxiXbar(dmaAxiParams) with HasZJParams {
  val slvMatchersSeq = Seq((_: UInt) => true.B)
  initialize()
}