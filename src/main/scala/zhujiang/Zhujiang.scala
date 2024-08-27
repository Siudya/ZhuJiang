package zhujiang

import DONGJIANG.CHI.CHIBundleDecoupled
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.Ring
import DONGJIANG._
import chisel3.util.{Decoupled, DecoupledIO}
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit}


class Zhujiang(implicit p: Parameters) extends ZJModule {
  require(p(ZJParametersKey).tfsParams.isEmpty)
  /*
   * xijiang
   */
  private val localRing = Module(new Ring(true))
  private val csnRing = Module(new Ring(false))

  localRing.io.get <> DontCare
  csnRing.io.get <> DontCare

  /*
   * dongjiang
   */
  val dongjiang = Module(new DongJiang())

  dongjiang.io <> DontCare

  dongjiang.io.fromHnIcn.zip(localRing.io.get.hnf).foreach { case(a, b) => a <> b }

  dongjiang.io.toSnIcn.zip(localRing.io.get.sn).foreach { case(a, b) => a <> b }

}
