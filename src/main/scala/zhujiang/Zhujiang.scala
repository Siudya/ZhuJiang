package zhujiang

import chisel3.{DontCare, Module}
import org.chipsalliance.cde.config.Parameters
import xijiang.Ring
import DONGJIANG._


class Zhujiang(implicit p: Parameters) extends ZJModule {
  require(p(ZJParametersKey).tfsParams.isEmpty)
  /*
   * xijiang
   */
  private val localRing = Module(new Ring(true))
  private val csnRing = Module(new Ring(false))

  val tmp_local_io = IO(localRing.io.get.cloneType)
  val tmp_csn_io = IO(csnRing.io.get.cloneType)

  tmp_local_io <> localRing.io.get
  tmp_csn_io <> csnRing.io.get

  /*
   * dongjiang
   */
  val dongjiang = Module(new DongJiang())

  dongjiang <> DontCare

}
