package zhujiang

import chisel3.Module
import org.chipsalliance.cde.config.Parameters
import xijiang.Ring

class Zhujiang(implicit p: Parameters) extends ZJModule {
  require(p(ZJParametersKey).tfsParams.isEmpty)
  private val localRing = Module(new Ring(true))
  private val csnRing = Module(new Ring(false))
  val tmp_local_io = IO(localRing.io.get.cloneType)
  val tmp_csn_io = IO(csnRing.io.get.cloneType)
  tmp_local_io <> localRing.io.get
  tmp_csn_io <> csnRing.io.get
}
