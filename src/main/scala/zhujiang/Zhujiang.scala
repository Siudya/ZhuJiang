package zhujiang

import chisel3.Module
import chisel3.experimental.{ChiselAnnotation, annotate}
import org.chipsalliance.cde.config.Parameters
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import xijiang.Ring

class Zhujiang(implicit p: Parameters) extends ZJModule {
  require(p(ZJParametersKey).tfsParams.isEmpty)
  if(p(ZJParametersKey).modulePrefix != "") {
    val mod = this.toNamed
    annotate(new ChiselAnnotation {
      def toFirrtl = NestedPrefixModulesAnnotation(mod, p(ZJParametersKey).modulePrefix, true)
    })
  }
  private val localRing = Module(new Ring(true))
  private val csnRing = Module(new Ring(false))
  val tmp_local_io = IO(localRing.io.get.cloneType)
  val tmp_csn_io = IO(csnRing.io.get.cloneType)
  tmp_local_io <> localRing.io.get
  tmp_csn_io <> csnRing.io.get
}
