package zhujiang

import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import org.chipsalliance.cde.config.Parameters
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import xijiang.router.base.IcnBundle
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

  private def makeIOs(icns: Option[Seq[IcnBundle]], local: Boolean): Unit = {
    icns.foreach(_.foreach(icn => {
      val port = IO(icn.cloneType)
      port.suggestName(icn.node.icnStr(local))
      port <> icn
    }))
  }

  makeIOs(localRing.icnRfs, true)
  makeIOs(localRing.icnRis, true)
  makeIOs(localRing.icnHfs, true)
  makeIOs(localRing.icnHis, true)
  makeIOs(localRing.icnSns, true)

  makeIOs(csnRing.icnRfs, false)
  makeIOs(csnRing.icnHfs, false)

  if(csnRing.c2cs.isDefined) {
    csnRing.c2cs.foreach(_.zipWithIndex.foreach({ case (c2c, idx) =>
      val port = IO(c2c.cloneType)
      port <> c2c
      port.suggestName(s"c2c_$idx")

    }))
  }
  val io_chip = IO(Input(UInt(p(ZJParametersKey).chipAddrBits.W)))
  localRing.io_chip := io_chip
  csnRing.io_chip := io_chip
}
