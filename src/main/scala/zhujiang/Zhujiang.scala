package zhujiang

import chisel3.Module
import chisel3.experimental.{ChiselAnnotation, annotate}
import org.chipsalliance.cde.config.Parameters
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import xijiang.router.base.BaseIcnBundle
import xijiang.{Ring, RingIO}

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
  private val (lrns, lhfs, lhis, lsns, lc2cs, lchip) = RingIO(true, p)
  private val (crns, chfs, chis, csns, cc2cs, cchip) = RingIO(false, p)

  private def conn(a: Seq[BaseIcnBundle], b: Option[Seq[BaseIcnBundle]]) = {
    if(b.isDefined) {
      a.zip(b.get).foreach({ case (m, s) =>
        m <> s
      })
    }
  }
  conn(lrns, localRing.icnRns)
  conn(lhfs, localRing.icnHfs)
  conn(lhis, localRing.icnHis)
  conn(lsns, localRing.icnSns)

  conn(crns, csnRing.icnRns)
  conn(chfs, csnRing.icnRns)
  conn(chis, csnRing.icnRns)
  conn(csns, csnRing.icnRns)

  if(csnRing.icnC2cs.isDefined) {
    cc2cs.zip(csnRing.icnC2cs.get).foreach({ case (a, b) => a <> b })
  }
  localRing.ioChip.foreach(_ := lchip)
  csnRing.ioChip.foreach(_ := cchip)
}
