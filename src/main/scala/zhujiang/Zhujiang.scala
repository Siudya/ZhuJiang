package zhujiang

import chisel3.Module
import chisel3.experimental.{ChiselAnnotation, annotate}
import org.chipsalliance.cde.config.Parameters
import xijiang.router._
import xijiang.Ring
import chisel3._
import chisel3.util._
import chisel3.experimental.{ChiselAnnotation, annotate}
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import org.chipsalliance.cde.config.Parameters
import xijiang.{NodeType, Ring}
import DONGJIANG._
import DONGJIANG.DCU._
import chisel3.util.{Decoupled, DecoupledIO}
import xijiang.c2c.C2cLinkPort
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit}
import zhujiang.nhl2._
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

  print(
    s"""
       |ZhuJiang Message: {
       |  Support Protocol: CHI-G
       |  nodeIdBits: ${niw}
       |  requestAddrBits: ${raw}
       |  dataBits: ${dw}
       |  dataCheckBits: ${dcw}
       |  txnIdBits: 12
       |  dbIdBits: 16
       |}
       |""".stripMargin)


  /*
   * xijiang
   */
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
  conn(chfs, csnRing.icnHfs)
  conn(chis, csnRing.icnHis)

  if(csnRing.icnC2cs.isDefined) {
    cc2cs.zip(csnRing.icnC2cs.get).foreach({ case (a, b) => a <> b })
  }
  localRing.ioChip.foreach(_ := lchip)
  csnRing.ioChip.foreach(_ := cchip)

  /*
   * NHL2 CHI Bundle Param
   */
  val params    = new CHIBundleParameters(
    nodeIdBits  = niw,
    addressBits = raw,
    dataBits    = dw,
    dataCheck   = dcw > 0,
    txnIdBits   = 12,
    dbIdBits    = 16
  )

  /*
   *Connect NHL2 IO <> xijiang
   */
  val nrLocalRn = lrns.length
  val io = IO(new Bundle { val fromNHL2 = Vec(nrLocalRn, Flipped(new CHIBundleDecoupled(params))) })
  val connectToNHL2s = Seq.fill(nrLocalRn) { Module(new ConnectToNHL2(params, zjparam.localRing.filter(_.nodeType == NodeType.R).last)) }
  connectToNHL2s.zipWithIndex.foreach {
    case (connect, i) =>
      connect.io.fromNHL2 <> io.fromNHL2(i)
      connect.io.toRnIcn  <> localRing.icnRns.get(i)
  }


  /*
   * dongjiang
   */
  val ddrcNode  = zjparam.localRing.filter(_.mainMemory).last
  require(zjparam.localRing.count(_.mainMemory) == 1)

  val dongjiang = Module(new DongJiang())
  val ddrc      = Module(new DCU(ddrcNode))

  dongjiang.io.toLocal  <> localRing.icnHfs.get.last
  ddrc.io.sn            <> localRing.icnSns.get.last

}
