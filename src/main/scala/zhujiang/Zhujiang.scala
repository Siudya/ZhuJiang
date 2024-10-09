package zhujiang

import chisel3._
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
import DONGJIANG.DDRC._
import chisel3.util.{Decoupled, DecoupledIO}
import xijiang.c2c.C2cLinkPort
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit}
import zhujiang.nhl2._
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import xijiang.router.base.IcnBundle
import xijiang.Ring
import xijiang.c2c.C2cLinkPort


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
//  private val csnRing = Module(new Ring(false))

  localRing.icnHis.get.foreach(_ <> DontCare)
  localRing.io_chip := 0.U

  /*
   * NHL2 CHI Bundle Param
   */
  val params    = new CHIBundleParameters(
    nodeIdBits  = niw,
    addressBits = raw,
    dataBits    = dw,
    dataCheck   = dcw > 0,
    issue       = "G"
  )

  /*
   *Connect NHL2 IO <> xijiang
   */
  val nrLocalRn = localRing.icnRfs.get.length
  val io = IO(new Bundle { val fromNHL2 = Vec(nrLocalRn, Flipped(new CHIBundleDecoupled(params))) })
  val connectToNHL2s = Seq.fill(nrLocalRn) { Module(new ConnectToNHL2(params, zjParams.localRing.filter(_.nodeType == NodeType.RF).last)) }
  connectToNHL2s.zipWithIndex.foreach {
    case (connect, i) =>
      connect.io.fromNHL2 <> io.fromNHL2(i)
      connect.io.toRnIcn  <> localRing.icnRfs.get(i)
  }


  /*
   * dongjiang
   */
  val ddrcNode  = zjParams.localRing.filter(_.mainMemory).last
  val dcuNodes  = zjParams.localRing.filter(_.nodeType == NodeType.S).filter(!_.mainMemory)
  require(zjParams.localRing.count(_.mainMemory) == 1)

  val dongjiang = Module(new DongJiang(zjParams.localRing.filter(_.nodeType == NodeType.HF).last))
  val ddrc      = Module(new FakeDDRC(ddrcNode))
  val dcus      = Seq.fill(dcuNodes.length) { Module(new DCU(dcuNodes.head)) }

  dongjiang.io.toLocal  <> localRing.icnHfs.get.last
  ddrc.io.sn            <> localRing.icnSns.get.last
  dcus.zip(localRing.icnSns.get.init).foreach { case(a, b) => a.io.sn(0) <> b }

}
