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
import dongjiang._
import dongjiang.dcu._
import dongjiang.ddrc._
import chisel3.util.{Decoupled, DecoupledIO}
import xijiang.c2c.C2cLinkPort
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit}
import zhujiang.nhl2._
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import xijiang.router.base.IcnBundle
import xijiang.Ring
import xijiang.c2c.C2cLinkPort
import zhujiang.device.bridge.axi.AxiBridge


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
  val ccNodes         = zjParams.localRing.filter(_.nodeType == NodeType.CC)
  val nrLocalCc       = ccNodes.length
  def createNHL2IO(i: Int) = { val l2 = Module(new ConnectToNHL2(params, ccNodes(i))); l2 }

  val io              = IO(new Bundle { val fromNHL2 = Vec(nrLocalCc, Flipped(new CHIBundleDecoupled(params))) })
  val connectToNHL2s  = ccNodes.indices.map(i => createNHL2IO(i))
  connectToNHL2s.zipWithIndex.foreach {
    case (connect, i) =>
      connect.io.fromNHL2 <> io.fromNHL2(i)
      connect.io.toCcIcn  <> localRing.icnCcs.get(i)
  }


  /*
   * dongjiang
   */
  val hnfNodes   = zjParams.localRing.filter(_.nodeType == NodeType.HF)
  val dcuNodes  = zjParams.localRing.filter(_.nodeType == NodeType.S).filter(!_.mainMemory)
  val ddrcNode  = zjParams.localRing.filter(_.mainMemory).last
  require(zjParams.localRing.count(_.mainMemory) == 1)

  def createHnf(i: Int) = { val hnf = Module(new DongJiang(hnfNodes(i))); hnf }
  def createDCU(i: Int) = { val dcu = Module(new DCU(dcuNodes(i))); dcu }

  val dongjiang = hnfNodes.indices.map(i => createHnf(i))
  val dcus      = dcuNodes.indices.map(i => createDCU(i))
  val ddrc      = Module(new FakeDDRC(ddrcNode))

  dongjiang.zip(localRing.icnHfs.get).foreach { case(a, b) => a.io.toLocal <> b }
  dcus.zip(localRing.icnSns.get).foreach      { case(a, b) => a.io.sn(0) <> b }
  ddrc.io.sn    <> localRing.icnSns.get.last

}
