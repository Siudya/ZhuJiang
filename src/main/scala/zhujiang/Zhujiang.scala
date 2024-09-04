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
import chisel3.util.{Decoupled, DecoupledIO}
import xijiang.c2c.C2cLinkPort
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit}
import zhujiang.nhl2._


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
   * NHL2 CHI Bundle Param
   */
  val params = new CHIBundleParameters(
    nodeIdBits = niw,
    addressBits = raw,
    dataBits = dw,
    dataCheck = dcw > 0,
    txnIdBits = 12,
    dbIdBits = 16
  )

  /*
   * IO
   */
  val nrLocalRn = zjparam.localRing.filter(_.nodeType == NodeType.R).length
  val nrLocalSn = zjparam.localRing.filter(_.nodeType == NodeType.S).length
  val nrLocalHNI = zjparam.localRing.filter(_.nodeType == NodeType.HI).length
  val nrCSNCn = zjparam.csnRing.filter(_.nodeType == NodeType.C).length

  val io = IO(new Bundle {
    val fromNHL2    = Vec(nrLocalRn, Flipped(new CHIBundleDecoupled(params)))
//    val localSn     = Vec(nrLocalSn, new SnIcn())
//    val localHnI    = Vec(nrLocalHNI, new HnIcn(local = true))
    val csnC2C      = Vec(nrCSNCn, new C2cLinkPort())
  })

  /*
   * xijiang
   */
  private val localRing = Module(new Ring(true))
  private val csnRing = Module(new Ring(false))

//  localRing.io.get.sn.zip(io.localSn).foreach { case(a, b) => a <> b }
//  localRing.io.get.hni.zip(io.localHnI).foreach { case(a, b) => a <> b }
  localRing.io.get.sn.foreach(_ <> DontCare)
  localRing.io.get.hni.foreach(_ <> DontCare)
  csnRing.io.get.c2c.zip(io.csnC2C).foreach { case(a, b) => a <> b }
  localRing.io.get.chip := zjparam.ringId.U
  csnRing.io.get.chip := zjparam.ringId.U

  /*
   * NHL2 Interface transform
   */
  val connectToNHL2s = Seq.fill(nrLocalRn) { Module(new ConnectToNHL2(params)) }

  connectToNHL2s.zipWithIndex.foreach {
    case(connect, i) =>
      connect.io.fromNHL2 <> io.fromNHL2(i)
      connect.io.toRnIcn <> localRing.io.get.rn(i)
  }

  /*
   * dongjiang
   */
  val dongjiang = Module(new DongJiang())

  dongjiang.io.toLocal <> localRing.io.get.hnf(0)
  if(nrCSNCn > 0) dongjiang.io.toCSNOpt.get.rn <> csnRing.io.get.rn(0)
  if(nrCSNCn > 0) dongjiang.io.toCSNOpt.get.hn <> csnRing.io.get.hnf(0)

}
