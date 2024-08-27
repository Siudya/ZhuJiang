package zhujiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{NodeType, Ring}
import DONGJIANG._
import chisel3.util.{Decoupled, DecoupledIO}
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit}
import zhujiang.nhl2._


class Zhujiang(implicit p: Parameters) extends ZJModule {
  require(p(ZJParametersKey).tfsParams.isEmpty)

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
  val nrRn = zjparam.localRing.filter(_.nodeType == NodeType.R).length
  val io = IO(new Bundle {
    val fromNHL2    = Vec(nrRn, Flipped(new CHIBundleDecoupled(params)))
  })

  io <> DontCare

  /*
   * xijiang
   */
  private val localRing = Module(new Ring(true))
  private val csnRing = Module(new Ring(false))

  localRing.io.get <> DontCare
  csnRing.io.get <> DontCare

  /*
   * NHL2 Interface transform
   */
  val connectToNHL2s = Seq.fill(nrRn) { Module(new ConnectToNHL2(params)) }

  connectToNHL2s.zipWithIndex.foreach {
    case(connect, i) =>
      connect.io.fromNHL2 <> io.fromNHL2(i)
      connect.io.toRnIcn <> localRing.io.get.rn(i)
  }

  /*
   * dongjiang
   */
  val dongjiang = Module(new DongJiang())

  dongjiang.io <> DontCare

  dongjiang.io.fromHnIcn.zip(localRing.io.get.hnf).foreach { case(a, b) => a <> b }

  dongjiang.io.toSnIcn.zip(localRing.io.get.sn).foreach { case(a, b) => a <> b }

}
