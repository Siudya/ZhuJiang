package zhujiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.Ring
import DONGJIANG._
import chisel3.util.{Decoupled, DecoupledIO}
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit}


class Zhujiang(implicit p: Parameters) extends ZJModule {
  require(p(ZJParametersKey).tfsParams.isEmpty)
  /*
   * xijiang
   */
  private val localRing = Module(new Ring(true))
  private val csnRing = Module(new Ring(false))

  localRing.io.get <> DontCare
  csnRing.io.get <> DontCare

  /*
   * dongjiang
   */
  val dongjiang = Module(new DongJiang())

  dongjiang.io <> DontCare

  // hn tx valid
  dongjiang.io.rnSlvChi(0).txreq.valid := localRing.io.get.hnf(0).tx.req.valid
  dongjiang.io.rnSlvChi(0).txdat.valid := localRing.io.get.hnf(0).tx.data.valid
  dongjiang.io.rnSlvChi(0).txrsp.valid := localRing.io.get.hnf(0).tx.resp.valid
  // hn tx bits
  dongjiang.io.rnSlvChi(0).txreq.bits := localRing.io.get.hnf(0).tx.req.bits.asTypeOf(new ReqFlit)
  dongjiang.io.rnSlvChi(0).txdat.bits := localRing.io.get.hnf(0).tx.data.bits.asTypeOf(new DataFlit)
  dongjiang.io.rnSlvChi(0).txrsp.bits := localRing.io.get.hnf(0).tx.resp.bits.asTypeOf(new RespFlit)
  // hn tx ready
  localRing.io.get.hnf(0).tx.req.ready := dongjiang.io.rnSlvChi(0).txreq.ready
  localRing.io.get.hnf(0).tx.data.ready := dongjiang.io.rnSlvChi(0).txdat.ready
  localRing.io.get.hnf(0).tx.resp.ready := dongjiang.io.rnSlvChi(0).txrsp.ready

  // hn rx valid
  localRing.io.get.hnf(0).rx.snoop.valid  := dongjiang.io.rnSlvChi(0).rxsnp.valid
  localRing.io.get.hnf(0).rx.data.valid := dongjiang.io.rnSlvChi(0).rxdat.valid
  localRing.io.get.hnf(0).rx.resp.valid := dongjiang.io.rnSlvChi(0).rxrsp.valid
  // hn rx valid
  localRing.io.get.hnf(0).rx.snoop.bits := dongjiang.io.rnSlvChi(0).rxsnp.bits.asUInt
  localRing.io.get.hnf(0).rx.data.bits := dongjiang.io.rnSlvChi(0).rxdat.bits.asUInt
  localRing.io.get.hnf(0).rx.resp.bits := dongjiang.io.rnSlvChi(0).rxrsp.bits.asUInt
  // hn rx valid
  dongjiang.io.rnSlvChi(0).rxsnp.ready := localRing.io.get.hnf(0).rx.snoop.ready
  dongjiang.io.rnSlvChi(0).rxdat.ready := localRing.io.get.hnf(0).rx.data.ready
  dongjiang.io.rnSlvChi(0).rxrsp.ready := localRing.io.get.hnf(0).rx.resp.ready





}
