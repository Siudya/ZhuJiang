package zhujiang.device.crossbar

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.ZJModule
import zhujiang.tilelink._

abstract class BaseTLULXbar(implicit p: Parameters) extends ZJModule {
  def tlmParams: TilelinkParams
  def tlsParams: TilelinkParams
  def mstSeq: Seq[TLULBundle]
  def slvSeq: Seq[TLULBundle]
  def slvMatcher: Seq[UInt => Bool]

  private lazy val mstASeq = mstSeq.map(_.a)
  private lazy val mstDSeq = mstSeq.map(_.d)
  private lazy val slvASeq = slvSeq.map(_.a)
  private lazy val slvDSeq = slvSeq.map(_.d)
  private lazy val mstSize = mstSeq.length
  private lazy val slvSize = slvSeq.length

  private lazy val slvArbSeq = Seq.fill(slvSize)(Module(new Arbiter(new AFlit(tlsParams), mstSize)))
  private lazy val mstArbSeq = Seq.fill(mstSize)(Module(new Arbiter(new DFlit(tlmParams), slvSize)))

  private lazy val mstAReadies = Wire(Vec(mstSize, Vec(slvSize, Bool())))
  private lazy val slvDReadies = Wire(Vec(slvSize, Vec(mstSize, Bool())))

  private val addrLegal = Wire(Bool())
  private val reqValid = Wire(Bool())
  when(reqValid) {
    assert(addrLegal)
  }

  def runConnection(): Unit = {
    reqValid := Cat(mstASeq.map(_.valid)).orR
    addrLegal := Cat(mstASeq.map(a => Cat(slvMatcher.map(m => m(a.bits.address))).orR)).orR
    mstASeq.zip(mstAReadies).foreach({ case (a, b) => a.ready := Cat(b).orR })
    slvDSeq.zip(slvDReadies).foreach({ case (a, b) => a.ready := Cat(b).orR })
    for(sidx <- slvSeq.indices) {
      val slv = slvASeq(sidx)
      val matcher = slvMatcher(sidx)
      val arb = slvArbSeq(sidx)
      slv <> arb.io.out
      arb.io.in.zip(mstASeq).zipWithIndex.foreach({ case ((a, b), midx) =>
        a.valid := b.valid && matcher(b.bits.address)
        a.bits := b.bits
        a.bits.source := Cat(midx.U(log2Ceil(mstSize).W), b.bits.source)
        mstAReadies(midx)(sidx) := a.ready && matcher(b.bits.address)
      })
    }
  }
  for(midx <- mstDSeq.indices) {
    val mst = mstDSeq(midx)
    val arb = mstArbSeq(midx)
    mst <> arb.io.out
    arb.io.in.zip(slvSeq.map(_.d)).zipWithIndex.foreach { case ((a, b), sidx) =>
      val correct = b.bits.source(tlsParams.sourceBits - 1, tlmParams.sourceBits) === sidx.U
      a.valid := b.valid && correct
      a.bits := b.bits
      a.bits.source := b.bits.source(tlmParams.sourceBits - 1, 0)
      slvDReadies(sidx)(midx) := a.ready && correct
    }
  }
}
