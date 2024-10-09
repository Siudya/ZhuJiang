package zhujiang.device.cluster

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.device.crossbar.BaseTLULXbar
import zhujiang.tilelink.{TLULBundle, TilelinkParams}

class PeriXBar(inNum: Int, mstIdBits: Int, coreNum: Int, privatePeriNum: Int, sharePeriNum: Int)(implicit p: Parameters) extends BaseTLULXbar {
  private val coreIdBits = clusterIdBits - nodeAidBits
  private val addrBits = 16 + coreIdBits
  private val peripheralSpaceBits = 11
  val tlmParams = TilelinkParams(sourceBits = mstIdBits, dataBits = 64, addrBits = addrBits)
  val tlsParams = TilelinkParams(sourceBits = mstIdBits + log2Ceil(inNum), dataBits = 64, addrBits = peripheralSpaceBits)
  val io = IO(new Bundle {
    val in = Vec(inNum, Flipped(new TLULBundle(tlmParams)))
    val core = Input(Vec(coreNum, UInt(coreIdBits.W)))
    val sharePeri = Vec(sharePeriNum, new TLULBundle(tlsParams))
    val privatePeri = Vec(coreNum, Vec(privatePeriNum, new TLULBundle(tlsParams)))
  })
  private val sharedAddrMatcher = Seq.tabulate(sharePeriNum)(i => (addr: UInt) => addr(15) && addr(14, 8) === i.U)
  private val privateAddrMatcher = Seq.tabulate(coreNum, privatePeriNum)(
    (c, i) => (addr: UInt) => addr(addrBits - 1, 16) === io.core(c) && !addr(15) && addr(14, 8) === i.U
  )
  val mstSeq = io.in
  val slvSeq = io.sharePeri ++ io.privatePeri.flatten
  val slvMatcher = sharedAddrMatcher ++ privateAddrMatcher.flatten
  private val slvMax = 1 << (16 - 1 - peripheralSpaceBits)
  require(slvSeq.length < slvMax)
  runConnection()
}
