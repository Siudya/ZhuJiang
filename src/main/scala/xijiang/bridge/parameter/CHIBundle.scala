package xijiang.bridge.parameter

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import scala.collection.immutable.ListMap
import zhujiang._
import zhujiang.chi._


class CHIBundleDecoupled(implicit p: Parameters) extends ZJBundle {
    val txreq = Decoupled(new ReqFlit)
    val txdat = Decoupled(new DataFlit)
    val txrsp = Decoupled(new RespFlit)

    val rxrsp = Flipped(Decoupled(new RespFlit))
    val rxdat = Flipped(Decoupled(new DataFlit))
    val rxsnp = Flipped(Decoupled(new SnoopFlit))
}

class CHIChannelIO[T <: Data](gen: T) extends Bundle {
    val flitpend = Output(Bool())
    val flitv    = Output(Bool())
    val flit     = Output(gen)
    val lcrdv    = Input(Bool())
}

object CHIChannelIO {
    def apply[T <: Data](gen: T): CHIChannelIO[T] = new CHIChannelIO(gen)
}

class CHIBundleDownstream(implicit p: Parameters) extends Record {
    val txreq: CHIChannelIO[ReqFlit]    = CHIChannelIO(new ReqFlit)
    val txdat: CHIChannelIO[DataFlit]   = CHIChannelIO(new DataFlit)
    val txrsp: CHIChannelIO[RespFlit]   = CHIChannelIO(new RespFlit)

    val rxrsp: CHIChannelIO[RespFlit]   = Flipped(CHIChannelIO(new RespFlit))
    val rxdat: CHIChannelIO[DataFlit]   = Flipped(CHIChannelIO(new DataFlit))
    val rxsnp: CHIChannelIO[SnoopFlit]  = Flipped(CHIChannelIO(new SnoopFlit))

    // @formatter:off
    val elements = ListMap(
        "txreq" -> txreq,
        "txdat" -> txdat,
        "txrsp" -> txrsp,
        "rxrsp" -> rxrsp,
        "rxdat" -> rxdat,
        "rxsnp" -> rxsnp
    )
    // @formatter:on
}

class CHIBundleUpstream(implicit p: Parameters) extends Record {
    val txreq: CHIChannelIO[ReqFlit]    = Flipped(CHIChannelIO(new ReqFlit))
    val txdat: CHIChannelIO[DataFlit]   = Flipped(CHIChannelIO(new DataFlit))
    val txrsp: CHIChannelIO[RespFlit]   = Flipped(CHIChannelIO(new RespFlit))

    val rxrsp: CHIChannelIO[RespFlit]   = CHIChannelIO(new RespFlit)
    val rxdat: CHIChannelIO[DataFlit]   = CHIChannelIO(new DataFlit)
    val rxsnp: CHIChannelIO[SnoopFlit]  = CHIChannelIO(new SnoopFlit)

    // @formatter:off
    val elements = ListMap(
        "txreq" -> txreq,
        "txdat" -> txdat,
        "txrsp" -> txrsp,
        "rxrsp" -> rxrsp,
        "rxdat" -> rxdat,
        "rxsnp" -> rxsnp
    )
    // @formatter:on


}
