package zhujiang.nhl2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xijiang.Node
import xijiang.router.base.IcnBundle
import zhujiang.{ZJBundle, ZJModule}
import zhujiang.chi._

import scala.collection.immutable.ListMap

case class CHIBundleParameters(
                                nodeIdBits: Int,
                                addressBits: Int,
                                dataBits: Int,
                                dataCheck: Boolean,
                                issue: String = "G"
                                // TODO: has snoop
                              ) {
    val txnIdBits: Int = issue match {
        case "B"       => 8
        case "E" | "G" => 12
        case _         => 12
    }
    val dbIdBits: Int = issue match {
        case "B"       => 8
        case "E" | "G" => 12
        case _         => 12
    }
    val reqOpcodeBits: Int = issue match {
        case "B"       => 6
        case "E" | "G" => 7
        case _         => 7
    }
    val rspOpcodeBits: Int = issue match {
        case "B"       => 4
        case "E" | "G" => 5
        case _         => 5
    }
    val datOpcodeBits: Int = issue match {
        case "B"       => 3
        case "E" | "G" => 4
        case _         => 4
    }
    require(nodeIdBits >= 7 && nodeIdBits <= 11)
    require(addressBits >= 44 && addressBits <= 52)
    require(isPow2(dataBits))
    require(dataBits == 128 || dataBits == 256 || dataBits == 512)
}


object CHIBundleParameters {
    def apply(
               nodeIdBits: Int = 9,
               addressBits: Int = 48,
               dataBits: Int = 256,
               dataCheck: Boolean = false,
               issue: String = "G"
             ): CHIBundleParameters = new CHIBundleParameters(
        nodeIdBits  = nodeIdBits,
        addressBits = addressBits,
        dataBits    = dataBits,
        dataCheck   = dataCheck,
        issue       = issue
    )
}


class CHIBundleREQ(params: CHIBundleParameters)(implicit p: Parameters) extends ZJBundle {
    val channelName = "'REQ' channel"

    val rsvdc         = UInt(4.W)
    val traceTag      = Bool()
    val expCompAck    = Bool()
    val snoopMe       = Bool()
    val lpID          = UInt(5.W)
    val snpAttr       = UInt(1.W)
    val memAttr       = UInt(4.W)
    val pCrdType      = UInt(4.W)
    val order         = UInt(2.W)
    val allowRetry    = Bool()
    val likelyshared  = Bool()
    val ns            = Bool()                    // TODO: not use?
    val addr          = UInt(params.addressBits.W)
    val size          = UInt(3.W)
    val opcode        = UInt(params.reqOpcodeBits.W)
    val returnTxnID   = UInt(8.W)
    val stashNIDValid = Bool()
    val returnNID     = UInt(params.nodeIdBits.W) // TODO: not use?
    val txnID         = UInt(params.txnIdBits.W)
    val srcID         = UInt(params.nodeIdBits.W)
    val tgtID         = UInt(params.nodeIdBits.W) // TODO: not use?
    val qos           = UInt(4.W)                 // TODO: not use?

    def toReqFlit = {
        val reqFlit = Wire(new ReqFlit)

        reqFlit.RSVDC := this.rsvdc
        reqFlit.SecID1 := DontCare
        reqFlit.MECID := DontCare
        reqFlit.PBHA := DontCare
        reqFlit.MPAM := DontCare
        reqFlit.TraceTag := this.traceTag
        reqFlit.TagOp := DontCare
        reqFlit.ExpCompAck := this.expCompAck
        reqFlit.Excl := DontCare
        reqFlit.PGroupID := DontCare
        reqFlit.SnpAttr := this.snpAttr
        reqFlit.MemAttr := this.memAttr
        reqFlit.PCrdType := this.pCrdType
        reqFlit.Order := this.order
        reqFlit.AllowRetry := this.allowRetry
        reqFlit.LikelyShared := this.likelyshared
        reqFlit.NSE := DontCare
        reqFlit.NS := this.ns
        reqFlit.Addr := this.addr
        reqFlit.Size := this.size
        reqFlit.Opcode := this.opcode
        reqFlit.ReturnTxnID := this.returnTxnID
        reqFlit.StashNIDValid := this.stashNIDValid
        reqFlit.ReturnNID := this.returnNID
        reqFlit.TxnID := this.txnID
        reqFlit.SrcID := this.srcID
        reqFlit.TgtID := this.tgtID
        reqFlit.QoS := this.qos

        reqFlit
    }
}

class CHIBundleRSP(params: CHIBundleParameters)(implicit p: Parameters) extends ZJBundle {
    val channelName = "'RSP' channel"

    val traceTag = Bool()
    val pCrdType = UInt(4.W)
    val dbID     = UInt(params.dbIdBits.W)
    val fwdState = UInt(3.W)                 // Used for DCT
    val resp     = UInt(3.W)
    val respErr  = UInt(2.W)
    val opcode   = UInt(params.rspOpcodeBits.W)
    val txnID    = UInt(params.txnIdBits.W)
    val srcID    = UInt(params.nodeIdBits.W)
    val tgtID    = UInt(params.nodeIdBits.W) // TODO: not use?
    val qos      = UInt(4.W)                 // TODO: not use?

    def toRespFlit = {
        val respFlit = Wire(new RespFlit)

        respFlit.TraceTag := this.traceTag
        respFlit.TagOp := DontCare
        respFlit.PCrdType := this.pCrdType
        respFlit.DBID := this.dbID
        respFlit.CBusy := DontCare
        respFlit.FwdState := this.fwdState
        respFlit.Resp := this.resp
        respFlit.RespErr := this.respErr
        respFlit.Opcode := this.opcode
        respFlit.TxnID := this.txnID
        respFlit.SrcID := this.srcID
        respFlit.TgtID := this.tgtID
        respFlit.QoS := this.qos

        respFlit
    }

    def fromRespFlit(in: UInt) = {
        require(in.getWidth == respFlitBits)
        val respFlit = in.asTypeOf(new RespFlit)
        val rsp = Wire(new CHIBundleRSP(params))

        rsp.qos := respFlit.QoS
        rsp.tgtID := respFlit.TgtID
        rsp.srcID := respFlit.SrcID
        rsp.txnID := respFlit.TxnID
        rsp.opcode := respFlit.Opcode
        rsp.respErr := respFlit.RespErr
        rsp.resp := respFlit.Resp
        rsp.fwdState := respFlit.FwdState
        rsp.dbID := respFlit.DBID
        rsp.pCrdType := respFlit.PCrdType
        rsp.traceTag := respFlit.TraceTag

        rsp
    }

}

class CHIBundleSNP(params: CHIBundleParameters)(implicit p: Parameters) extends ZJBundle {
    val channelName = "'SNP' channel"

    val traceTag    = Bool()
    val retToSrc    = Bool()
    val doNotGoToSD = Bool()
    val ns          = Bool()
    val addr        = UInt((params.addressBits - 3).W)
    val opcode      = UInt(5.W)
    val fwdTxnID    = UInt(params.txnIdBits.W)  // Used for DCT
    val fwdNID      = UInt(params.nodeIdBits.W) // Used for DCT
    val txnID       = UInt(params.txnIdBits.W)
    val srcID       = UInt(params.nodeIdBits.W)
    val qos         = UInt(4.W)                 // TODO: not use?

    def fromSnoopFlit(in: UInt) = {
        require(in.getWidth == snoopFlitBits)
        val snoopFlit = in.asTypeOf(new SnoopFlit)
        val snp = Wire(new CHIBundleSNP(params))

        snp.qos := snoopFlit.QoS
        snp.srcID := snoopFlit.SrcID
        snp.txnID := snoopFlit.TxnID
        snp.fwdNID := snoopFlit.FwdNID
        snp.fwdTxnID := snoopFlit.FwdTxnID
        snp.opcode := snoopFlit.Opcode
        snp.addr := snoopFlit.Addr
        snp.ns := snoopFlit.NS
        snp.doNotGoToSD := snoopFlit.DoNotGoToSD
        snp.retToSrc := snoopFlit.RetToSrc
        snp.traceTag := snoopFlit.TraceTag

        snp
    }
}

class CHIBundleDAT(params: CHIBundleParameters)(implicit p: Parameters) extends ZJBundle {
    val channelName = "'DAT' channel"

    val poison    = if (params.dataCheck) Some(UInt((params.dataBits / 64).W)) else None
    val dataCheck = if (params.dataCheck) Some(UInt((params.dataBits / 8).W)) else None
    val data      = UInt(params.dataBits.W)
    val be        = UInt((params.dataBits / 8).W)
    val rsvdc     = UInt(4.W)
    val traceTag  = Bool()
    val dataID    = UInt(2.W)
    val ccID      = UInt(2.W)                 // TODO: not use?
    val dbID      = UInt(params.dbIdBits.W)
    val fwdState  = UInt(3.W)                 // Used for DCT
    val resp      = UInt(3.W)
    val respErr   = UInt(2.W)
    val opcode    = UInt(params.datOpcodeBits.W)
    val homeNID   = UInt(params.nodeIdBits.W) // Used for DCT
    val txnID     = UInt(params.txnIdBits.W)
    val srcID     = UInt(params.nodeIdBits.W)
    val tgtID     = UInt(params.nodeIdBits.W) // TODO: not use?
    val qos       = UInt(4.W)                 // TODO: not use?

    def toDataFlit = {
        val dataFlit = Wire(new DataFlit)

        dataFlit.Poison := this.poison.getOrElse(0.U)
        dataFlit.DataCheck := this.dataCheck.getOrElse(0.U)
        dataFlit.Data := this.data
        dataFlit.BE := this.be
        dataFlit.RSVDC := this.rsvdc
        dataFlit.Replicate := DontCare
        dataFlit.NumDat := DontCare
        dataFlit.CAH := DontCare
        dataFlit.TraceTag := this.traceTag
        dataFlit.TU := DontCare
        dataFlit.Tag := DontCare
        dataFlit.TagOp := DontCare
        dataFlit.DataID := this.dataID
        dataFlit.CCID := this.ccID
        dataFlit.DBID := this.dbID
        dataFlit.CBusy := DontCare
        dataFlit.DataPull := this.data
        dataFlit.DataSource := this.data
        dataFlit.Resp := this.resp
        dataFlit.RespErr := this.respErr
        dataFlit.Opcode := this.opcode
        dataFlit.HomeNID := this.homeNID
        dataFlit.TxnID := this.txnID
        dataFlit.SrcID := this.srcID
        dataFlit.TgtID := this.tgtID
        dataFlit.QoS := this.qos

        dataFlit
    }

    def fromDataFlit(in: UInt) = {
        require(in.getWidth == dataFlitBits)
        val dataFlit = in.asTypeOf(new DataFlit())
        val dat = Wire(new CHIBundleDAT(params))

        dat.qos := dataFlit.QoS
        dat.tgtID := dataFlit.TgtID
        dat.srcID := dataFlit.SrcID
        dat.txnID := dataFlit.TxnID
        dat.homeNID := dataFlit.HomeNID
        dat.opcode := dataFlit.Opcode
        dat.respErr := dataFlit.RespErr
        dat.resp := dataFlit.Resp
        dat.fwdState := dataFlit.FwdState
        dat.dbID := dataFlit.DBID
        dat.ccID := dataFlit.CCID
        dat.dataID := dataFlit.DataID
        dat.traceTag := dataFlit.TraceTag
        dat.rsvdc := dataFlit.RSVDC
        dat.be := dataFlit.BE
        dat.data := dataFlit.Data
        if(params.dataCheck){
            dat.dataCheck.get := dataFlit.DataCheck
            dat.poison.get := dataFlit.Poison
        }

        dat
    }

}

class CHIBundleDecoupled(params: CHIBundleParameters)(implicit p: Parameters) extends ZJBundle {
    val txreq = Decoupled(UInt(new CHIBundleREQ(params).getWidth.W))
    val txdat = Decoupled(UInt(new CHIBundleDAT(params).getWidth.W))
    val txrsp = Decoupled(UInt(new CHIBundleRSP(params).getWidth.W))

    val rxrsp = Flipped(Decoupled(UInt(new CHIBundleRSP(params).getWidth.W)))
    val rxdat = Flipped(Decoupled(UInt(new CHIBundleDAT(params).getWidth.W)))
    val rxsnp = Flipped(Decoupled(UInt(new CHIBundleSNP(params).getWidth.W)))
}

class ConnectToNHL2(params: CHIBundleParameters, node: Node)(implicit p: Parameters) extends ZJModule {
    val io = IO(new Bundle {
        val fromNHL2    = Flipped(new CHIBundleDecoupled(params))
        val toCcIcn     = Flipped(new IcnBundle(node))
    })

    io.toCcIcn.rx.req.get.valid     := io.fromNHL2.txreq.valid
    io.toCcIcn.rx.req.get.bits      := io.fromNHL2.txreq.bits.asTypeOf(new CHIBundleREQ(params)).toReqFlit
    io.fromNHL2.txreq.ready         := io.toCcIcn.rx.req.get.ready

    io.toCcIcn.rx.resp.get.valid    := io.fromNHL2.txrsp.valid
    io.toCcIcn.rx.resp.get.bits     := io.fromNHL2.txrsp.bits.asTypeOf(new CHIBundleRSP(params)).toRespFlit
    io.fromNHL2.txrsp.ready         := io.toCcIcn.rx.resp.get.ready

    io.toCcIcn.rx.data.get.valid    := io.fromNHL2.txdat.valid
    io.toCcIcn.rx.data.get.bits     := io.fromNHL2.txdat.bits.asTypeOf(new CHIBundleDAT(params)).toDataFlit
    io.fromNHL2.txdat.ready         := io.toCcIcn.rx.data.get.ready

    io.fromNHL2.rxsnp.valid         := io.toCcIcn.tx.snoop.get.valid
    io.fromNHL2.rxsnp.bits          := io.fromNHL2.rxsnp.bits.asTypeOf(new CHIBundleSNP(params)).fromSnoopFlit(io.toCcIcn.tx.snoop.get.bits.asUInt).asUInt
    io.toCcIcn.tx.snoop.get.ready   := io.fromNHL2.rxsnp.ready

    io.fromNHL2.rxrsp.valid         := io.toCcIcn.tx.resp.get.valid
    io.fromNHL2.rxrsp.bits          := io.fromNHL2.rxrsp.bits.asTypeOf(new CHIBundleRSP(params)).fromRespFlit(io.toCcIcn.tx.resp.get.bits.asUInt).asUInt
    io.toCcIcn.tx.resp.get.ready    := io.fromNHL2.rxrsp.ready

    io.fromNHL2.rxdat.valid         := io.toCcIcn.tx.data.get.valid
    io.fromNHL2.rxdat.bits          := io.fromNHL2.rxdat.bits.asTypeOf(new CHIBundleDAT(params)).fromDataFlit(io.toCcIcn.tx.data.get.bits.asUInt).asUInt
    io.toCcIcn.tx.data.get.ready    := io.fromNHL2.rxdat.ready

    // Unuse
    assert(!io.toCcIcn.tx.req.get.valid)
    io.toCcIcn.tx.req.get.ready     := false.B

}

