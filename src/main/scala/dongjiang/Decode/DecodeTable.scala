package DONGJIANG.DECODE

import DONGJIANG._
import DONGJIANG.DECODE.InstBundle
import DONGJIANG.DECODE.RespType._
import DONGJIANG.ChipType._
import DONGJIANG.DECODE.Code._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIChannel._
import DONGJIANG.CHI.CHIOp.REQ._
import DONGJIANG.CHI.CHIOp.RSP._
import DONGJIANG.CHI.CHIOp.DAT._
import DONGJIANG.CHI.CHIOp.SNP._
import DONGJIANG.CHI.ChiState._
import chisel3._
import chisel3.util._

/*
 * TODO: Update It
 * {Read / Dataless / Atomic / CMO} Request Processing Flow 0 (longest path):
 *                                                                  GetChiTxReq -> RecordInReqBuf -> IssueReq -> [Retry] -> [IssueReq] -> AllocMSHR -> ReadDir -> WaitDirResp -> Decode0 -> MpUpdateMSHR -> Process([Snoop] / [ReadDown] / [ReqRetry])
 *                                                                  |-------------------------- RnSlave -----------------------------|    |-- S0 --|  |-- S1 --|  |--- S2 ---|  |------------------------------- S3 ---------------------------------|
 *                                                                              -> [SnpCtlUpateMSHR] / [ReadCtlUpdateMSHR] -> [IssueResp] -> [Retry] -> [IssueResp] -> [ReadDir] -> [WaitDirResp] -> [Decode1] -> Process([SnpHlp] / [Replace] / [ReqRetry])
 *                                                                                 |----------------------------------- MSHRCtl ----------------------------------|    |-- S1 --|   |---------- S2 ----------|   |-------------------- S3 ----------------|
 *                                                                              -> Commit([ReadDB] / [WSDir] / [WCDir] / [ReadDS] / [WriteDS] / [Atomic]) -> MpUpdateMSHR
 *                                                                                 |----------------------------------------- S4 ---------------------------------------|
 *                                                                              -> UpdateReqBuf -> SendChiRxRsp/Dat -> [GetAck] -> RnUpdateMSHR -> MSHRRelease
 *                                                                                 |---------------------------- RnSlave ---------------------|   |- MSHRCtl -|
 *
 *
 * {Read / Dataless / Atomic / CMO} Request Processing Flow 1 (shortest path):
 *                                                                  GetChiTxReq -> RecordInReqBuf -> IssueReq -> AllocMSHR -> ReadDir -> WaitDirResp -> Decode0 -> MpUpdateMSHR -> Commit([WSDir] / [WCDir] / [ReadDS] / [Atomic]) -> UpdateReqBuf -> SendChiRxRsp/Dat -> RnUpdateMSHR -> MSHRRelease
 *                                                                  |-------------- RnSlave -----------------|   |-- S0 --|  |-- S1 --|  |--- S2 ---|   |-------- S3 ---------|    |-------------------- S4 ---------------------|    |------------------------- RnSlave -------------|   |- MSHRCtl -|
 *
 *
 * {Write} Request Processing Flow 0 (longest path):
 *                                                                  GetChiTxReq -> RecordInReqBuf -> GetDBID -> SendDBIDResp -> GetChiTxDat-> WBDataToDB-> IssueReq -> [Retry] -> [IssueReq] -> AllocMSHR -> ReadDir -> WaitDirResp -> Decode0 -> MpUpdateMSHR -> Process([Replace])
 *                                                                  |------------------------------------------------------------------- RnSlave ------------------------------------------|    |-- S0 --|  |-- S1 --|  |--- S2 ---|  |-------------------- S3 --------------------|
 *                                                                              -> Commit([WSDir] / [WCDir] / [ReadDS] / [WriteDS]) -> MpUpdateMSHR -> MSHRRelease
 *                                                                                 |------------------------------ S4 -----------------------------|  |- MSHRCtl -|
 *
 *
 * {SnpHlp} Request Processing Flow 0 (longest path):
 *                                                                  AllocMSHR -> [SnpCtlUpateMSHR] -> MpUpdateMSHR
 *                                                                  |-- S0 --|   |------------ MSHRCtl ----------|
 *
 * {Replace} Request Processing Flow 0 (longest path):
 *                                                                  AllocMSHR -> [ReadCtlUpdateMSHR] -> MpUpdateMSHR
 *                                                                  |-- S0 --|   |------------- MSHRCtl -----------|
 *
 *
 * decoder table: [opcode, srcState, srcHit, othState, othHit, hnState, hnHit] -> [Process Operations] + [srcNS, othNS, hnNS]
 *
 */



/*
 * When it need Snoop or ReadDown, it need to decode twice, and the result is based on the second decode
 * It cant be Commit and Snoop / ReadDown at the same time
 */



object LocalReadDecode {
  def readNotSharedDirty: Seq[(UInt, UInt)] = Seq(
    // ----------------------------------------------------------- LOCAL REQ ----------------------------------------------//
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I,  I, NOTRESP) -> (ReadDown | RDOp(ReadNoSnp)),
    Cat(Local, REQ, ReadNotSharedDirty,  I, UC,  I, NOTRESP) -> (Snoop    | SnpOp(SnpNotSharedDirty) | retToSrc),
    Cat(Local, REQ, ReadNotSharedDirty,  I, UD,  I, NOTRESP) -> (Snoop    | SnpOp(SnpNotSharedDirty) | retToSrc),
    Cat(Local, REQ, ReadNotSharedDirty,  I, SC,  I, NOTRESP) -> (Snoop    | SnpOp(SnpNotSharedDirty) | retToSrc),
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I, UC, NOTRESP) -> (ReadDCU  | RDOp(ReadNoSnp)),
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I, UD, NOTRESP) -> (ReadDCU  | RDOp(ReadNoSnp)),
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I, SC, NOTRESP) -> (ReadDCU  | RDOp(ReadNoSnp)),
    Cat(Local, REQ, ReadNotSharedDirty,  I, SC, SC, NOTRESP) -> (ReadDCU  | RDOp(ReadNoSnp)),
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I, SD, NOTRESP) -> (ReadDCU  | RDOp(ReadNoSnp)),
    Cat(Local, REQ, ReadNotSharedDirty,  I, SC, SD, NOTRESP) -> (ReadDCU  | RDOp(ReadNoSnp)),
    // ----------------------------------------------------------- LOCAL RESP ----------------------------------------------//
    // TODO: Consider a variation of the SC/SD mapping as UC/SD In Local
    //  I  I  I
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I,  I, RD,  RespHasData, ChiResp.I,     ChiResp.I, ChiResp.UC) -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    //  I UC  I
    Cat(Local, REQ, ReadNotSharedDirty,  I, UC,  I, Snp, RespHasData, ChiResp.I,     ChiResp.I, ChiResp.I)  -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    Cat(Local, REQ, ReadNotSharedDirty,  I, UC,  I, Snp, RespHasData, ChiResp.I_PD,  ChiResp.I, ChiResp.I)  -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(I)  | SrcState(UD) | OthState(I)),
    Cat(Local, REQ, ReadNotSharedDirty,  I, UC,  I, Snp, RespHasData, ChiResp.SC,    ChiResp.I, ChiResp.I)  -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(I)  | SrcState(SC) | OthState(SC)),
    Cat(Local, REQ, ReadNotSharedDirty,  I, UC,  I, Snp, RespHasData, ChiResp.SC_PD, ChiResp.I, ChiResp.I)  -> (Commit | RDB2Src | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SD) | SrcState(SC) | OthState(SC) | WriteDCU | WDOp(WriteNoSnpFull)),
    //  I UD  I
    Cat(Local, REQ, ReadNotSharedDirty,  I, UD,  I, Snp, RespHasData, ChiResp.I_PD,  ChiResp.I, ChiResp.I)  -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(I)  | SrcState(UD) | OthState(I)),
    Cat(Local, REQ, ReadNotSharedDirty,  I, UD,  I, Snp, RespHasData, ChiResp.SC_PD, ChiResp.I, ChiResp.I)  -> (Commit | RDB2Src | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SD) | SrcState(SC) | OthState(SC) | WriteDCU | WDOp(WriteNoSnpFull)),
    //  I SC  I
    Cat(Local, REQ, ReadNotSharedDirty,  I, SC,  I, Snp, RespHasData, ChiResp.I,     ChiResp.I, ChiResp.I)  -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(I)  | SrcState(SC) | OthState(I)),
    Cat(Local, REQ, ReadNotSharedDirty,  I, SC,  I, Snp, RespHasData, ChiResp.SC,    ChiResp.I, ChiResp.I)  -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(I)  | SrcState(SC) | OthState(SC)),
    //  I  I UC
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I, UC, RD,  RespHasData, ChiResp.I,     ChiResp.I, ChiResp.UC) -> (Commit | RDB2Src | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    //  I  I UD
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I, UD, RD,  RespHasData, ChiResp.I,     ChiResp.I, ChiResp.UD) -> (Commit | RDB2Src | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(I)  | SrcState(UD) | OthState(I)),
    //  I  I SC
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I, SC, RD,  RespHasData, ChiResp.I,     ChiResp.I, ChiResp.SC) -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SC) | SrcState(SC) | OthState(I)),
    //  I SC SC
    Cat(Local, REQ, ReadNotSharedDirty,  I, SC, SC, RD,  RespHasData, ChiResp.I,     ChiResp.I, ChiResp.SC) -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SC) | SrcState(SC) | OthState(SC)),
    //  I  I SD
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I, SD, RD,  RespHasData, ChiResp.I,     ChiResp.I, ChiResp.SC) -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SD) | SrcState(SC) | OthState(I)),
    //  I SC SD
    Cat(Local, REQ, ReadNotSharedDirty,  I,  I, SD, RD,  RespHasData, ChiResp.I,     ChiResp.I, ChiResp.SC) -> (Commit | RDB2Src | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SD) | SrcState(SC) | OthState(SC)),
  )

  def table: Seq[(UInt, UInt)] = readNotSharedDirty
}