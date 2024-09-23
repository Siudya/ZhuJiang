package DONGJIANG.DECODE

import DONGJIANG._
import DONGJIANG.DECODE.InstBundle
import DONGJIANG.DECODE.RespType._
import DONGJIANG.ChipType._
import DONGJIANG.DECODE.Inst._
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
    LocalReqInst(ReadNotSharedDirty, I, I,   I) -> (ReadDown | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadNotSharedDirty, I, UC,  I) -> (Snoop    | SnpOp(SnpNotSharedDirty) | retToSrc),
    LocalReqInst(ReadNotSharedDirty, I, UD,  I) -> (Snoop    | SnpOp(SnpNotSharedDirty) | retToSrc),
    LocalReqInst(ReadNotSharedDirty, I, SC,  I) -> (Snoop    | SnpOp(SnpNotSharedDirty) | retToSrc),
    LocalReqInst(ReadNotSharedDirty, I, I,  UC) -> (ReadDCU  | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadNotSharedDirty, I, I,  UD) -> (ReadDCU  | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadNotSharedDirty, I, I,  SC) -> (ReadDCU  | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadNotSharedDirty, I, SC, SC) -> (ReadDCU  | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadNotSharedDirty, I, I,  SD) -> (ReadDCU  | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadNotSharedDirty, I, SC, SD) -> (ReadDCU  | ReadOp(ReadNoSnp)),

    // ----------------------------------------------------------- LOCAL RESP ----------------------------------------------//
    // TODO: Consider a variation of the SC/SD mapping as UC/SD In Local
    //  I  I  I
    LocalRespInst(ReadNotSharedDirty,  I,  I,  I, RD,  HasData, rd = ChiResp.UC)      -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    //  I UC  I
    LocalRespInst(ReadNotSharedDirty,  I, UC,  I, Snp, HasData, snp = ChiResp.I)      -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    LocalRespInst(ReadNotSharedDirty,  I, UC,  I, Snp, HasData, snp = ChiResp.I_PD)   -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(I)  | SrcState(UD) | OthState(I)),
    LocalRespInst(ReadNotSharedDirty,  I, UC,  I, Snp, HasData, snp = ChiResp.SC)     -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(I)  | SrcState(SC) | OthState(SC)),
    LocalRespInst(ReadNotSharedDirty,  I, UC,  I, Snp, HasData, snp = ChiResp.SC_PD)  -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SD) | SrcState(SC) | OthState(SC) | WriteDCU | WriOp(WriteNoSnpFull)),
    //  I UD  I
    LocalRespInst(ReadNotSharedDirty,  I, UD,  I, Snp, HasData, snp = ChiResp.I_PD)   -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(I)  | SrcState(UD) | OthState(I)),
    LocalRespInst(ReadNotSharedDirty,  I, UD,  I, Snp, HasData, snp = ChiResp.SC_PD)  -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SD) | SrcState(SC) | OthState(SC) | WriteDCU | WriOp(WriteNoSnpFull)),
    //  I SC  I
    LocalRespInst(ReadNotSharedDirty,  I, SC,  I, Snp, HasData, snp = ChiResp.I)      -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(I)  | SrcState(SC) | OthState(I)),
    LocalRespInst(ReadNotSharedDirty,  I, SC,  I, Snp, HasData, snp = ChiResp.SC)     -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(I)  | SrcState(SC) | OthState(SC)),
    //  I  I UC
    LocalRespInst(ReadNotSharedDirty,  I,  I, UC, RD,  HasData, rd = ChiResp.UC)      -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    //  I  I UD
    LocalRespInst(ReadNotSharedDirty,  I,  I, UD, RD,  HasData, rd = ChiResp.UD)      -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(I)  | SrcState(UD) | OthState(I)),
    //  I  I SC
    LocalRespInst(ReadNotSharedDirty,  I,  I, SC, RD,  HasData, rd = ChiResp.SC)      -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SC) | SrcState(SC) | OthState(I)),
    //  I SC SC
    LocalRespInst(ReadNotSharedDirty,  I, SC, SC, RD,  HasData, rd = ChiResp.SC)      -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SC) | SrcState(SC) | OthState(SC)),
    //  I  I SD
    LocalRespInst(ReadNotSharedDirty,  I,  I, SD, RD,  HasData, rd = ChiResp.SD)      -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SD) | SrcState(SC) | OthState(I)),
    //  I SC SD
    LocalRespInst(ReadNotSharedDirty,  I,  I, SD, RD,  HasData, rd = ChiResp.SD)      -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.SC)    | HnState(SD) | SrcState(SC) | OthState(SC)),
  )


  def readUnique: Seq[(UInt, UInt)] = Seq(
    // ----------------------------------------------------------- LOCAL REQ ----------------------------------------------//
    LocalReqInst(ReadUnique, I, I,   I) -> (ReadDown | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadUnique, I, UC,  I) -> (Snoop    | SnpOp(SnpUnique) | retToSrc),
    LocalReqInst(ReadUnique, I, UD,  I) -> (Snoop    | SnpOp(SnpUnique) | retToSrc),
    LocalReqInst(ReadUnique, I, SC,  I) -> (Snoop    | SnpOp(SnpUnique) | retToSrc),
    LocalReqInst(ReadUnique, I, I,  UC) -> (ReadDCU  | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadUnique, I, I,  UD) -> (ReadDCU  | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadUnique, I, I,  SC) -> (ReadDCU  | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadUnique, I, SC, SC) -> (ReadDCU  | ReadOp(ReadNoSnp) | SnpOp(SnpUnique)),
    LocalReqInst(ReadUnique, I, I,  SD) -> (ReadDCU  | ReadOp(ReadNoSnp)),
    LocalReqInst(ReadUnique, I, SC, SD) -> (ReadDCU  | ReadOp(ReadNoSnp) | SnpOp(SnpUnique)),

    // ----------------------------------------------------------- LOCAL RESP ----------------------------------------------//
    // TODO: Consider a variation of the SC/SD mapping as UC/SD In Local
    //  I  I  I
    LocalRespInst(ReadUnique,  I,  I,  I, RD,     HasData, rd = ChiResp.UC)                   -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    //  I UC  I
    LocalRespInst(ReadUnique,  I, UC,  I, Snp,    HasData, snp = ChiResp.I)                   -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    LocalRespInst(ReadUnique,  I, UC,  I, Snp,    HasData, snp = ChiResp.I_PD)                -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(I)  | SrcState(UD) | OthState(I)),
    //  I UD  I
    LocalRespInst(ReadUnique,  I, UD,  I, Snp,    HasData, snp = ChiResp.I_PD)                -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(I)  | SrcState(UD) | OthState(I)),
    //  I SC  I
    LocalRespInst(ReadUnique,  I, SC,  I, Snp,    HasData, snp = ChiResp.I)                   -> (Commit | RDB2Src | CleanDB | wSFDir |         RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    //  I  I UC
    LocalRespInst(ReadUnique,  I,  I, UC, RD,     HasData, rd = ChiResp.UC)                   -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    //  I  I UD
    LocalRespInst(ReadUnique,  I,  I, UD, RD,     HasData, rd = ChiResp.UD)                   -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(I)  | SrcState(UD) | OthState(I)),
    //  I  I SC
    LocalRespInst(ReadUnique,  I,  I, SC, RD,     HasData, rd = ChiResp.SC)                   -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    //  I SC SC
    LocalRespInst(ReadUnique,  I, SC, SC, Snp_RD, HasData, snp = ChiResp.I, rd = ChiResp.SC)  -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UC)    | HnState(I)  | SrcState(UC) | OthState(I)),
    //  I  I SD
    LocalRespInst(ReadUnique,  I,  I, SD, RD,     HasData, rd = ChiResp.SD)                   -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(UD) | SrcState(I)  | OthState(I)),
    //  I SC SD
    LocalRespInst(ReadUnique,  I,  I, SD, Snp_RD, HasData, snp = ChiResp.I, rd = ChiResp.SD)  -> (Commit | RDB2Src | CleanDB | wSFDir | WSDir | RespOp(CompData) | RespChnl(DAT) | Resp(ChiResp.UD_PD) | HnState(UD) | SrcState(I)  | OthState(I)),
  )


  def table: Seq[(UInt, UInt)] = readNotSharedDirty ++ readUnique
}