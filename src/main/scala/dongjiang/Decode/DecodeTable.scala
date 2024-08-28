package DONGJIANG.DECODE

import DONGJIANG._
import DONGJIANG.DECODE.RespType._
import DONGJIANG.DECODE.Code._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIOp.REQ._
import DONGJIANG.CHI.CHIOp.RSP._
import DONGJIANG.CHI.CHIOp.DAT._
import DONGJIANG.CHI.CHIOp.SNP._
import DONGJIANG.CHI.ChiState._
import chisel3._
import chisel3.util._

/*
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

object ReadDecode {
  def readNotSharedDirty: Seq[(UInt, UInt)] = Seq(
    Cat(ReadNotSharedDirty, I, I , I, NotResp, ChiResp.I, ChiResp.I ,ChiResp.I) -> (ReadDown | RDOp(ReadNoSnp)) ,
    Cat(ReadNotSharedDirty, I, I , UC, NotResp, ChiResp.I, ChiResp.I ,ChiResp.I) -> 0.U,
  )

  def table: Seq[(UInt, UInt)] = readNotSharedDirty
}