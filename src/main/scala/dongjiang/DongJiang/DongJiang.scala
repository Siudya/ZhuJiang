package DONGJIANG

import zhujiang.chi._
import DONGJIANG.CHI._
import DONGJIANG.RNSLAVE._
import DONGJIANG.RNMASTER._
import DONGJIANG.SNMASTER._
import DONGJIANG.SLICE._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import Utils.GenerateVerilog
import Utils.IDConnector._
import Utils.FastArb._
import xijiang.router.{HnIcn, RnIcn, SnIcn}
import zhujiang.HasZJParams

abstract class DJModule(implicit val p: Parameters) extends Module with HasDJParam with HasZJParams
abstract class DJBundle(implicit val p: Parameters) extends Bundle with HasDJParam with HasZJParams


class DongJiang()(implicit p: Parameters) extends DJModule {
/*
 * System Architecture: (3 RNSLAVE, 1 RNMASTER and 2 bank)
 *
 *                                          ----------------------------------------------------------
 *                                  |       |                       |   Dir    |                     |        |
 *               ------------       |       |      -----------      ------------      ---------      |        |       ------------
 *     CSN <---> | RNSLAVE  | <---> | <---> | ---> |  MSHR   | ---> | MainPipe | ---> | Queue | ---> |  <---> | <---> | RNMASTER | <---> CSN
 *               ------------       |       |      -----------      ------------      ---------      |        |       ------------
 *                                  |       |                       |DataBuffer|                     |        |
 *                                  |       ----------------------------------------------------------        |
 *                                  |                                                                         |
 *                                  |                                                                         |
 *                                 XBar                                                                      XBar
 *                                  |                                                                         |
 *                                  |                                                                         |
 *                                  |       ----------------------------------------------------------        |
 *                                  |       |                       |   Dir    |                     |        |
 *               ------------       |       |      -----------      ------------      ---------      |        |       ------------
 *   Local <---> | RNSLAVE  | <---> | <---> | ---> |  MSHR   | ---> | MainPipe | ---> | Queue | ---> |  <---> | <---> | SNMASTER | <---> Local
 *               ------------       |       |      -----------      ------------      ---------      |        |       ------------
 *                                  |       |                       |DataBuffer|                     |        |
 *                                          ----------------------------------------------------------
 *                                                      
 */


/*
 * How To Detemine A Req( Read / Dataless / Atomic / CMO / Write ) Go To Which Master( RNMASTER / SNMASTER ):
 * 1. Use Address Id Bits
 *
 * How To Detemine A Req( Read / Dataless / Atomic / CMO / Write ) Go To Which HN/SN Node:
 * 1. It is only one HN(expect DongJiang) in CSN, its tgtId always be 0
 * 2. Its DCU tgtId is same as bankId in Local Ring; The DDR tgtId is the max value of tgtId
 *
 * How To Detemine A Snoop Go To Which Slave:
 * 1. It will be detemine by SF Result; It may be possible to send Snoop to Slave more than one
 * 2. Snp*Fwd only will be send to one Slave
 *
 */




/*
 * System ID Map Table:
 * [Module]     |  [private ID]             |  [XBar ID]
 *
 * BASE Slice Ctrl Signals:
 * [req2Slice]  |  [hasAddr]                |  from: [idL0]   [idL1]    [idL2]       | to: [idL0]     [idL1]    [idL2]
 * [resp2Node]  |  [hasCHIChnl]             |  from: [idL0]   [idL1]    [idL2]       | to: [idL0]     [idL1]    [idL2]
 * [req2Node]   |  [hasAddr]                |  from: [idL0]   [idL1]    [idL2]       | to: [idL0]     [idL1]    [idL2]
 * [resp2Slice] |  [hasMSHRSet] [HasDBID]   |  from: [idL0]   [idL1]    [idL2]       | to: [idL0]     [idL1]    [idL2]
 *
 *
 * BASE Slice DB Signals:
 * [dbRCReq]    |  [hasMSHRSet] [hasDBID]   |  from: [idL0]   [idL1]    [idL2]       | to: [idL0]     [idL1]    [idL2]
 * [wReq]       |                           |  from: [idL0]   [idL1]    [idL2]       | to: [idL0]     [idL1]    [idL2]
 * [wResp]      |  [hasDBID]                |  from: [SLICE]  [sliceId] [DontCare]   | to: [idL0]     [idL1]    [idL2]
 * [dataFDB]    |                           |  from: None                            | to: [idL0]     [idL1]    [idL2]
 * [dataTDB]    |  [hasDBID]                |  from: None                            | to: [idL0]     [idL1]    [idL2]
 *
 * ****************************************************************************************************************************************************
 *
 * RnSlave <-> Slice Ctrl Signals:
 * [req2Slice]  |  [hasAddr]                |  from: [RNSLV]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [DontCare]
 * [resp2Node]  |  [hasCHIChnl]             |  from: [SLICE]  [sliceId] [mshrWay]    | to: [RNSLV]    [nodeId]  [reqBufId]
 * [req2Node]   |  [hasAddr]                |  from: [SLICE]  [sliceId] [mshrWay]    | to: [RNSLV]    [nodeId]  [DontCare]
 * [resp2Slice] |  [hasMSHRSet] [HasDBID]   |  from: [RNSLV]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [mshrWay]
 *
 *
 * RnSlave <-> Slice DB Signals:
 * [wReq]       |                           |  from: [RNSLV]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [DontCare]
 * [wResp]      |  [hasDBID]                |  from: [SLICE]  [sliceId] [DontCare]   | to: [RNSLV]    [nodeId]  [reqBufId]
 * [dataFDB]    |                           |  from: None                            | to: [RNSLV]    [nodeId]  [reqBufId]
 * [dataTDB]    |  [hasDBID]                |  from: None                            | to: [SLICE]    [sliceId] [DontCare]
 *
 * ****************************************************************************************************************************************************
 *
 * RnMaster <-> Slice Ctrl Signals:
 * [req2Slice]  |  [hasAddr]                |  from: [RNMAS]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [DontCare]
 * [resp2Node]  |  [hasCHIChnl]             |  from: [SLICE]  [sliceId] [mshrWay]    | to: [RNMAS]    [nodeId]  [reqBufId]
 * [req2Node]   |  [hasAddr]                |  from: [SLICE]  [sliceId] [mshrWay]    | to: [RNMAS]    [nodeId]  [DontCare]
 * [resp2Slice] |  [hasMSHRSet] [HasDBID]   |  from: [RNMAS]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [mshrWay]
 *
 *
 * RnMaster <-> Slice DB Signals:
 * [dbRCReq]    |  [hasMSHRSet] [hasDBID]   |  from: [RNMAS]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [mshrWay]    // When Data from DS use mshrId(Cat(Set, way))
 * [wReq]       |                           |  from: [RNMAS]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [DontCare]
 * [wResp]      |  [hasDBID]                |  from: [SLICE]  [sliceId] [DontCare]   | to: [RNMAS]    [nodeId]  [reqBufId]
 * [dataFDB]    |                           |  from: None                            | to: [RNMAS]    [nodeId]  [reqBufId]
 * [dataTDB]    |  [hasDBID]                |  from: None                            | to: [SLICE]    [sliceId] [DontCare]
 *
 * ****************************************************************************************************************************************************
 *
 * Slice <-> SnMaster Ctrl Signals:
 * [req2Node]   |  [hasAddr]                |  from: [SLICE]  [sliceId] [mshrWay]    | to: [SNMAS]    [nodeId]  [DontCare]
 * [resp2Slice] |  [hasMSHRSet] [HasDBID]   |  from: [RNSLV]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [mshrWay]
 *
 *
 * Slice <-> SnMaster DB Signals:
 * [dbRCReq]    |  [hasMSHRSet] [hasDBID]   |  from: [SNMAS]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [mshrWay]   // When Data from DS use mshrId(Cat(Set, way))
 * [wReq]       |                           |  from: [SNMAS]  [nodeId]  [reqBufId]   | to: [SLICE]    [sliceId] [DontCare]
 * [wResp]      |  [hasDBID]                |  from: [SLICE]  [sliceId] [DontCare]   | to: [RNMAS]    [nodeId]  [reqBufId]  // Unuse from
 * [dataFDB]    |                           |  from: None                            | to: [RNMAS]    [nodeId]  [reqBufId]
 * [dataTDB]    |  [hasDBID]                |  from: None                            | to: [SLICE]    [sliceId] [DontCare]
 *
 * ****************************************************************************************************************************************************
 *
 * MainPipe S4 Commit <-> DB Signals:
 * [dbRCReq]    |  [hasMSHRSet] [hasDBID]   |  from: [SLICE]  [sliceId] [DontCare]   | to: [NODE]     [nodeId]  [reqBufId] // Unuse mshrSet
 *
 *
 * MainPipe S4 Commit <-> DS Signals:
 * [dsRWReq]    |  [hasMSHRSet] [hasDBID]   |  from: [SLICE]  [sliceId] [mshrWay]    | to: [NODE]     [nodeId]  [reqBufId]
 *
 *
 * DS <-> DB Signals:
 * [dbRCReq]    |  [hasMSHRSet] [hasDBID]   |  from: [SLICE]  [sliceId] [dsId]       | to: [NODE]     [nodeId]  [reqBufId]  // Unuse mshrSet
 * [wReq]       |                           |  from: [SLICE]  [sliceId] [dsId]       | to: [SLICE]    [sliceId] [DontCare]
 * [wResp]      |  [hasDBID]                |  from: [SLICE]  [sliceId] [DontCare]   | to: [SLICE]    [sliceId] [dsId]      // Unuse from
 * [dataFDB]    |                           |  from: None                            | to: [SLICE]    [sliceId] [dsId]
 * [dataTDB]    |  [hasDBID]                |  from: None                            | to: [SLICE]    [sliceId] [mshrWay]
 *
 */


/*
 * CHI ID Map Table:
 *
 * *********************************************************** RNSLAVE ***************************************************************************
 *
 * tgtNodeID    <-> Get from Slice req
 * nodeID       <-> RnSlave
 * reqBufId     <-> ReqBuf
 * fwdNId       <-> Get from Slice req
 * fwdTxnID     <-> Get from Slice req
 *
 *
 *
 * { Read / Dataless / Atomic / CMO }   TxReq: Store {                    |  SrcID_g = SrcID     |   TxnID_g = TxnID     |                      }
 * { CompAck                        }   TxRsp: Match {                    |                      |   TxnID  == reqBufId  |                      }
 * { CompData                       }   RxDat: Send  { TgtID   = SrcID_g  |  SrcID   = nodeID    |   TxnID   = TxnID_g   |  DBID    = reqBufId  }
 * { Comp                           }   RxRsp: Send  { TgtID   = SrcID_g  |  SrcID   = nodeID    |   TxnID   = TxnID_g   |  DBID    = reqBufId  }
 *
 *
 * { Write                          }   TxReq: Store {                    |  SrcID_g = SrcID     |   TxnID_g = TxnID     |                      }
 * { WriteData                      }   TxDat: Match {                    |                      |   TxnID  == reqBufId  |                      }
 * { CompDBIDResp                   }   RxRsp: Send  { TgtID   = SrcID_g  |  SrcID   = nodeID    |   TxnID   = TxnID_g   |  DBID    = reqBufId  }
 *
 *
 * { SnoopResp                      }   TxRsp: Match {                    |                      |   TxnID  == reqBufId  |                      }
 * { SnoopRespData                  }   TxDat: Match {                    |                      |   TxnID  == reqBufId  |                      }
 * { Snoop                          }   RxSnp: Send  { TgtID  = tgtNodeID |  SrcID   = nodeID    |   TxnID   = reqBufId  |                      }
 *
 *
 * { SnpRespFwded                   }   TxRsp: Match {                    |                      |   TxnID  == reqBufId  |                      }
 * { SnpRespDataFwded               }   TxDat: Match {                    |                      |   TxnID  == reqBufId  |                      }
 * { SnoopFwd                       }   RxSnp: Send  {                    |  SrcID   = nodeID    |   TxnID   = reqBufId  |                      |   FwdNId  = fwdNId    |   FwdTxnID    = fwdTxnID }
 *
 *
 *
 * *********************************************************** RNMASTRE *************************************************************************
 *
 * tgtNodeID    <-> Get from Slice req
 * nodeID       <-> RnMaster
 * reqBufId     <-> ReqBuf
 *
 *
 * { Read / Dataless / Atomic / CMO }   TxReq: Send  { TgtID = tgtNodeID  |  SrcID   = nodeID    |   TxnID   = reqBufId  |                      }
 * { CompAck(When get CompData)     }   TxRsp: Send  { TgtID = HomeNID_g  |  SrcID   = nodeID    |   TxnID   = DBID_g    |                      }
 * { CompAck(When get Comp)         }   TxRsp: Send  { TgtID = SrcID_g    |  SrcID   = nodeID    |   TxnID   = DBID_g    |                      }
 * { CompData                       }   RxDat: M & S {                    |                      |   TxnID  == reqBufId  |  DBID_g  = DBID      |   HomeNID_g   = HomeNID   }
 * { Comp                           }   RxRsp: M & S {                    |  SrcID_g = SrcID     |   TxnID  == reqBufId  |  DBID_g  = DBID      }
 *
 *
 * { Write                          }   TxReq: Send  { TgtID = tgtNodeID  |  SrcID   = nodeID    |   TxnID   = reqBufId  |                      }
 * { WriteData                      }   TxDat: Send  { TgtID = tgtNodeID  |  SrcID   = nodeID    |   TxnID   = DBID_g    |                      }
 * { CompDBIDResp                   }   RxRsp: M & G {                    |                      |   TxnID  == reqBufId  |  DBID_g = DBID       }
 *
 *
 * { SnoopResp                      }   TxRsp: Match { TgtID = SrcID_g    |  SrcID   = nodeID    |   TxnID   = TxnID_g   |                      }
 * { SnoopRespData                  }   TxDat: Send  { TgtID = SrcID_g    |  SrcID   = nodeID    |   TxnID   = TxnID_g   |                      }
 * { Snoop                          }   RxSnp: Store {                    |  SrcID_g = SrcID     |   TxnID_g = TxnID     |                      }
 *
 *
 * { SnpRespFwded                   }   TxRsp: Match { TgtID = SrcID_g    |  SrcID   = nodeID    |   TxnID   = TxnID_g   |                      }
 * { SnpRespDataFwded               }   TxDat: Match { TgtID = SrcID_g    |  SrcID   = nodeID    |   TxnID   = TxnID_g   |                      }
 * { CompData                       }   TxDat: Match { TgtID = FwdNId_g   |  SrcID   = nodeID    |   TxnID   = FwdTxnID  |  DBID = TxnID_g      |   HomeNID     = SrcID_g   }
 * { SnoopFwd                       }   RxSnp: Store {                    |  SrcID_g = SrcID     |   TxnID_g = TxnID     |                      |   FwdNId_g    = FwdNId    |   FwdTxnID_g  = FwdTxnID }
 *
 *
 *
 * *********************************************************** SNMASTRE *************************************************************************
 *
 * nodeID       <-> RnMaster
 * reqBufId     <-> ReqBuf
 *
 * { Read                           }   TxReq: Send  { TgtID = 0          |  SrcID   = nodeID    |   TxnID   = reqBufId  |                      }
 * { CompData                       }   RxDat: Match {                    |                      |   TxnID  == reqBufId  |                      }
 *
 * { Write                          }   TxReq: Send  { TgtID = 0          |  SrcID   = nodeID    |   TxnID   = reqBufId  |                      }
 * { WriteData                      }   TxDat: Send  { TgtID = 0          |  SrcID   = nodeID    |   TxnID   = DBID_g    |                      }
 * { CompDBIDResp                   }   RxRsp: M & G {                    |                      |   TxnID  == reqBufId  |  DBID_g = DBID       }
 *
 */


// ------------------------------------------ IO declaration ----------------------------------------------//
    val io = IO(new Bundle {
        val toLocal        = Flipped(new HnIcn(local = true))
        val toCSNOpt       = if (hasCSNIntf) Some(new Bundle {
            val hn         = Flipped(new HnIcn(local = false))
            val rn         = Flipped(new RnIcn)
        }) else None
    })

// ------------------------------------------ Modules declaration ----------------------------------------------//

    val localRnSlave    = Module(new RnSlave(IdL1.LOCALSLV, djparam.localRnSlaveIntf))
    val localSnMaster   = Module(new SnMaster(IdL1.LOCALMAS, djparam.localSnMasterIntf))
    val csnRnSlaveOpt   = if (hasCSNIntf) Some(Module(new RnSlave(IdL1.CSNSLV, djparam.csnRnSlaveIntf.get))) else None
    val csnRnMasterOpt  = if (hasCSNIntf) Some(Module(new RnMaster(IdL1.CSNMAS, djparam.csnRnMasterIntf.get))) else None
    val intfs           = if (hasCSNIntf) Seq(localRnSlave, localSnMaster, csnRnSlaveOpt.get, csnRnMasterOpt.get)
                          else            Seq(localRnSlave, localSnMaster)

    val xbar            = Module(new Xbar())
    val slices          = Seq.fill(djparam.nrBank) { Module(new Slice()) }
    slices.zipWithIndex.foreach { case(s, i) => s.io.sliceId := i.U }

    // TODO:
    slices.foreach(_.io.valid := true.B)
    xbar.io.bankVal.foreach(_ := true.B)


// ---------------------------------------------- Connection ---------------------------------------------------//

    def connectBundle[T1 <: UInt, T2 <: Bundle](in: DecoupledIO[T1], out: DecoupledIO[T2]): Unit = {
        out.valid   := in.valid
        out.bits    := in.bits.asTypeOf(out.bits)
        in.ready    := out.ready
        require(in.bits.getWidth == out.bits.getWidth)
    }

    def connectUInt[T1 <: Bundle, T2 <: UInt](in: DecoupledIO[T1], out: DecoupledIO[T2]): Unit = {
        out.valid := in.valid
        out.bits := in.bits.asTypeOf(out.bits)
        in.ready := out.ready
        require(in.bits.getWidth == out.bits.getWidth)
    }


    /*
     * Connect LOCAL RING CHI IO
     */
    // tx req
    connectBundle(io.toLocal.tx.req, localRnSlave.io.chi.txreq)
    // tx rsp & dat
    val rspQ = Module(new Queue(new RespFlit(), entries = 2))
    val datQ = Module(new Queue(new DataFlit(), entries = 2))
    connectBundle(io.toLocal.tx.resp, rspQ.io.enq)
    connectBundle(io.toLocal.tx.data, datQ.io.enq)
    // tx rsp
    localSnMaster.io.chi.rxrsp.valid    := rspQ.io.deq.valid & fromSnNode(rspQ.io.deq.bits.SrcID)
    localRnSlave.io.chi.txrsp.valid     := rspQ.io.deq.valid & !fromSnNode(rspQ.io.deq.bits.SrcID)
    localSnMaster.io.chi.rxrsp.bits     := rspQ.io.deq.bits
    localRnSlave.io.chi.txrsp.bits      := rspQ.io.deq.bits
    when(fromSnNode(rspQ.io.deq.bits.SrcID)) { rspQ.io.deq.ready := localSnMaster.io.chi.rxrsp.ready }.otherwise { rspQ.io.deq.ready := localRnSlave.io.chi.txrsp.ready }
    // tx dat
    localSnMaster.io.chi.rxdat.valid    := datQ.io.deq.valid & fromSnNode(datQ.io.deq.bits.SrcID)
    localRnSlave.io.chi.txdat.valid     := datQ.io.deq.valid & !fromSnNode(datQ.io.deq.bits.SrcID)
    localSnMaster.io.chi.rxdat.bits     := datQ.io.deq.bits
    localRnSlave.io.chi.txdat.bits      := datQ.io.deq.bits
    when(fromSnNode(datQ.io.deq.bits.SrcID)) { datQ.io.deq.ready := localSnMaster.io.chi.rxdat.ready }.otherwise { datQ.io.deq.ready := localRnSlave.io.chi.txdat.ready }
    // rx ereq & snp
    connectUInt(localSnMaster.io.chi.txreq, io.toLocal.rx.ereq.get)
    connectUInt(localRnSlave.io.chi.rxsnp, io.toLocal.rx.snoop)
    localSnMaster.io.chi.rxsnp <> DontCare
    // rx rsp & dat
    connectUInt(fastArbDec(Seq(localSnMaster.io.chi.txrsp, localRnSlave.io.chi.rxrsp)), io.toLocal.rx.resp)
    connectUInt(fastArbDec(Seq(localSnMaster.io.chi.txdat, localRnSlave.io.chi.rxdat)), io.toLocal.rx.data)

    /*
     * Connect CSN CHI IO
     */
    if(hasCSNIntf) {
        // tx
        connectBundle(io.toCSNOpt.get.hn.tx.req, csnRnSlaveOpt.get.io.chi.txreq)
        connectBundle(io.toCSNOpt.get.hn.tx.resp, csnRnSlaveOpt.get.io.chi.txrsp)
        connectBundle(io.toCSNOpt.get.hn.tx.data, csnRnSlaveOpt.get.io.chi.txdat)
        // rx
        connectUInt(csnRnSlaveOpt.get.io.chi.rxsnp, io.toCSNOpt.get.hn.rx.snoop)
        connectUInt(csnRnSlaveOpt.get.io.chi.rxrsp, io.toCSNOpt.get.hn.rx.resp)
        connectUInt(csnRnSlaveOpt.get.io.chi.rxdat, io.toCSNOpt.get.hn.rx.data)
        // tx
        connectUInt(csnRnMasterOpt.get.io.chi.txreq, io.toCSNOpt.get.rn.rx.req)
        connectUInt(csnRnMasterOpt.get.io.chi.txrsp, io.toCSNOpt.get.rn.rx.resp)
        connectUInt(csnRnMasterOpt.get.io.chi.txdat, io.toCSNOpt.get.rn.rx.data)
        // rx
        connectBundle(io.toCSNOpt.get.rn.tx.snoop, csnRnMasterOpt.get.io.chi.rxsnp)
        connectBundle(io.toCSNOpt.get.rn.tx.resp, csnRnMasterOpt.get.io.chi.rxrsp)
        connectBundle(io.toCSNOpt.get.rn.tx.data, csnRnMasterOpt.get.io.chi.rxdat)
    }

    /*
     * Connect RNs <-> Xbar
     */

    intfs.zipWithIndex.foreach {
        case(intf, i) =>
            // slice ctrl signals
            if (intf.io.req2SliceOpt.nonEmpty) {
                xbar.io.req2Slice.in(i)             <> intf.io.req2SliceOpt.get
            } else {
                xbar.io.req2Slice.in(i)             <> DontCare
            }
            if (intf.io.resp2NodeOpt.nonEmpty) {
                xbar.io.resp2Node.out(i)            <> intf.io.resp2NodeOpt.get
            } else {
                xbar.io.resp2Node.out(i)            <> DontCare
            }
            xbar.io.req2Node.out(i)                 <> intf.io.req2Node
            xbar.io.resp2Slice.in(i)                <> intf.io.resp2Slice
            // slice DataBuffer signals
            if (intf.io.dbSigs.dbRCReqOpt.nonEmpty) {
                xbar.io.dbSigs.in(i).dbRCReqOpt.get <> intf.io.dbSigs.dbRCReqOpt.get
            } else {
                xbar.io.dbSigs.in(i).dbRCReqOpt.get <> DontCare
            }
            xbar.io.dbSigs.in(i).wReq               <> intf.io.dbSigs.wReq
            xbar.io.dbSigs.in(i).wResp              <> intf.io.dbSigs.wResp
            xbar.io.dbSigs.in(i).dataFDB            <> intf.io.dbSigs.dataFDB
            xbar.io.dbSigs.in(i).dataTDB            <> intf.io.dbSigs.dataTDB
    }

    /*
     * Connect Slice <-> Xbar
     */
    slices.zipWithIndex.foreach {
        case (slice, i) =>
            // slice ctrl signals
            xbar.io.req2Slice.out(i)    <> slice.io.req2Slice
            xbar.io.resp2Node.in(i)     <> slice.io.resp2Node
            xbar.io.req2Node.in(i)      <> slice.io.req2Node
            xbar.io.resp2Slice.out(i)   <> slice.io.resp2Slice
            // slice DataBuffer signals
            xbar.io.dbSigs.out(i)       <> slice.io.nodeDBSigs
    }
}
