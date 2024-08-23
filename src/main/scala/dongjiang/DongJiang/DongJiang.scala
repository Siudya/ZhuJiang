package DONGJIANG

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

abstract class DJModule(implicit val p: Parameters) extends Module with HasDJParam
abstract class DJBundle(implicit val p: Parameters) extends Bundle with HasDJParam


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
        val rnSlvChi            = Vec(nrRnSlv, Flipped(CHIBundleDecoupled(chiParams)))
        val rnMasChi            = Vec(nrRnMas, CHIBundleDecoupled(chiParams))
        val snMasChi            = Vec(nrSnSlv, CHIBundleDecoupled(chiParams))
    })

    io <> DontCare

// ------------------------------------------ Modules declaration ----------------------------------------------//
    def createRnSlv(id: Int) = { val rnSlv = Module(new RnSlave(id, rnSlvParamSeq(id))); rnSlv }
    def createRnMas(id: Int) = { val rnMas = Module(new RnMaster(id, rnMasParamSeq(id))); rnMas }
    def createSnMas(id: Int) = { val snMas = Module(new SnMaster(id, snMasParamSeq(id))); snMas }

    val rnSlaves    = (0 until nrRnSlv).map(i => createRnSlv(i))
    val rnMasters   = (0 until nrRnMas).map(i => createRnMas(i))
    val snMasters   = (0 until nrSnSlv).map(i => createSnMas(i))

    val rnNodes     = rnSlaves ++ rnMasters

//    val xbar        = Module(new RN2SliceXbar())
    val slices      = Seq.fill(djparam.nrBank) { Module(new Slice()) }

    // TODO:
    rnSlaves.foreach(_.io <> DontCare)
    rnMasters.foreach(_.io <> DontCare)
    snMasters.foreach(_.io <> DontCare)
    slices.foreach(_.io <> DontCare)
//    xbar.io <> DontCare

    slices.foreach(_.io.valid := true.B)
//    xbar.io.bankVal.foreach(_ := true.B)


// ---------------------------------------------- Connection ---------------------------------------------------//
    /*
     * Connect IO CHI
     */
    rnSlaves.map(_.io.chi).zip(io.rnSlvChi).foreach { case(a, b) => a <> b }

    rnMasters.map(_.io.chi).zip(io.rnMasChi).foreach { case (a, b) => a <> b }

    snMasters.map(_.io.chi).zip(io.snMasChi).foreach { case (a, b) => a <> b }

//    /*
//     * Connect RNs <-> Xbar
//     */
//    rnNodes.zipWithIndex.foreach {
//        case(rn, i) =>
//            // slice ctrl signals
//            if(djparam.rnNodeMes(i).hasReq2Slice) {
//                xbar.io.req2Slice.in(i)             <> rn.io.req2SliceOpt.get
//                xbar.io.resp2Node.out(i)            <> rn.io.resp2NodeOpt.get
//            } else {
//                xbar.io.req2Slice.in(i)             <> DontCare
//                xbar.io.resp2Node.out(i)            <> DontCare
//            }
//            xbar.io.req2Node.out(i)                 <> rn.io.req2Node
//            xbar.io.resp2Slice.in(i)                <> rn.io.resp2Slice
//            // slice DataBuffer signals
//            if (djparam.rnNodeMes(i).hasDBRCReq) {
//                xbar.io.dbSigs.in(i).dbRCReqOpt.get <> rn.io.dbSigs.dbRCReqOpt.get
//            } else {
//                xbar.io.dbSigs.in(i).dbRCReqOpt.get <> DontCare
//            }
//            xbar.io.dbSigs.in(i).wReq               <> rn.io.dbSigs.wReq
//            xbar.io.dbSigs.in(i).wResp              <> rn.io.dbSigs.wResp
//            xbar.io.dbSigs.in(i).dataFDB            <> rn.io.dbSigs.dataFDB
//            xbar.io.dbSigs.in(i).dataTDB            <> rn.io.dbSigs.dataTDB
//    }
//
//    /*
//     * Seq Slice Id Value
//     */
//    slices.zipWithIndex.foreach { case(s, i) => s.io.sliceId := i.U }
//
//
//    /*
//     * Connect Slice <-> Xbar
//     */
//    slices.zipWithIndex.foreach {
//        case (slice, i) =>
//            // slice ctrl signals
//            xbar.io.req2Slice.out(i)    <> slice.io.rnReq2Slice
//            xbar.io.resp2Node.in(i)     <> slice.io.resp2RnNode
//            xbar.io.req2Node.in(i)      <> slice.io.req2RnNode
//            xbar.io.resp2Slice.out(i)   <> slice.io.rnResp2Slice
//            // slice DataBuffer signals
//            xbar.io.dbSigs.out(i)       <> slice.io.rnDBSigs
//    }
//
//    /*
//     * Connect Slice <-> SnMaster
//     */
//    slices.zipWithIndex.foreach {
//        case (slice, i) =>
//            // slice ctrl signals
//            snMasters(i).io.req2Node    <> slice.io.req2SnNode
//            snMasters(i).io.resp2Slice  <> slice.io.snResp2Slice
//            // slice DataBuffer signals
//            snMasters(i).io.dbSigs      <> slice.io.snDBSigs
//    }

}
