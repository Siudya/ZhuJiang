package DONGJIANG

import zhujiang.chi._
import DONGJIANG.CHI._
import DONGJIANG.PCU._
import DONGJIANG.CPU._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import Utils.FastArb._
import xijiang.router.{HnfIcn, RnIcn}
import zhujiang.HasZJParams

abstract class DJModule(implicit val p: Parameters) extends Module with HasDJParam
abstract class DJBundle(implicit val p: Parameters) extends Bundle with HasDJParam


class DongJiang()(implicit p: Parameters) extends DJModule {
/*
 * System Architecture: (2 RNSLAVE, 1 RNMASTER, 1 SNMASTER and 2 Slice)
 *
 *                                          ----------------------------------------------------------
 *                                          |                       |   Dir    |                     |
 *               ------------               |      -----------      ------------      ---------      |                ------------
 *     CSN <---> | RNSLAVE  | <---> | <---> | ---> |  MSHR   | ---> | MainPipe | ---> | Queue | ---> |  <---> | <---> | RNMASTER | <---> CSN
 *               ------------       |       |      -----------      ------------      ---------      |        |       ------------
 *                                  |       |                            |                           |        |
 *                                  |       ----------------------------------------------------------        |
 *                                  |                                    |                                    |
 *                                  |                              --------------                             |
 *                                 XBar <------------------------> | DataBuffer | <------------------------> XBar
 *                                  |                              --------------                             |
 *                                  |                                    |                                    |
 *                                  |       ----------------------------------------------------------        |
 *                                  |       |                            |                           |        |
 *               ------------       |       |      -----------      ------------      ---------      |        |       ------------
 *   Local <---> | RNSLAVE  | <---> | <---> | ---> |  MSHR   | ---> | MainPipe | ---> | Queue | ---> |  <---> | <---> | SNMASTER | <---> Local
 *               ------------               |      -----------      ------------      ---------      |                ------------
 *                                          |                       |   Dir    |                     |
 *                                          ----------------------------------------------------------
 *                                                      
 */


/*
 * How To Detemine A Req( Read / Dataless / Atomic / CMO / Write ) Go To Which Master( RNMASTER / SNMASTER ):
 * 1. Use CoreID
 *
 * How To Detemine A Req( Read / Dataless / Atomic / CMO / Write ) Go To HN(In CSN) or SN(In LocalRing) Node:
 * 1. Use CoreID
 *
 * How To Detemine A Snoop Go To Which Slave:
 * 1. It will be detemine by SF Result; It may be possible to send Snoop to Slave more than one
 * 2. Snp*Fwd only will be send to one Slave
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
        val toLocal        = Flipped(new HnfIcn(local = true, node = localHnfNode))
        val toCSNOpt       = if (hasCSNIntf) Some(new Bundle {
            val hn         = Flipped(new HnfIcn(local = false, node = csnHnfNodeOpt.get))
            val rn         = Flipped(new RnIcn(node = csnRnfNodeOpt.get))
        }) else None
    })

// ------------------------------------------ Modules declaration ----------------------------------------------//

    val localRnSlave    = Module(new RnSlavePCU(IncoID.LOCALSLV, djparam.localRnSlaveIntf))
    val localSnMaster   = Module(new SnMasterPCU(IncoID.LOCALMAS, djparam.localSnMasterIntf))
    val csnRnSlaveOpt   = if (hasCSNIntf) Some(Module(new RnSlavePCU(IncoID.CSNSLV, djparam.csnRnSlaveIntf.get))) else None
    val csnRnMasterOpt  = if (hasCSNIntf) Some(Module(new RnMasterPCU(IncoID.CSNMAS, djparam.csnRnMasterIntf.get))) else None
    val intfs           = if (hasCSNIntf) Seq(localRnSlave, localSnMaster, csnRnSlaveOpt.get, csnRnMasterOpt.get)
                          else            Seq(localRnSlave, localSnMaster)
    val databuffer      = Module(new DataBuffer())

    val xbar            = Module(new Xbar())
    val slices          = Seq.fill(djparam.nrBank) { Module(new SliceWrapper()) }
    slices.zipWithIndex.foreach { case(s, i) => s.io.sliceId := i.U }

    // TODO:
    slices.foreach(_.io.valid := true.B)
    xbar.io.bankVal.foreach(_ := true.B)


// ---------------------------------------------- Connection ---------------------------------------------------//
    /*
     * Connect LOCAL RING CHI IO
     */
    // tx req
    io.toLocal.tx.req   <> localRnSlave.io.chi.txreq
    // tx rsp & dat
    val rspQ = Module(new Queue(new RespFlit(), entries = 2))
    val datQ = Module(new Queue(new DataFlit(), entries = 2))
    io.toLocal.tx.resp  <> rspQ.io.enq
    io.toLocal.tx.data  <> datQ.io.enq
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
    localSnMaster.io.chi.txreq          <> io.toLocal.rx.req.get
    localRnSlave.io.chi.rxsnp           <> io.toLocal.rx.snoop
    localSnMaster.io.chi.rxsnp          <> DontCare
    // rx rsp & dat
    fastArbDec(Seq(localSnMaster.io.chi.txrsp, localRnSlave.io.chi.rxrsp)) <> io.toLocal.rx.resp
    fastArbDec(Seq(localSnMaster.io.chi.txdat, localRnSlave.io.chi.rxdat)) <> io.toLocal.rx.data

    /*
     * Connect CSN CHI IO
     */
    if(hasCSNIntf) {
        // tx
        io.toCSNOpt.get.hn.tx.req <> csnRnSlaveOpt.get.io.chi.txreq
        io.toCSNOpt.get.hn.tx.resp <> csnRnSlaveOpt.get.io.chi.txrsp
        io.toCSNOpt.get.hn.tx.data <> csnRnSlaveOpt.get.io.chi.txdat
        // rx
        csnRnSlaveOpt.get.io.chi.rxsnp <> io.toCSNOpt.get.hn.rx.snoop
        csnRnSlaveOpt.get.io.chi.rxrsp <> io.toCSNOpt.get.hn.rx.resp
        csnRnSlaveOpt.get.io.chi.rxdat <> io.toCSNOpt.get.hn.rx.data
        // tx
        csnRnMasterOpt.get.io.chi.txreq <> io.toCSNOpt.get.rn.rx.req
        csnRnMasterOpt.get.io.chi.txrsp <> io.toCSNOpt.get.rn.rx.resp
        csnRnMasterOpt.get.io.chi.txdat <> io.toCSNOpt.get.rn.rx.data
        // rx
        io.toCSNOpt.get.rn.tx.snoop <> csnRnMasterOpt.get.io.chi.rxsnp
        io.toCSNOpt.get.rn.tx.resp <> csnRnMasterOpt.get.io.chi.rxrsp
        io.toCSNOpt.get.rn.tx.data <> csnRnMasterOpt.get.io.chi.rxdat
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
                xbar.io.dbSigs.in0(i)               <> intf.io.dbSigs.dbRCReqOpt.get
            } else {
                xbar.io.dbSigs.in0(i)               <> DontCare
            }
            xbar.io.dbSigs.in1(i).wReq              <> intf.io.dbSigs.wReq
            xbar.io.dbSigs.in1(i).wResp             <> intf.io.dbSigs.wResp
            xbar.io.dbSigs.in1(i).dataFDB           <> intf.io.dbSigs.dataFDB
            xbar.io.dbSigs.in1(i).dataTDB           <> intf.io.dbSigs.dataTDB
    }

    /*
     * Connect Slice <-> Xbar
     */
    slices.zipWithIndex.foreach {
        case (slice, i) =>
            // slice ctrl signals
            xbar.io.req2Slice.out(i)        <> slice.io.req2Slice
            xbar.io.resp2Node.in(i)         <> slice.io.resp2Node
            xbar.io.req2Node.in(i)          <> slice.io.req2Node
            xbar.io.resp2Slice.out(i)       <> slice.io.resp2Slice
            // slice DataBuffer signals
            xbar.io.dbSigs.in0(i+nrIntf)    <> slice.io.dbRCReq
    }

    /*
     * Connect DataBuffer
     */
    databuffer.io <> xbar.io.dbSigs.out(0)
}
