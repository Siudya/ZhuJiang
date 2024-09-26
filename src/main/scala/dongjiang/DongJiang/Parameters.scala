package DONGJIANG

import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xijiang.NodeType
import zhujiang.HasZJParams

import scala.math.{max, min}

case object DJParamKey extends Field[DJParam](DJParam())


// Node Interface Params, used for generation
case class InterfaceParam
(
    // BASE
    name: String, // RNSLAVE / RNMASTER / SNMASTER
    isRn: Boolean,
    isSlave: Boolean,
    chipId: Option[Int] = None, // Use In MASTER
    nrPCUEntry: Int = 16,
    nrPCUEvictEntry: Int = 4
) {
    val pcuIdBits = log2Ceil(nrPCUEntry)
    val isSn = !isRn
    val isMaster = !isSlave
    val hasReq2Slice = true
    val hasDBRCReq = isMaster
    if(isMaster) require(chipId.nonEmpty) else require(chipId.isEmpty)
}


case class DJParam(
                    // -------------------------- Base Mes ---------------------- //
                    blockBytes: Int = 64,
                    beatBytes: Int = 32,
                    addressBits: Int = 48,
                    hasLLC: Boolean = true,
                    // ------------------------- Interface Mes -------------------- //
                    localRnSlaveIntf:   InterfaceParam =              InterfaceParam( name = "RnSalve_LOCAL",  isRn = true,   isSlave = true),
                    localSnMasterIntf:  InterfaceParam =              InterfaceParam( name = "SnMaster_LOCAL", isRn = false,  isSlave = false, chipId = Some(0)),
                    csnRnSlaveIntf:     Option[InterfaceParam] = None, // Some(InterfaceParam( name = "RnSalve_CSN",    isRn = true,   isSlave = true)),
                    csnRnMasterIntf:    Option[InterfaceParam] = None, // Some(InterfaceParam( name = "RnMaster_CSN",   isRn = true,   isSlave = false, chipId = Some(1) )),
                    chiTxnidBits: Int = 12,
                    chiDBIDBits: Int = 16,
                    // ------------------------ DCU Base Mes ------------------ //
                    nrDSBank: Int = 2,
                    nrDCUWBuf: Int = 4,
                    nrDCUReqQ: Int = 4,
                    nrDCURespQ: Int = 4,
                    dcuMulticycle: Int = 2,
                    dcuHoldMcp: Boolean = true,
                    // ------------------------ CPU Base Mes ------------------ //
                    nrMpTaskQueue: Int = 4,
                    nrMpReqQueue: Int = 4,
                    nrMpRespQueue: Int = 4,
                    // MSHR
                    nrMSHRSets: Int = 2,
                    nrMSHRWays: Int = 4,
                    // number of bank or buffer
                    nrBank: Int = 2,
                    nrDatBuf: Int = 16,
                    // ------------------------ Directory Mes ------------------ //
                    // self dir & ds mes, dont care when hasLLC is false
                    selfWays: Int = 4,
                    selfSets: Int = 32,
                    selfReplacementPolicy: String = "plru",
                    // snoop filter dir mes
                    sfDirWays: Int = 4,
                    sfDirSets: Int = 32,
                    sfReplacementPolicy: String = "plru",
                    // DIR SRAM
                    nrDirBank: Int = 4,
                    dirMulticycle: Int = 2,
                    dirHoldMcp: Boolean = true,
                  ) {
    require(nrDirBank >= nrMSHRSets)
    require(isPow2(nrDirBank))
    require(isPow2(nrMSHRSets))
    require(nrMpTaskQueue > 0)
    require(nrMpReqQueue > 0)
    require(nrMpRespQueue > 0)
    require(nrMSHRSets <= selfSets)
    require(nrBank == 1 | nrBank == 2 | nrBank == 4)
    require(nrMSHRWays <= min(selfWays, sfDirWays))
    require(selfReplacementPolicy == "random" || selfReplacementPolicy == "plru")
    require(sfReplacementPolicy == "random" || sfReplacementPolicy == "plru")
    require(log2Ceil(nrDCUWBuf) <= chiDBIDBits)
}


trait HasParseZJParam extends HasZJParams {
    val localRnfNode    = zjParams.localRing.filter(_.nodeType == NodeType.RF)
    val localHnfNode    = zjParams.localRing.filter(_.nodeType == NodeType.HF).last
    val localSnNode     = zjParams.localRing.filter(_.nodeType == NodeType.S)
    val hasCSN          = zjParams.csnRing.nonEmpty
    val csnHnfNodeOpt   = if(hasCSN) Option(zjParams.csnRing.filter(_.nodeType == NodeType.HF).last) else None
    val csnRnfNodeOpt   = if(hasCSN) Option(zjParams.csnRing.filter(_.nodeType == NodeType.RF).last) else None

    require(zjParams.localRing.count(_.nodeType == NodeType.HF) == 1)
    require(localSnNode.last.mainMemory)
    require(localHnfNode.splitFlit)
    if(hasCSN) {
        require(csnHnfNodeOpt.get.splitFlit)
        require(csnRnfNodeOpt.get.splitFlit)
    }

    // CHI Node ID Width
    val chiNodeIdBits   = zjParams.nodeIdBits

    // Local Base Node Mes
    val nrRnfNode       = zjParams.localRing.count(_.nodeType  == NodeType.RF)
    val rnfNodeIdBits   = log2Ceil(nrRnfNode)
    val rnNodeIdSeq     = localRnfNode.map(_.nodeId)
    val snNodeIdSeq     = localSnNode.map(_.nodeId)
    val ddrcNodeId      = localSnNode.map(_.nodeId).last
    val hnfNodeId       = localHnfNode.nodeId

    // CSN Base Node Mes
    val csnHnfNodeId    = 0 // TODO

    def fromSnNode(x: UInt) = snNodeIdSeq.map(_.asUInt === x).reduce(_ | _)

    def getSnNodeIDByBankId(x: UInt): UInt = {
        val nodeID = WireInit(0xfff.U)
        snNodeIdSeq.zipWithIndex.foreach {
            case(id, i) =>
                when(x === i.U) {
                    nodeID := id.U
                }
        }
        assert(nodeID =/= 0xfff.U)
        nodeID
    }

    def getMetaIdByNodeID(x: UInt): UInt = {
        val metaId = WireInit((nrRnfNode + 1).U((rnfNodeIdBits + 1).W))
        rnNodeIdSeq.zipWithIndex.foreach {
            case (id, i) =>
                when(x === id.U) {
                    metaId := i.U
                }
        }
        assert(metaId =/= (nrRnfNode + 1).U)
        metaId(rnfNodeIdBits - 1, 0)
    }

    def getNodeIDByMetaId(x: UInt) = {
        val nodeID = WireInit(0xfff.U)
        rnNodeIdSeq.zipWithIndex.foreach {
            case (id, i) =>
                when(x === i.U) {
                    nodeID := id.U
                }
        }
        assert(nodeID =/= 0xfff.U)
        nodeID(10, 0)
    }
}


trait HasDJParam extends HasParseZJParam {
    val p: Parameters
    val djparam = p(DJParamKey)

    // Base Mes Parameters
    val nrBeat          = djparam.blockBytes / djparam.beatBytes
    val beatNumBits     = log2Ceil(nrBeat)
    val addressBits     = djparam.addressBits
    val dataBits        = djparam.blockBytes * 8
    val beatBits        = djparam.beatBytes * 8
    val localChipId     = 0 // TODO
    val csnChipId       = 1 // TODO
    require(isPow2(nrBeat))

    // Base Interface Mes
    val hasCSNIntf      = djparam.csnRnSlaveIntf.nonEmpty & djparam.csnRnMasterIntf.nonEmpty
    val interfaceMes    = if(hasCSNIntf) Seq(djparam.localRnSlaveIntf, djparam.localSnMasterIntf, djparam.csnRnSlaveIntf.get, djparam.csnRnMasterIntf.get)
                          else           Seq(djparam.localRnSlaveIntf, djparam.localSnMasterIntf)
    val nrIntf          = interfaceMes.length
    val nrSlvIntf       = interfaceMes.count(_.isSlave)
    val nrMasIntf       = interfaceMes.count(_.isMaster)
    val nrIntfBits      = log2Ceil(nrIntf)
    val nrPCUMax        = interfaceMes.map(_.nrPCUEntry).max
    val pcuIdBits       = log2Ceil(nrPCUMax)

    // Base DCU Mes
    val dcuWBufIdBits   = log2Ceil(djparam.nrDCUWBuf)
    val nrDCUsEntry     = djparam.selfSets * djparam.selfWays
    val nrPerDCUEntry   = nrDCUsEntry / djparam.nrBank
    val nrDSEntry       = nrPerDCUEntry / djparam.nrDSBank
    // DCU Index = [sSet] + [dirBank] + [sWay] = [dsIndex] + [dsBank]
    // Bank -> EREQ TgtID
    val dcuIndexBits    = log2Ceil(nrPerDCUEntry)
    val dsIndexBits     = log2Ceil(nrDSEntry)
    val dsBankBits      = log2Ceil(djparam.nrDSBank)
    require(dcuIndexBits == (dsIndexBits + dsBankBits))

    // Base Fake DDRC Mes
    val nrDDRCBank      = dataBits / 64
    val nrDDRCReqQ      = 4
    val nrDDRCRespQ     = 4
    val nrDDRCWBuf      = 4
    val ddrcWBufIdBits  = log2Ceil(nrDDRCWBuf)

    // Slice Id Bits Parameters
    val dbIdBits        = log2Ceil(djparam.nrDatBuf)

    // DIR BASE Parameters
    val bankBits        = log2Ceil(djparam.nrBank)
    val dirBankBits     = log2Ceil(djparam.nrDirBank)
    val offsetBits      = log2Ceil(djparam.blockBytes)

    // SELF DIR Parameters: [sTag] + [sSet] + [dirBank] + [bank] + [offset]
    val sWayBits        = log2Ceil(djparam.selfWays)
    val sSetBits        = log2Ceil(djparam.selfSets  / djparam.nrBank  /djparam.nrDirBank)
    val sTagBits        = djparam.addressBits - sSetBits - dirBankBits - bankBits - offsetBits

    // SF DIR Parameters: [sfTag] + [sfSet] + [dirBank] + [bank] + [offset]
    val sfWayBits       = log2Ceil(djparam.sfDirWays)
    val sfSetBits       = log2Ceil(djparam.sfDirSets / djparam.nrBank / djparam.nrDirBank)
    val sfTagBits       = djparam.addressBits - sfSetBits - dirBankBits - bankBits - offsetBits

    // MSHR TABLE Parameters: [mshrTag] + [mshrSet] + [bank] + [offset]
    val mshrWayBits     = log2Ceil(djparam.nrMSHRWays)
    val mshrSetBits     = log2Ceil(djparam.nrMSHRSets)
    val mshrTagBits     = djparam.addressBits - mshrSetBits - bankBits - offsetBits

    // replacement Parameters
    val sReplWayBits    = if(djparam.selfReplacementPolicy != "random") djparam.selfWays - 1 else 0
    val sfReplWayBits   = if(djparam.sfReplacementPolicy != "random") djparam.sfDirWays - 1 else 0
    require(djparam.selfReplacementPolicy == "random" | djparam.selfReplacementPolicy == "plru", "It should modify sReplWayBits when use replacement except of random or plru")
    require(djparam.sfReplacementPolicy == "random" | djparam.sfReplacementPolicy == "plru", "It should modify cReplWayBits when use replacement except of random or plru")

    // Node address id map check
    // TODO

    // TIMEOUT CHECK CNT VALUE
    val TIMEOUT_RSPCU   = 5000 // Rn Slave PCU
    val TIMEOUT_SMPCU   = 5000 // Sn Master PCU
    val TIMEOUT_RMPCU   = 5000 // Rn Master PCU
    val TIMEOUT_DB      = 4000  // DataBuffer
    val TIMEOUT_MSHR    = 8000  // BlockTable
    val TIMEOUT_EXU     = 2000  // Pipe Execute
    val TIMEOUT_COM     = 2000  // Pipe Commit

    // some requirements for CHI width
    require(pcuIdBits <= djparam.chiTxnidBits)
    require(dbIdBits <= djparam.chiDBIDBits)

    def getChipTypeByAddr(x: UInt): UInt = {
        val chipType = Wire(UInt(ChipType.width.W))
        when(x(46, 44) === localChipId.U) {
            chipType := ChipType.Local
        }.otherwise {
            chipType := ChipType.CSN
        }
        chipType
    }

    def parseAddress(x: UInt, modBankBits: Int = 1, setBits: Int = 1, tagBits: Int = 1): (UInt, UInt, UInt, UInt, UInt) = {
        val offset  = x
        val bank    = offset    >> offsetBits
        val modBank = bank      >> bankBits
        val set     = modBank   >> modBankBits
        val tag     = set       >> setBits
        // return: [1:tag] [2:set] [3:modBank] [4:bank] [5:offset]
        require(x.getWidth == addressBits)
        (tag(tagBits - 1, 0), set(setBits - 1, 0), modBank(modBankBits - 1, 0), bank(bankBits - 1, 0), offset(offsetBits - 1, 0))
    }

    def parseMSHRAddress(x: UInt): (UInt, UInt, UInt) = {
        val (tag, set, modBank, bank, offset) = parseAddress(x, modBankBits = 0, setBits = mshrSetBits, tagBits = mshrTagBits)
        require(sSetBits + dirBankBits >= mshrSetBits)
        // return: [1:mshrTag] [2:mshrSet] [3:bank]
        (tag, set, bank)
    }

     def parseDCUAddress(x: UInt): (UInt, UInt) = {
         val dsBank  = x
         val dsIndex = dsBank    >> dsBankBits
         // return: [1:dsIndex] [2:dsBank]
         (dsIndex(dsIndexBits - 1, 0), dsBank(dsBankBits - 1, 0))
     }


    def getDCUAddress(addr:UInt, sWay:UInt): UInt = {
        require(addr.getWidth == addressBits)
        require(sWay.getWidth == sWayBits)
        val (sTag, sSet, dirbank, bank, offset) = parseAddress(addr, dirBankBits, sSetBits, sTagBits)
        val dcuAddr = Cat(sSet, dirbank, sWay)
        require(dcuAddr.getWidth == dcuIndexBits, s"${dcuAddr.getWidth} = ${sSet.getWidth} + ${dirbank.getWidth} + ${sWay.getWidth} =/= $dcuIndexBits")
        dcuAddr
    }

    def getDirBank(x: UInt): UInt = parseAddress(x, dirBankBits, 0, 0)._3

    def toDataID(x: UInt): UInt = {
        require(nrBeat == 1 | nrBeat == 2 | nrBeat == 4)
        if (nrBeat == 1) { "b00".U }
        else if (nrBeat == 2) { Mux(x === 0.U, "b00".U, "b10".U) }
        else if (nrBeat == 4) { x }
        else { 0.U }
    }

    def toBeatNum(x: UInt): UInt = {
        if (nrBeat == 1) { assert(x === "b00".U); 0.U }
        else if (nrBeat == 2) { assert(x === "b00".U | x === "b10".U); Mux(x === "b00".U, 0.U, 1.U) }
        else if (nrBeat == 4) { x }
        else { 0.U }
    }
}
