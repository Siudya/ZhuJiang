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
    nrReqBuf: Int = 16,
    nrPCUEntry: Int = 16,
) {
    val reqBufIdBits = log2Ceil(nrReqBuf)
    val pcuIdBits = log2Ceil(nrPCUEntry)
    val isSn = !isRn
    val isMaster = !isSlave
    val hasReq2Slice = true
    val hasDBRCReq = isMaster
    if(isMaster) require(chipId.nonEmpty) else require(chipId.isEmpty)
}

// Node Param In Local Ring
case class NodeParam
(
  name: String = "Node",
  nodeId: Int,
  local: Boolean = false,
  isRNF: Boolean = false,
  isRNI: Boolean = false,
  isHN: Boolean = false,
  isSN: Boolean = false,
  isDDR: Boolean = false,
  useDCT: Boolean = false,
  useDMT: Boolean = false,
  useDWT: Boolean = false,
  chipId: Option[Int] = None, // Only Use In isHN
  bankId: Option[Int] = None, // Only Use In isSN and !isDDR
  bankIdBits: Option[Int] = None, // Only Use In isSN and !isDDR
) {
    val tpyes = Seq(isRNF, isRNI, isHN, isSN)
    val isRN = isRNF | isRNI
    require(tpyes.count(_ == true) == 1)
    if(isRN) require(!isDDR)
    if(isSN) require(!useDCT)
    if(isSN & !isDDR) {
        require(bankId.nonEmpty); require(bankIdBits.nonEmpty)
    } else {
        require(bankId.isEmpty); require(bankIdBits.isEmpty)
    }
    if(isHN | (isSN & isDDR)) {
        require(chipId.nonEmpty)
    } else {
        require(chipId.isEmpty)
    }
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
                    nodeIdBits: Int = 12,
                    txnidBits: Int = 12,
                    dbidBits: Int = 16,
                    localNodeID: Int = 0x10,
                    csnNodeID: Int = 0,
                    // ------------------------- Node Mes -------------------- //
                    nodeMes: Seq[NodeParam] = Seq(  // NodeParam( name = "RN_CSN",     local = false, nodeId = 0,    isRNF = true ),
                                                    // NodeParam( name = "HN_CSN",     local = false, nodeId = 1,    isHN = true, chipId = Some(1)),
                                                    NodeParam( name = "RN_LOCAL_0", local = true,  nodeId = 0x0,  isRNF = true ),
                                                    NodeParam( name = "RN_LOCAL_1", local = true,  nodeId = 0x1,  isRNF = true ),
                                                    NodeParam( name = "SN_LOCAL_0", local = true,  nodeId = 0x30, isSN = true, bankId = Some(0), bankIdBits = Some(1) ),
                                                    NodeParam( name = "SN_LOCAL_1", local = true,  nodeId = 0x31, isSN = true, bankId = Some(1), bankIdBits = Some(1) ),
                                                    NodeParam( name = "SN_DDR",     local = true,  nodeId = 0x32, isSN = true, isDDR = true, chipId = Some(0))
                    ),
                    // ------------------------ Slice Base Mes ------------------ //
                    nrMpTaskQueue: Int = 4,
                    nrMpReqQueue: Int = 4,
                    nrMpRespQueue: Int = 4,
                    // MSHR
                    nrMSHRSets: Int = 4,
                    nrMSHRWays: Int = 4,
                    nrEvictWays: Int = 4,
                    // number of bank or buffer
                    nrBank: Int = 2,
                    nrSnpCtl: Int = 16,
                    nrDataBuf: Int = 16,
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
                    nrDirBank: Int = 2,
                    dirMulticycle: Int = 2,
                    dirHoldMcp: Boolean = true,
                  ) {
    require(nodeMes.nonEmpty)
    require(nodeMes.count(_.isDDR) == 1)
    require(2 <= nrEvictWays & nrEvictWays <= nrMSHRWays)
    require(nrMpTaskQueue > 0)
    require(nrMpReqQueue > 0)
    require(nrMpRespQueue > 0)
    require(nrMSHRSets <= selfSets)
    require(nrBank == 1 | nrBank == 2 | nrBank == 4)
    require(nrMSHRWays <= min(selfWays, sfDirWays))
    require(selfReplacementPolicy == "random" || selfReplacementPolicy == "plru")
    require(sfReplacementPolicy == "random" || sfReplacementPolicy == "plru")
}


trait HasParseZJParam extends HasZJParams {
    val localHnfNode    = zjparam.localRing.filter(_.nodeType == NodeType.HF).last
    val hasCSN          = zjparam.csnRing.nonEmpty
    val csnHnfNodeOpt   = if(hasCSN) Option(zjparam.csnRing.filter(_.nodeType == NodeType.HF).last) else None
    val csnRnfNodeOpt   = if(hasCSN) Option(zjparam.csnRing.filter(_.nodeType == NodeType.R).last) else None

    require(zjparam.localRing.count(_.nodeType == NodeType.HF) == 1)
    require(localHnfNode.splitFlit)

    if(hasCSN) {
        require(csnHnfNodeOpt.get.splitFlit)
        require(csnRnfNodeOpt.get.splitFlit)
    }
}


trait HasDJParam extends HasParseZJParam {
    val p: Parameters
    val djparam = p(DJParamKey)

    // Base Mes Parameters
    val nrBeat          = djparam.blockBytes / djparam.beatBytes
    val addressBits     = djparam.addressBits
    val dataBits        = djparam.blockBytes * 8
    val beatBits        = djparam.beatBytes * 8
    val localChipId     = djparam.nodeMes.filter(_.isDDR).map(_.chipId)(0).get

    // Base Interface Mes
    val hasCSNIntf      = djparam.csnRnSlaveIntf.nonEmpty & djparam.csnRnMasterIntf.nonEmpty
    val interfaceMes    = if(hasCSNIntf) Seq(djparam.localRnSlaveIntf, djparam.localSnMasterIntf, djparam.csnRnSlaveIntf.get, djparam.csnRnMasterIntf.get)
                          else           Seq(djparam.localRnSlaveIntf, djparam.localSnMasterIntf)
    val nrIntf          = interfaceMes.length
    val nrSlvIntf       = interfaceMes.count(_.isSlave)
    val nrMasIntf       = interfaceMes.count(_.isMaster)
    val nrIntfBits      = log2Ceil(nrIntf)
    val nrReqBufMax     = interfaceMes.map(_.nrReqBuf).max
    val reqBufIdBits    = log2Ceil(nrReqBufMax)

    // Base Node Mes
    val nrRnfNode       = djparam.nodeMes.count(_.isRNF)
    val rnfNodeIdBits   = log2Ceil(nrRnfNode)
    val snNodeIdSeq     = djparam.nodeMes.filter(_.isSN).map(_.nodeId)
    val ddrcNodeId      = djparam.nodeMes.filter(_.isDDR).map(_.nodeId)(0)
    def fromSnNode(x: UInt) = snNodeIdSeq.map(_.asUInt === x).reduce(_ | _)

    // Slice Queue
    val mpTaskQBits     = log2Ceil(djparam.nrMpTaskQueue)
    val mpReqQBits      = log2Ceil(djparam.nrMpReqQueue)
    val mpRespQBits     = log2Ceil(djparam.nrMpRespQueue)

    // Slice Id Bits Parameters
    val dbIdBits        = log2Ceil(djparam.nrDataBuf)

    // DIR BASE Parameters
    val bankBits        = log2Ceil(djparam.nrBank)
    val dirBankBits     = log2Ceil(djparam.nrDirBank)
    val offsetBits      = log2Ceil(djparam.blockBytes)

    // SELF DIR Parameters: [sTag] + [sSet] + [sDirBank] + [bank] + [offset]
    // [sSet] + [sDirBank] = [setBis]
    val sWayBits        = log2Ceil(djparam.selfWays)
    val sSetBits        = log2Ceil(djparam.selfSets/djparam.nrDirBank)
    val sTagBits        = djparam.addressBits - sSetBits - dirBankBits - bankBits - offsetBits

    // SF DIR Parameters: [cTag] + [cSet] + [cDirBank] + [bank] + [offset]
    // [sfSet] + [sfDirBank] = [sfSetsBits]
    val sfWayBits       = log2Ceil(djparam.sfDirWays)

    val sfSetBits       = log2Ceil(djparam.sfDirSets / djparam.nrDirBank)
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
    val TIMEOUT_PCU     = 5000 // PCU
    val TIMEOUT_RB      = 10000 // ReqBuf
    val TIMEOUT_DB      = 8000  // DataBuffer
    val TIMEOUT_BT      = 8000  // BlockTable
    val TIMEOUT_MP      = 8000  // MainPipe
    val TIMEOUT_SNP     = 8000  // SnoopCtl
    val TIMEOUT_RC      = 6000  // ReadCtl
    val TIMEOUT_TXD     = 1000  // SnChiTxDat

    // some requirements for CHI width
    require(reqBufIdBits <= 12)
    require(dbIdBits <= 16)

    def getChipTypeByAddr(x: UInt): UInt = {
        val chipType = Wire(UInt(ChipType.width.W))
        when(x(46, 44) === localChipId.U) {
            chipType := ChipType.Local
        }.otherwise {
            chipType := ChipType.CSN
        }
        chipType
    }

    def getMetaIdBySrcID(x: UInt): UInt = {
        val rnfNodeId   = djparam.nodeMes.filter(_.isRNF).map(_.nodeId.U)
        val metaId      = WireInit((nrRnfNode+1).U((rnfNodeIdBits+1).W))
        rnfNodeId.zipWithIndex.foreach {
            case (id, i) =>
                when(x === id) {
                    metaId := i.U
                }
        }
        assert(metaId =/= (nrRnfNode+1).U)
        metaId(rnfNodeIdBits-1, 0)
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

    def toDataID(x: UInt): UInt = {
        if (nrBeat == 1) { x }
        else if (nrBeat == 2) { Mux(x === 0.U, 0.U, 2.U) }
        else { 0.U }
    }
}
