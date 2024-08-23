package DONGJIANG

import DONGJIANG.CHI._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import scala.math.{max, min}

case object DJParamKey extends Field[DJParam](DJParam())


// Node Interface Params, used for generation
case class InterfaceParam
(
    // BASE
    name: String, // RNSLAVE / RNMASTER / SNMASTER
    nrReqBuf: Int = 16,
    isRn: Boolean,
    isSlave: Boolean,
    // Use In Master
    addressId: Option[Int] = None,
    addressIdBits: Option[Int] = None,
    // Use In Slave
    preferTgtIdMap: Option[Seq[Int]] = None,
) {
    val reqBufIdBits = log2Ceil(nrReqBuf)
    val isSn = !isRn
    val isMaster = !isSlave
    val hasReq2Slice = true
    val hasDBRCReq = isMaster
    require(addressId.nonEmpty | isSlave)
    if(isMaster) require(addressId.nonEmpty)      else require(addressId.isEmpty)
    if(isMaster) require(addressIdBits.nonEmpty)  else require(addressIdBits.isEmpty)
    if(isSlave)  require(preferTgtIdMap.nonEmpty) else require(preferTgtIdMap.isEmpty)
}

// Node Param In Local Ring
case class NodeParam
(
  name: String = "Node",
  nodeId: Int,
  isRN: Boolean = false,
  isHN: Boolean = false,
  isSN: Boolean = false,
  isDDR: Boolean = false,
  useDCT: Boolean = false,
  useDMT: Boolean = false,
  useDWT: Boolean = false,
  bankId: Option[Int] = None, // Only Use In isSN and !isDDR
  bankIdBits: Option[Int] = None, // Only Use In isSN and !isDDR
  addressId: Option[Int] = None, // Only Use In isHN or isSN and isDDR
  addressIdBits: Option[Int] = None, // Only Use In isHN or isSN and sisDDR
) {
    val tpyes = Seq(isRN, isHN, isSN)
    require(tpyes.count(_ == true) == 1)
    if(isRN) require(!isDDR)
    if(isHN) require(!isDDR); require(!useDCT); require(!useDMT); require(!useDWT)
    if(isSN) require(!useDCT)
    if(isSN & !isDDR) {
        require(bankId.nonEmpty); require(bankIdBits.nonEmpty)
    } else {
        require(bankId.isEmpty); require(bankIdBits.isEmpty)
    }
    if(isHN | (isSN & isDDR)) {
        require(addressId.nonEmpty); require(addressIdBits.nonEmpty)
    }else {
        require(addressId.isEmpty); require(addressIdBits.isEmpty)
    }
}


case class DJParam(
                    // -------------------------- Base Mes ---------------------- //
                    blockBytes: Int = 64,
                    beatBytes: Int = 32,
                    addressBits: Int = 48,
                    hasLLC: Boolean = true,
                    // ------------------------- Interface Mes -------------------- //
                    interfaceMes: Seq[InterfaceParam] = Seq(InterfaceParam( name = "RnSalve_LOCAL",  isRn = true,   isSlave = true,  preferTgtIdMap = Some(Seq(0x0, 0x1)) ),
                                                            InterfaceParam( name = "SnMaster_LOCAL", isRn = false,  isSlave = false, addressId = Some(0), addressIdBits = Some(0) ),
                                                            // InterfaceParam( name = "RnSalve_CSN",    isRn = true,   isSlave = true,  preferTgtIdMap = Some(Seq(0)) ),
                                                            // InterfaceParam( name = "RnMaster_CSN",   isRn = true,   isSlave = false, addressId = Some(1), addressIdBits = Some(1) )
                    ),
                    // ------------------------- Node Mes -------------------- //
                    nodeMes: Seq[NodeParam] = Seq(  // NodeParam( name = "RN_CSN",     nodeId = 0, isRN = true ),
                                                    // NodeParam( name = "HN_CSN",     nodeId = 1, isHN = true, addressId = Some(1), addressIdBits = Some(1) ),
                                                    NodeParam( name = "RN_LOCAL_0", nodeId = 0x0, isRN = true ),
                                                    NodeParam( name = "RN_LOCAL_1", nodeId = 0x1, isRN = true ),
                                                    NodeParam( name = "SN_LOCAL_0", nodeId = 0x30, isSN = true, bankId = Some(0), bankIdBits = Some(1) ),
                                                    NodeParam( name = "SN_LOCAL_1", nodeId = 0x31, isSN = true, bankId = Some(1), bankIdBits = Some(1) ),
                                                    NodeParam( name = "SN_DDR",     nodeId = 0x32, isSN = true, isDDR = true, addressId = Some(0), addressIdBits = Some(1))
                    ),
                    // ------------------------ Slice Base Mes ------------------ //
                    nrMpTaskQueue: Int = 4,
                    nrMpReqQueue: Int = 4,
                    nrMpRespQueue: Int = 4,
                    mpBlockBySet: Boolean = true,
                    // MSHR
                    nrMSHRSets: Int = 4,
                    nrMSHRWays: Int = 4,
                    // number of bank or buffer
                    nrBank: Int = 2,
                    nrSnpCtl: Int = 16,
                    nrDataBuf: Int = 16,
                    // ------------------------ Directory Mes ------------------ //
                    // self dir & ds mes, dont care when hasLLC is false
                    nrSelfDirBank: Int = 2,
                    selfWays: Int = 4,
                    selfSets: Int = 32,
                    selfDirMulticycle: Int = 2,
                    selfDirHoldMcp: Boolean = true,
                    selfReplacementPolicy: String = "plru",
                    // snoop filter dir mes
                    nrSFDirBank: Int = 2,
                    sfDirWays: Int = 4,
                    sfDirSets: Int = 32,
                    sfDirMulticycle: Int = 2,
                    sfDirHoldMcp: Boolean = true,
                    sfReplacementPolicy: String = "plru",
                  ) {
    require(interfaceMes.nonEmpty)
    require(nodeMes.nonEmpty)
    require(nrMpTaskQueue > 0)
    require(nrMpReqQueue > 0)
    require(nrMpRespQueue > 0)
    require(nrMSHRSets <= selfSets)
    require(nrBank == 1 | nrBank == 2 | nrBank == 4)
    require(nrMSHRWays <= min(selfWays, sfDirWays))
    require(selfReplacementPolicy == "random" || selfReplacementPolicy == "plru")
    require(sfReplacementPolicy == "random" || sfReplacementPolicy == "plru")
}

trait HasDJParam {
    val p: Parameters
    val djparam = p(DJParamKey)

    // Base Mes Parameters
    val nrBeat          = djparam.blockBytes / djparam.beatBytes
    val addressBits     = djparam.addressBits
    val dataBits        = djparam.blockBytes * 8
    val beatBits        = djparam.beatBytes * 8

    // Base Interface Mes
    val nrIntf          = djparam.interfaceMes.length
    val rnSlvParamSeq   = djparam.interfaceMes.filter(i => i.isRn).filter(i => i.isSlave)
    val rnMasParamSeq   = djparam.interfaceMes.filter(i => i.isRn).filter(i => i.isMaster)
    val snSlvParamSeq   = djparam.interfaceMes.filter(i => i.isSn).filter(i => i.isSlave)
    val snMasParamSeq   = djparam.interfaceMes.filter(i => i.isSn).filter(i => i.isMaster)
    val nrRnSlv         = rnSlvParamSeq.length
    val nrRnMas         = rnMasParamSeq.length
    val nrSnSlv         = snSlvParamSeq.length
    val nrSnMas         = snMasParamSeq.length
    val nrSlave         = nrRnSlv + nrSnSlv
    val nrMaster        = nrRnMas + nrSnMas
    val rnslvIdBits     = log2Ceil(nrRnSlv)
    val nrReqBufMax     = djparam.interfaceMes.map(_.nrReqBuf).max
    val reqBufIdBits    = log2Ceil(nrReqBufMax)
    require(nrRnSlv >= 1)
    require(nrRnSlv <= 2)
    require(nrRnMas <= 1)
    require(nrSnSlv == 0)
    require(nrSnMas <= 1)

    // Base Node Mes
    val nrNode          = djparam.nodeMes.length
    val nodeIdMax       = djparam.nodeMes.map(_.nodeId).max
    val nodeIdBits      = log2Ceil(nodeIdMax)

    // Slice Queue
    val mpTaskQBits     = log2Ceil(djparam.nrMpTaskQueue)
    val mpReqQBits      = log2Ceil(djparam.nrMpReqQueue)
    val mpRespQBits     = log2Ceil(djparam.nrMpRespQueue)

    // Slice Id Bits Parameters
    val dbIdBits        = log2Ceil(djparam.nrDataBuf)

    // DIR BASE Parameters
    val bankBits        = log2Ceil(djparam.nrBank)
    val offsetBits      = log2Ceil(djparam.blockBytes)

    // SELF DIR Parameters: [sTag] + [sSet] + [sDirBank] + [bank] + [offset]
    // [sSet] + [sDirBank] = [setBis]
    val sWayBits        = log2Ceil(djparam.selfWays)
    val sDirBankBits    = log2Ceil(djparam.nrSelfDirBank)
    val sSetBits        = log2Ceil(djparam.selfSets/djparam.nrSelfDirBank)
    val sTagBits        = djparam.addressBits - sSetBits - sDirBankBits - bankBits - offsetBits

    // SF DIR Parameters: [cTag] + [cSet] + [cDirBank] + [bank] + [offset]
    // [sfSet] + [sfDirBank] = [sfSetsBits]
    val sfWayBits       = log2Ceil(djparam.sfDirWays)
    val sfDirBankBits   = log2Ceil(djparam.nrSFDirBank)
    val sfSetBits       = log2Ceil(djparam.sfDirSets / djparam.nrSFDirBank)
    val sfTagBits       = djparam.addressBits - sfSetBits - sfDirBankBits - bankBits - offsetBits

    // DS Parameters
    val dsWayBits       = sWayBits
    val dsSetBits       = sSetBits

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
    val TIMEOUT_RB      = 10000 // ReqBuf
    val TIMEOUT_DB      = 8000  // DataBuffer
    val TIMEOUT_BT      = 8000  // BlockTable
    val TIMEOUT_MP      = 8000  // MainPipe
    val TIMEOUT_SNP     = 8000  // SnoopCtl
    val TIMEOUT_RC      = 6000  // ReadCtl
    val TIMEOUT_TXD     = 1000  // SnChiTxDat

    // chiParams
    val chiParams = CHIBundleParameters(
        nodeIdBits = 7,
        addressBits = addressBits,
        dataBits = beatBits,
        dataCheck = false,
        snpHasTgtId = true
    )

    // some requirements
    require(reqBufIdBits <= chiParams.txnidBits)
    require(dbIdBits <= chiParams.dbidBits)

    def parseAddress(x: UInt, modBankBits: Int = 1, setBits: Int = 1, tagBits: Int = 1): (UInt, UInt, UInt, UInt, UInt) = {
        val offset  = x
        val bank    = offset    >> offsetBits
        val modBank = bank      >> bankBits
        val set     = modBank   >> modBankBits
        val tag     = set       >> setBits
        // return: [5:tag] [4:set] [3:modBank] [2:bank] [1:offset]
        (tag(tagBits - 1, 0), set(setBits - 1, 0), modBank(modBankBits - 1, 0), bank(bankBits - 1, 0), offset(offsetBits - 1, 0))
    }

    def parseMSHRAddress(x: UInt, mpBlockBySet: Boolean = false): (UInt, UInt, UInt) = {
        val tag = WireInit(0.U(mshrTagBits.W))
        val bank = WireInit(0.U(bankBits.W))
        val (tag_, set, modBank, bank_, offset) = parseAddress(x, modBankBits = 0, setBits = mshrSetBits, tagBits = mshrTagBits)
        if (mpBlockBySet) {
            tag := tag_ // TODO: When !mpBlockBySet it must support useWayOH Check and RetryQueue
            bank := bank_
        } else {
            require(sSetBits + sDirBankBits > mshrSetBits)
            tag := tag_(sSetBits + sDirBankBits - 1 - mshrSetBits, 0)
            bank := 0.U
        }
        // return: [3:mshrTag] [2:mshrSet] [1:bank]
        (tag, set, bank)
    }

    def toDataID(x: UInt): UInt = {
        if (nrBeat == 1) { x }
        else if (nrBeat == 2) { Mux(x === 0.U, 0.U, 2.U) }
        else { 0.U }
    }
}
