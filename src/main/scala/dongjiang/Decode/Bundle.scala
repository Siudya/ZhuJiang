package DONGJIANG.DECODE

import DONGJIANG._
import DONGJIANG.CHI._
import DONGJIANG.CHI.CHIChannel._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xs.utils.ParallelLookUp
import DONGJIANG.CHI.CHIOp.REQ._
import DONGJIANG.CHI.CHIOp.RSP._
import DONGJIANG.CHI.CHIOp.DAT._
import DONGJIANG.CHI.CHIOp.SNP._
import DONGJIANG.CHI.ChiState._


object RespType {
  val width         = 3
  val NotResp       = "b000".U
  val Snp           = "b001".U
  val SnpFwd        = "b010".U
  val RD            = "b100".U // Read Down

  def Snp_RD        = Snp | RD
  def SnpFwd_RD     = SnpFwd | RD
  def NOTRESP       = 0.U((RespType.width + 1 + ChiResp.width * 3).W)

  def RespNoData    = 0.U
  def RespHasData   = 1.U
}

class InstBundle(implicit p: Parameters) extends DJBundle {
  val chipType    = UInt(ChipType.width.W)
  val channel     = UInt(CHIChannel.width.W)
  val opcode      = UInt(6.W)
  val srcState    = UInt(ChiState.width.W)
  val othState    = UInt(ChiState.width.W)
  val hnState     = UInt(ChiState.width.W)
  val respType    = UInt(RespType.width.W)
  val respHasData = Bool()
  val snpResp     = UInt(ChiResp.width.W)
  val fwdState    = UInt(ChiResp.width.W)
  val rdResp      = UInt(ChiResp.width.W) // Read Down
}


trait HasOperationsBundle extends Bundle {
  // Commit(Resp to Rn Node)
  val commit      = Bool()

  // Send Snoop to Slave Node
  val snoop       = Bool()

  // Send Read to Slave Node
  val readDown    = Bool()
  val writeDown   = Bool()

  // Read DataBuffer to Send Data to Resp Node
  val rDB2Src     = Bool() // Read DataBuffer to Req Src
  val cleanDB     = Bool() // Clean DataBuffer

  // Read(Send Data to Resp Node) or Write DataStorage
  val readDCU     = Bool()
  val writeDCU    = Bool()

  // Write New State to Directory
  val wSDir       = Bool()
  val wSFDir      = Bool()

  def reqToSlv    = snoop
  def reqToMas    = readDown | writeDown | readDCU | writeDCU
}

class OperationsBundle extends Bundle with HasOperationsBundle

class DecodeBundle(chiRespWidth: Int = 3, chiStateWidth: Int = 3) extends Bundle with HasOperationsBundle {
  // Commit(Resp to Rn Node)
  val respChnl    = UInt(CHIChannel.width.W)
  val respOp      = UInt(5.W)
  val resp        = UInt(chiRespWidth.W)
  val fwdState    = UInt(chiRespWidth.W)

  // Send Snoop to Slave Node
  val snpOp       = UInt(5.W)
  val retToSrc    = Bool()
//  val doNotGoToSD = Bool() // The default is true

  // Send Read or Write to Master Node
  val rdOp        = UInt(6.W)
  val wdOp        = UInt(6.W)

  // Write New State to Directory
  val hnState     = UInt(chiStateWidth.W)
  val srcState    = UInt(chiStateWidth.W)
  val othState    = UInt(chiStateWidth.W)

  def decode(inst: InstBundle, table: Seq[(UInt, UInt)]): DecodeBundle = {
    this := ParallelLookUp(
      inst.asUInt,
      table
    ).asTypeOf(new DecodeBundle(chiRespWidth, chiStateWidth))
//    this := Mux1H(table.map(_._1 === inst.asUInt), table.map(_._2)).asTypeOf(new DecodeBundle(chiRespWidth, chiStateWidth))
    this
  }
}

object Code {
  // Operations
  def Commit           : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.commit := true.B;     temp.asUInt }
  def Snoop            : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.snoop := true.B;      temp.asUInt }
  def ReadDown         : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.readDown := true.B;   temp.asUInt }
  def WriteDown        : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.writeDown := true.B;  temp.asUInt }
  def RDB2Src          : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.rDB2Src := true.B;    temp.asUInt }
  def CleanDB          : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.cleanDB := true.B;    temp.asUInt }
  def ReadDCU          : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.readDCU := true.B;    temp.asUInt }
  def WriteDCU         : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.writeDCU := true.B;   temp.asUInt }
  def WSDir            : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.wSDir := true.B;      temp.asUInt }
  def wSFDir           : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.wSFDir := true.B;     temp.asUInt }

  // other
  def RespChnl(x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.respChnl := x;        temp.asUInt }
  def RespOp  (x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.respOp := x;          temp.asUInt }
  def Resp    (x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.resp := x;            temp.asUInt }
  def FwdState(x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.fwdState := x;        temp.asUInt }
  def SnpOp   (x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.snpOp := x;           temp.asUInt }
  def retToSrc         : UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.retToSrc := true.B;   temp.asUInt }
  def RDOp    (x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.rdOp := x;            temp.asUInt }
  def WDOp    (x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.wdOp := x;            temp.asUInt }
  def HnState (x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.hnState := x;         temp.asUInt }
  def SrcState(x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.srcState := x;        temp.asUInt }
  def OthState(x: UInt): UInt = { val temp = WireInit(0.U.asTypeOf(new DecodeBundle())); temp.othState := x;        temp.asUInt }
}

