package DONGJIANG.DECODE

import DONGJIANG._
import DONGJIANG.CHI._
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
  val Snp_RD        = Snp | RD
  val SnpFwd_RN     = SnpFwd | RD
}


class InstBundle(implicit p: Parameters) extends DJBundle {
  val opcode    = UInt(6.W)
  val srcState  = UInt(ChiState.width.W)
  val othState  = UInt(ChiState.width.W)
  val hnState   = UInt(ChiState.width.W)
  val respType  = UInt(RespType.width.W)
  val snpResp   = UInt(ChiResp.width.W)
  val fwdState  = UInt(ChiResp.width.W)
  val rdResp    = UInt(ChiResp.width.W) // Read Down
}


trait HasOperationsBundle extends Bundle {
  // Commit(Resp to Rn Node)
  val commit      = Bool()

  // Send Snoop to Slave Node
  val snoop       = Bool()

  // Send Read to Slave Node
  val readDown    = Bool()

  // Read DataBuffer to Send Data to Resp Node
  val rcDB        = Bool() // Read and Clean DataBuffer

  // Read(Send Data to Resp Node) or Write DataStorage
  val readDS      = Bool()
  val writeDS     = Bool()

  // Write New State to Directory
  val wSDir       = Bool()
  val wCDir       = Bool()
}

class OperationsBundle extends Bundle with HasOperationsBundle

class DecodeBundle(chiRespWidth: Int, chiStateWidth: Int) extends Bundle with HasOperationsBundle {
  // Commit(Resp to Rn Node)
  val respOp      = UInt(5.W)
  val resp        = UInt(chiRespWidth.W)
  val fwdState    = UInt(chiRespWidth.W)

  // Send Snoop to Slave Node
  val snpOp       = UInt(5.W)

  // Send Read to Slave Node
  val rdOp        = UInt(6.W)

  // Read DataBuffer to Send Data to Resp Node

  // Read(Send Data to Resp Node) or Write DataStorage

  // Write New State to Directory
  val hnState     = UInt(chiStateWidth.W)
  val srcState    = UInt(chiStateWidth.W)
  val othState    = UInt(chiStateWidth.W)


  def decode(inst: InstBundle, table: Seq[(UInt, UInt)]): DecodeBundle = {
    this := ParallelLookUp(
      inst.asUInt,
      table
    ).asTypeOf(new DecodeBundle(chiRespWidth, chiStateWidth))
    this
  }
}

object Code {
  // Operations
  def Commit           : UInt = Cat(1.U,  0.U(0.W))
  def Snoop            : UInt = Cat(1.U,  0.U(Commit.getWidth.W))
  def ReadDown         : UInt = Cat(1.U,  0.U(Snoop.getWidth.W))
  def RCDB             : UInt = Cat(1.U,  0.U(ReadDown.getWidth.W))
  def ReadDS           : UInt = Cat(1.U,  0.U(RCDB.getWidth.W))
  def WriteDS          : UInt = Cat(1.U,  0.U(ReadDS.getWidth.W))
  def WSDir            : UInt = Cat(1.U,  0.U(WriteDS.getWidth.W))
  def WCDir            : UInt = Cat(1.U,  0.U(WSDir.getWidth.W))

  // other
  def RespOp  (x: UInt): UInt = Cat(x,    0.U(WCDir.getWidth.W))
  def Resp    (x: UInt): UInt = Cat(x,    0.U(RespOp(CHIOp.RSP.CompAck).getWidth.W))
  def FwdState(x: UInt): UInt = Cat(x,    0.U(Resp(ChiResp.I).getWidth.W))
  def SnpOp   (x: UInt): UInt = Cat(x,    0.U(FwdState(ChiResp.I).getWidth.W))
  def RDOp    (x: UInt): UInt = Cat(x,    0.U(SnpOp(CHIOp.SNP.SnpOnce).getWidth.W))
  def HnState (x: UInt): UInt = Cat(x,    0.U(RDOp(CHIOp.REQ.ReadNoSnp).getWidth.W))
  def SrcState(x: UInt): UInt = Cat(x,    0.U(HnState(ChiState.I).getWidth.W))
  def OthState(x: UInt): UInt = Cat(x,    0.U(SrcState(ChiState.I).getWidth.W))
}

