package zhujiang.tilelink

import chisel3._

object AOpcode {
  val width = 3
  val PutFullData = 0x0.U(width.W)
  val PutPartialData = 0x1.U(width.W)
  val ArithmeticLogic = 0x2.U(width.W)
  val LogicalData = 0x3.U(width.W)
  val Get = 0x4.U(width.W)
  val Intent = 0x5.U(width.W)
  val AcquireBlock = 0x6.U(width.W)
  val AcquirePerm = 0x7.U(width.W)
}

object DOpcode {
  val width = 3
  val AccessAck = 0x0.U(width.W)
  val AccessAckData = 0x1.U(width.W)
  val HintAck = 0x2.U(width.W)
  val Grant = 0x3.U(width.W)
  val GrantData = 0x4.U(width.W)
  val ReleaseAck = 0x5.U(width.W)
}
