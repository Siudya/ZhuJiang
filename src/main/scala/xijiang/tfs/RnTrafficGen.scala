package xijiang.tfs

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xijiang.Node
import xijiang.router.{RnRx, RnTx}
import zhujiang.chi.ChannelEncodings
import zhujiang.ZJModule

class RnTrafficGen(node: Node)(implicit p: Parameters) extends ZJModule {
  val io = IO(new Bundle {
    val rx = Flipped(new RnTx(node))
    val tx = Flipped(new RnRx(node))
    val nodeId = Input(UInt(niw.W))
  })
  private val reqTxGen = Module(new TrafficSimTx)
  private val respTxGen = Module(new TrafficSimTx)
  private val dataTxGen = Module(new TrafficSimTx)

  private val respRxGen = Module(new TrafficSimRx)
  private val dataRxGen = Module(new TrafficSimRx)
  private val snoopRxGen = Module(new TrafficSimRx)

  TrafficSimTx.connTfsTx(reqTxGen, io.tx.req, io.nodeId, ChannelEncodings.REQ.U, clock, reset)
  TrafficSimTx.connTfsTx(respTxGen, io.tx.resp, io.nodeId, ChannelEncodings.RSP.U, clock, reset)
  TrafficSimTx.connTfsTx(dataTxGen, io.tx.data, io.nodeId, ChannelEncodings.DAT.U, clock, reset)

  TrafficSimRx.connTfsRx(respRxGen, io.rx.resp, io.nodeId, ChannelEncodings.RSP.U, clock, reset)
  TrafficSimRx.connTfsRx(dataRxGen, io.rx.data, io.nodeId, ChannelEncodings.DAT.U, clock, reset)
  TrafficSimRx.connTfsRx(snoopRxGen, io.rx.snoop, io.nodeId, ChannelEncodings.SNP.U, clock, reset)
}
