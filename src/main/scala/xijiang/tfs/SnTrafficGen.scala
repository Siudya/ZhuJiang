package xijiang.tfs

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.{SnRx, SnTx}
import zhujiang.chi.ChannelEncodings
import zhujiang.ZJModule

class SnTrafficGen(implicit p: Parameters) extends ZJModule {
  val io = IO(new Bundle {
    val rx = Flipped(new SnTx)
    val tx = Flipped(new SnRx)
    val nodeId = Input(UInt(niw.W))
  })
  private val respTxGen = Module(new TrafficSimTx)
  private val dataTxGen = Module(new TrafficSimTx)

  private val reqRxGen = Module(new TrafficSimRx)
  private val dataRxGen = Module(new TrafficSimRx)

  TrafficSimTx.connTfsTx(respTxGen, io.tx.req, io.nodeId, ChannelEncodings.ERQ.U, clock, reset)
  TrafficSimTx.connTfsTx(dataTxGen, io.tx.data, io.nodeId, ChannelEncodings.DAT.U, clock, reset)

  TrafficSimRx.connTfsRx(dataRxGen, io.rx.data, io.nodeId, ChannelEncodings.DAT.U, clock, reset)
  TrafficSimRx.connTfsRx(reqRxGen, io.rx.resp, io.nodeId, ChannelEncodings.RSP.U, clock, reset)
}
