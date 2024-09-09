package xijiang.tfs

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xijiang.Node
import xijiang.router.{HniRx, HniTx}
import zhujiang.chi.ChannelEncodings
import zhujiang.ZJModule

class HniTrafficGen(local: Boolean, node: Node)(implicit p: Parameters) extends ZJModule {
  val io = IO(new Bundle {
    val rx = Flipped(new HniTx(node))
    val tx = Flipped(new HniRx(local, node))
    val nodeId = Input(UInt(niw.W))
  })
  private val respTxGen = Module(new TrafficSimTx)
  private val dataTxGen = Module(new TrafficSimTx)
  private val ereqTxGen = if(local) Some(Module(new TrafficSimTx)) else None

  private val reqRxGen = Module(new TrafficSimRx)
  private val respRxGen = Module(new TrafficSimRx)
  private val dataRxGen = Module(new TrafficSimRx)

  TrafficSimTx.connTfsTx(respTxGen, io.tx.resp, io.nodeId, ChannelEncodings.RSP.U, clock, reset)
  TrafficSimTx.connTfsTx(dataTxGen, io.tx.data, io.nodeId, ChannelEncodings.DAT.U, clock, reset)
  if(local) TrafficSimTx.connTfsTx(ereqTxGen.get, io.tx.req.get, io.nodeId, ChannelEncodings.ERQ.U, clock, reset)

  TrafficSimRx.connTfsRx(respRxGen, io.rx.resp, io.nodeId, ChannelEncodings.RSP.U, clock, reset)
  TrafficSimRx.connTfsRx(dataRxGen, io.rx.data, io.nodeId, ChannelEncodings.DAT.U, clock, reset)
  TrafficSimRx.connTfsRx(reqRxGen, io.rx.req, io.nodeId, ChannelEncodings.REQ.U, clock, reset)
}
