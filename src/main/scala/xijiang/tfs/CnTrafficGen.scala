package xijiang.tfs

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xijiang.router.{CnTx, CnRx}
import zhujiang.ZJModule
import zhujiang.chi.ChannelEncodings

class CnTrafficGen(implicit p: Parameters) extends ZJModule {
  val io = IO(new Bundle {
    val rx = Flipped(new CnTx)
    val tx = Flipped(new CnRx)
    val nodeId = Input(UInt(niw.W))
  })
  private val reqTxGen = Module(new TrafficSimTx)
  private val respTxGen = Module(new TrafficSimTx)
  private val dataTxGen = Module(new TrafficSimTx)
  private val snoopTxGen = Module(new TrafficSimTx)

  private val reqRxGen = Module(new TrafficSimRx)
  private val respRxGen = Module(new TrafficSimRx)
  private val dataRxGen = Module(new TrafficSimRx)
  private val snoopRxGen = Module(new TrafficSimRx)

  TrafficSimTx.connTfsTx(reqTxGen, io.tx.req, io.nodeId, ChannelEncodings.REQ.U, clock, reset)
  TrafficSimTx.connTfsTx(respTxGen, io.tx.resp, io.nodeId, ChannelEncodings.RSP.U, clock, reset)
  TrafficSimTx.connTfsTx(dataTxGen, io.tx.data, io.nodeId, ChannelEncodings.DAT.U, clock, reset)
  TrafficSimTx.connTfsTx(snoopTxGen, io.tx.snoop, io.nodeId, ChannelEncodings.SNP.U, clock, reset)

  TrafficSimRx.connTfsRx(reqRxGen, io.rx.req, io.nodeId, ChannelEncodings.REQ.U, clock, reset)
  TrafficSimRx.connTfsRx(respRxGen, io.rx.resp, io.nodeId, ChannelEncodings.RSP.U, clock, reset)
  TrafficSimRx.connTfsRx(dataRxGen, io.rx.data, io.nodeId, ChannelEncodings.DAT.U, clock, reset)
  TrafficSimRx.connTfsRx(snoopRxGen, io.rx.snoop, io.nodeId, ChannelEncodings.SNP.U, clock, reset)
}
