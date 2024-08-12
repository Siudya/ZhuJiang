package xijiang.tfb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.{HasZJParams, ZJParametersKey}

class NodeRegister(implicit val p: Parameters) extends BlackBox with HasBlackBoxInline with HasZJParams {
  val io = IO(new Bundle {
    val nodeId = Input(UInt(niw.W))
  })
  private val modName = s"${p(ZJParametersKey).modulePrefix}TrafficBoardNodeRegister"
  override val desiredName = modName
  setInline(s"$modName.sv",
    s"""
       |module $modName (
       |  input  [${niw - 1}:0] \t\tnodeId
       |);
       |`ifndef SYNTHESIS
       |  import "DPI-C" function void tfb_register_node (
       |    input  shortint \t\tnode_id
       |  );
       |  wire [15:0] nid;
       |  assign nid = {${16 - niw}'h0, nodeId};
       |  initial tfb_register_node(nid);
       |`endif
       |endmodule""".stripMargin)
}
