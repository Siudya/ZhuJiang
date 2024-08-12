package xijiang.tfb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.chi.Flit
import zhujiang.{HasZJParams, ZJBundle, ZJParametersKey}

class FlitMonitorIO[T <: Flit](implicit p: Parameters) extends ZJBundle {
  val clock = Input(Clock())
  val valid = Input(Bool())
  val nodeId = Input(UInt(niw.W))
  val inject = Input(Bool())
  val flitType = Input(UInt(8.W))
  val flit = Input(UInt(maxFlitBits.W))
  val fault = Output(Bool())
}

class FlitMonitor()(implicit val p: Parameters) extends BlackBox with HasBlackBoxInline with HasZJParams {
  val io = IO(new FlitMonitorIO)
  private val modName = s"${p(ZJParametersKey).modulePrefix}TrafficBoardFlitMonitor"
  override val desiredName = modName

  setInline(s"$modName.sv",
    s"""
       |module $modName (
       |  input \t\t\t\t\tclock,
       |  input \t\t\t\t\tvalid,
       |  input \t\t\t\t\tinject,
       |  input  [7:0] \t\tflitType,
       |  input  [${niw - 1}:0] \t\tnodeId,
       |  input  [${maxFlitBits - 1}:0] \tflit,
       |  output \t\t\t\t\tfault
       |);
       |`ifndef SYNTHESIS
       |  import "DPI-C" function void tfb_flit_monitor (
       |    input  shortint \t\tnode_id,
       |    input  bit \t\t\t\t\tinject,
       |    input  byte \t\t\t\tflit_type,
       |    input  bit [${maxFlitBits - 1}:0] \tflit,
       |    output bit \t\t\t\t\tfault
       |  );
       |  wire [15:0] nid;
       |  assign nid = {${16 - niw}'h0, nodeId};
       |  always @(posedge clock) begin
       |    if(valid) tfb_flit_monitor(nid, inject, flitType, flit, fault);
       |  end
       |`else
       |  assign fault = 0;
       |`endif
       |endmodule""".stripMargin)
}
