package zhujiang

import xijiang.{Node, NodeType}
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import zhujiang.UnitTop.firtoolOpts
import zhujiang.device.bridge.axi.AxiBridge
import zhujiang.device.bridge.axilite.AxiLiteBridge
import zhujiang.device.bridge.chi.ChiSnBridge
import zhujiang.device.bridge.tlul.TLULBridge

object UnitTop {
  val firtoolOpts = Seq(FirtoolOption("-O=release"),
    FirtoolOption("--disable-all-randomization"),
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--strip-debug-info"),
    FirtoolOption("--lower-memories"),
    FirtoolOption("--add-vivado-ram-address-conflict-synthesis-bug-workaround"),
    FirtoolOption("--lowering-options=noAlwaysComb," +
      " disallowLocalVariables, disallowMuxInlining," +
      " emittedLineLength=120, explicitBitcast, locationInfoStyle=plain"))
}

object AxiBridgeTop extends App {
  val (config, firrtlOpts) = ZhujiangTopParser(args)
  (new ChiselStage).execute(firrtlOpts, firtoolOpts ++ Seq(
    ChiselGeneratorAnnotation(() => new AxiBridge(Node(nodeType = NodeType.S, outstanding = 8))(config))
  ))
}

object AxiLiteBridgeTop extends App {
  val (config, firrtlOpts) = ZhujiangTopParser(args)
  (new ChiselStage).execute(firrtlOpts, firtoolOpts ++ Seq(
    ChiselGeneratorAnnotation(() => new AxiLiteBridge(Node(nodeType = NodeType.HI, splitFlit = true, outstanding = 8), 64, 16)(config))
  ))
}

object TLULBridgeTop extends App {
  val (config, firrtlOpts) = ZhujiangTopParser(args)
  (new ChiselStage).execute(firrtlOpts, firtoolOpts ++ Seq(
    ChiselGeneratorAnnotation(() => new TLULBridge(Node(nodeType = NodeType.HI, splitFlit = true, outstanding = 8), 64, 16)(config))
  ))
}

object ChiSnBridgeTop extends App {
  val (config, firrtlOpts) = ZhujiangTopParser(args)
  (new ChiselStage).execute(firrtlOpts, firtoolOpts ++ Seq(
    ChiselGeneratorAnnotation(() => new ChiSnBridge(Node(nodeType = NodeType.HI, splitFlit = true, outstanding = 8))(config))
  ))
}
