package zhujiang

import org.chipsalliance.cde.config.{Config, Parameters}
import xijiang.{NodeParam, NodeType}
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import xijiang.tfb.TrafficBoardFileManager
import xs.utils.FileRegisters

import scala.annotation.tailrec

class ZhujiangTopConfig extends Config((site, here, up) => {
  case ZJParametersKey => ZJParameters(
    localNodeParams = Seq(
      NodeParam(nodeType = NodeType.RF, splitFlit = true),
      NodeParam(nodeType = NodeType.RF, splitFlit = true),
      NodeParam(nodeType = NodeType.HF, splitFlit = true),
      NodeParam(nodeType = NodeType.HI, splitFlit = true),
      NodeParam(nodeType = NodeType.S, splitFlit = true),
      NodeParam(nodeType = NodeType.S, splitFlit = true),
      NodeParam(nodeType = NodeType.S, splitFlit = true, mainMemory = true)
    )
  )
})

object ZhujiangTopParser {
  def apply(args: Array[String]): (Parameters, Array[String]) = {
    val defaultConfig = new ZhujiangTopConfig
    var firrtlOpts = Array[String]()
    var hasHelp: Boolean = false

    @tailrec
    def parse(config: Parameters, args: List[String]): Parameters = {
      args match {
        case Nil => config

        case "--help" :: tail =>
          hasHelp = true
          parse(config, tail)

        case "--prefix" :: confString :: tail =>
          parse(config.alter((site, here, up) => {
            case ZJParametersKey => up(ZJParametersKey).copy(modulePrefix = confString)
          }), tail)

        case option :: tail =>
          firrtlOpts :+= option
          parse(config, tail)
      }
    }

    val cfg = parse(defaultConfig, args.toList)
    if(hasHelp) firrtlOpts :+= "--help"
    (cfg, firrtlOpts)
  }
}

object ZhujiangTop extends App {
  val (config, firrtlOpts) = ZhujiangTopParser(args)
  (new ChiselStage).execute(firrtlOpts, Seq(
    FirtoolOption("-O=release"),
    FirtoolOption("--disable-all-randomization"),
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--strip-debug-info"),
    FirtoolOption("--lower-memories"),
    FirtoolOption("--add-vivado-ram-address-conflict-synthesis-bug-workaround"),
    FirtoolOption("--lowering-options=noAlwaysComb," +
      " disallowLocalVariables, disallowMuxInlining," +
      " emittedLineLength=120, explicitBitcast, locationInfoStyle=plain"),
    ChiselGeneratorAnnotation(() => new Zhujiang()(config))
  ))
  if(config(ZJParametersKey).tfbParams.isDefined) TrafficBoardFileManager.release(config)
  FileRegisters.write()
}
