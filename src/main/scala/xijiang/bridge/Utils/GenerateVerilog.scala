package xijiang.bridge.Utils

import _root_.circt.stage.{ChiselStage, FirtoolOption}
import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation

object GenerateVerilog {
    def apply(args: Array[String], gen: () => RawModule, release: Boolean = false, name: String = "Unknown", split: Boolean = false) {

        var extraFirtoolOptions = Seq(FirtoolOption("--export-module-hierarchy"))
        if (split) {
            extraFirtoolOptions = extraFirtoolOptions ++ Seq(FirtoolOption("--split-verilog"), FirtoolOption("-o=./build/" + name))
        }

        val buildOpt = if (release) {
            FirtoolOption("-O=release")
        } else {
            FirtoolOption("-O=debug")
        }

        (new ChiselStage).execute(
            Array("--target", "verilog") ++ args,
            Seq(
                buildOpt,
                FirtoolOption("--disable-all-randomization"),
                FirtoolOption("--disable-annotation-unknown"),
                FirtoolOption("--strip-debug-info"),
                FirtoolOption("--lower-memories"),
                FirtoolOption(
                    "--lowering-options=noAlwaysComb," +
                        " disallowPortDeclSharing, disallowLocalVariables," +
                        " emittedLineLength=120, explicitBitcast, locationInfoStyle=plain," +
                        " disallowExpressionInliningInPorts, disallowMuxInlining"
                )
            ) ++ extraFirtoolOptions ++ Seq(ChiselGeneratorAnnotation(gen))
        )
    }
}

object MultiDontTouch {
    def apply[T <: Data](signals: T*): Unit = {
        signals.foreach(s => dontTouch(s))
    }
}
