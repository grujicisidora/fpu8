import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import fpu8.{Add, Compare, Divide, FPU8Top, GenerateFinalResult, Multiply}

object Main extends App {
  //println(ChiselStage.emitSystemVerilog(new Add(false)))
  //println(ChiselStage.emitSystemVerilog(new TreeMultiplier(4, 3)))
  //println(ChiselStage.emitSystemVerilog(new Divide(false)))
  //println(ChiselStage.emitSystemVerilog(new Multiply(false)))
  //println(ChiselStage.emitSystemVerilog(new Compare(false)))
  //println(ChiselStage.emitSystemVerilog(new GenerateFinalResult(false)))
  println(ChiselStage.emitSystemVerilog(new FPU8Top))
}

