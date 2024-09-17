import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import fpu8.{Add, Divide, TreeMultiplier}

object Main extends App {
  //println(ChiselStage.emitSystemVerilog(new Add(0)))
  //println(ChiselStage.emitSystemVerilog(new TreeMultiplier(4, 3)))
  println(ChiselStage.emitSystemVerilog(new Divide(0)))
}
