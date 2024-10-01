import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import fpu8.{Add, Divide, Multiply, Compare}

object Main extends App {
  //println(ChiselStage.emitSystemVerilog(new Add(false)))
  //println(ChiselStage.emitSystemVerilog(new TreeMultiplier(4, 3)))
  //println(ChiselStage.emitSystemVerilog(new Divide(false)))
  println(ChiselStage.emitSystemVerilog(new Multiply(false)))
  //println(ChiselStage.emitSystemVerilog(new Compare(false)))
}

