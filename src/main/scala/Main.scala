import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import fpu8.Add

object Main extends App {
  println(ChiselStage.emitSystemVerilog(new Add(0)))
}
