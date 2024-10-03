import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import fpu8.{FPU8Generator, FPU8Top}

import java.io.PrintWriter

object Main extends App {
  val FPU8E4M3VerilogString = ChiselStage.emitSystemVerilog(new FPU8Generator(false))
  val FPU8E5M2VerilogString = ChiselStage.emitSystemVerilog(new FPU8Generator(true))
  val FPU8TopVerilogString = ChiselStage.emitSystemVerilog(new FPU8Top)

  val writer1 = new PrintWriter("fpu8_e4m3.v")
  writer1.write(FPU8E4M3VerilogString)
  writer1.close()

  val writer2 = new PrintWriter("fpu8_e5m2.v")
  writer2.write(FPU8E5M2VerilogString)
  writer2.close()

  val writer3 = new PrintWriter("fpu8_top.v")
  writer3.write(FPU8TopVerilogString)
  writer3.close()

  println("Verilog generated and written to fpu8_e4m3.v, fpu8_e5m2.v and fpu8_top.v")
  //println(ChiselStage.emitSystemVerilog(new FPU8Top))
}

