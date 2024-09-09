package fpu8

import chisel3._

class Add (val format: Int) extends Module {
  val enable = IO(Input(UInt(1.W)))
  val a = IO(Input(new FloatingPoint(format)))
  val b = IO(Input(new FloatingPoint(format)))
  val roundingMode = IO(Input(UInt(2.W)))
  val saturationMode = IO(Input(UInt(1.W)))
  val z = IO(Output(UInt(8.W)))

  val result = a.+(b)(roundingMode, saturationMode)

  when(enable === 1.U) {
    z := result
  }.otherwise {
    z := 0.U
  }
}
