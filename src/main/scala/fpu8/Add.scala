package fpu8

import chisel3._

class Add (val e5m2: Boolean) extends Module {
  val enable = IO(Input(UInt(1.W)))
  val a = IO(Input(new FloatingPoint(e5m2)))
  val b = IO(Input(new FloatingPoint(e5m2)))
  val roundingMode = IO(Input(UInt(2.W)))
  val saturationMode = IO(Input(UInt(1.W)))
  val subtract = IO(Input(UInt(1.W)))
  val z = IO(Output(UInt(8.W)))

  val result = a.+(b)(roundingMode, saturationMode, subtract)

  when(enable === 1.U) {
    z := result
  }.otherwise {
    z := 0.U
  }
}
