package fpu8

import chisel3._

class Convert(e5m2: Boolean) extends Module {
  val enable = IO(Input(UInt(1.W)))
  val a = IO(Input(new FloatingPoint(e5m2)))
  val roundingMode = IO(Input(UInt(2.W)))
  val saturationMode = IO(Input(UInt(1.W)))
  val z = IO(Output(UInt(8.W)))

  when(enable === 1.U){
    z := {
      if (e5m2) a.convert(roundingMode, saturationMode)
      else a.convert()
    }
  }.otherwise{
    z := 0.U
  }
}