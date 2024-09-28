package fpu8

import chisel3._

class Compare(val e5m2: Boolean) extends Module {
  val enable = IO(Input(UInt(1.W)))
  val compareMode = IO(Input(UInt(3.W)))
  val a = IO(Input(new FloatingPoint(e5m2)))
  val b = IO(Input(new FloatingPoint(e5m2)))
  val z = IO(Output(UInt(8.W)))

  when(enable === 1.U){
    when(compareMode === 0.U) {
      z := a < b
    }.elsewhen(compareMode === 1.U) {
      z := a > b
    }.elsewhen(compareMode === 2.U) {
      z := a == b
    }.elsewhen(compareMode === 3.U) {
      z := a <= b
    }.elsewhen(compareMode === 4.U) {
      z := a >= b
    }.otherwise {
      z := a != b
    }
  }.otherwise{
   z := 0.U
  }
}
