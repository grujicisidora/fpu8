package fpu8

import chisel3._

class Compare(val e5m2: Boolean) extends Module {
  val enable = IO(Input(UInt(1.W)))
  val compareMode = IO(Input(UInt(3.W)))
  val a = IO(Input(new FloatingPoint(e5m2)))
  val b = IO(Input(new FloatingPoint(e5m2)))
  val z = IO(Output(UInt(8.W)))
  val status = IO(Output(UInt(5.W)))

  val isResultNaN = Wire(Bool())

  when(enable === 1.U){
    when(compareMode === 0.U) {
      z := a < b
      isResultNaN := a.isNaN || b.isNaN
    }.elsewhen(compareMode === 1.U) {
      z := a > b
      isResultNaN := a.isNaN || b.isNaN
    }.elsewhen(compareMode === 2.U) {
      z := a == b
      isResultNaN := a.isNaN || b.isNaN
    }.elsewhen(compareMode === 3.U) {
      z := a <= b
      isResultNaN := a.isNaN || b.isNaN
    }.elsewhen(compareMode === 4.U) {
      z := a >= b
      isResultNaN := a.isNaN || b.isNaN
    }.elsewhen(compareMode === 5.U){
      z := a != b
      isResultNaN := a.isNaN && b.isNaN
    }.otherwise {
      z := 0.U
      isResultNaN := 0.U
    }

    when(isResultNaN){
      status := 4.U
    }.otherwise{
      status := 0.U
    }
  }.otherwise{
    z := 0.U
    isResultNaN := 0.U
    status := 0.U
  }

}
