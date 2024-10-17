package fpu8

import chisel3._

class Compare(val e5m2: Boolean) extends Module {
  val enable = IO(Input(UInt(1.W)))
  val compareMode = IO(Input(UInt(3.W)))
  val a = IO(Input(new FloatingPoint(e5m2)))
  val b = IO(Input(new FloatingPoint(e5m2)))
  val z = IO(Output(UInt(8.W)))
  val status = IO(Output(UInt(5.W)))

  val result = Wire(UInt(8.W))
  val isNaN = Wire(Bool())
  val is0 = Wire(Bool())

  when(enable === 1.U){
    when(compareMode === 0.U) {
      result := a < b
    }.elsewhen(compareMode === 1.U) {
      result := a > b
    }.elsewhen(compareMode === 2.U) {
      result := a == b
    }.elsewhen(compareMode === 3.U) {
      result := a <= b
    }.elsewhen(compareMode === 4.U) {
      result := a >= b
    }.elsewhen(compareMode === 5.U){
      result := a != b
    }.otherwise {
      result := 0.U
    }

    isNaN := a.isNaN || b.isNaN
    is0 := a.is0 && b.is0

    when(isNaN){
      status := 4.U
      z := Mux(compareMode === 5.U && a.isNaN && b.isNaN, result, 0.U(8.W))
    }.elsewhen(is0) {
      status := 1.U
      z := result
    }.otherwise{
      status := 0.U
      z := result
    }
  }.otherwise{
    result := 0.U
    z := 0.U
    isNaN := 0.U
    is0 := 0.U
    status := 0.U
  }

}
