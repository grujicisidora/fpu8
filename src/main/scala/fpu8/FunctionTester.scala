package fpu8

import chisel3._
import chisel3.util._

class FunctionTester(val length: Int) extends Module {
  val a = IO(Input(UInt(length.W)))
  //val b = IO(Input(UInt(length.W)))
  val z = IO(Output(UInt(length.W)))
  val y = IO(Output(UInt(length.W)))

  val (shiftedValue, leadingZeros) = shiftToMSB(a)

  z := shiftedValue
  y := leadingZeros

  def shiftToMSB(input: UInt): (UInt, UInt) = {
    val width = input.getWidth
    val leadingZeros = Wire(UInt(width.W))
    val shiftedValue = Wire(UInt(width.W))

    leadingZeros := PriorityEncoder(Reverse(input))

    shiftedValue := input << leadingZeros

    (shiftedValue, leadingZeros)
  }

}
