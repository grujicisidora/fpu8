package fpu8

import chisel3._
import chisel3.util._

class FunctionTester(len1: Int, len2: Int) extends Module {
  val a = IO(Input(UInt(len1.W)))
  //val b = IO(Input(UInt(4.W)))
  //val c = IO(Input(UInt(len1.W)))
  //val d = IO(Input(UInt(len2.W)))
  val z = IO(Output(UInt((len2).W)))
  //val y = IO(Output(UInt(length.W)))

  val roundedValue = roundValue(a, len2)

  z := roundedValue

  def roundValue(value: UInt, resLength: Int): UInt = {
    val valueLength = value.getWidth
    assert(valueLength >= resLength, "Invalid resLength parameter.")
    val msbIndex = valueLength - 1
    val lsbIndex = valueLength - resLength
    val roundedValue = {
      if (lsbIndex - 3 >= 0) {
        value(msbIndex, lsbIndex) +& (value(lsbIndex - 1) & value(lsbIndex - 2, lsbIndex - 3).orR.asUInt)
      } else
        Cat(0.U, value(msbIndex, lsbIndex))
    }
    roundedValue
  }
}
