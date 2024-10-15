package fpu8

import chisel3._
import chisel3.util.Cat

class Divide(val e5m2: Boolean) extends Module {
  val exponentLength = if (e5m2) 5 else 4
  val mantissaLength = if (e5m2) 2 else 3

  val enable = IO(Input(UInt(1.W)))
  val a = IO(Input(new FloatingPoint(e5m2)))
  val b = IO(Input(new FloatingPoint(e5m2)))
  val roundingMode = IO(Input(UInt(2.W)))
  val sign = IO(Output(UInt(1.W)))
  val exponent = IO(Output(UInt((exponentLength + 1).W)))
  val fraction = IO(Output(UInt((mantissaLength + 1).W)))
  val NaNFractionValue = IO(Output(UInt(1.W)))
  val status = IO(Output(UInt(5.W)))

  val (resultSign, resultExponent, resultFraction, resultStatus, resultNaNFractionValue) =
    (a / b)(roundingMode)

  when(enable === 1.U) {
    sign := resultSign
    exponent := resultExponent
    fraction := resultFraction
    status := resultStatus
    NaNFractionValue := resultNaNFractionValue
  }.otherwise {
    sign := 0.U
    exponent := 0.U
    fraction := 0.U
    status := 0.U
    NaNFractionValue := 0.U
  }
}
