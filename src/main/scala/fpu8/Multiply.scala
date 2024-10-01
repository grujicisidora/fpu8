package fpu8

import chisel3._

class Multiply (val e5m2: Boolean) extends Module {
  val exponentLength = if (e5m2) 5 else 4
  val mantissaLength = if (e5m2) 2 else 3

  val enable = IO(Input(UInt(1.W)))
  val a = IO(Input(new FloatingPoint(e5m2)))
  val b = IO(Input(new FloatingPoint(e5m2)))
  val roundingMode = IO(Input(UInt(2.W)))
  //val saturationMode = IO(Input(UInt(1.W)))
  //val z = IO(Output(UInt(8.W)))
  val sign = IO(Output(UInt(1.W)))
  val exponent = IO(Output(UInt((exponentLength + 1).W)))
  val fraction = IO(Output(UInt((mantissaLength + 1).W)))
  val overflow = IO(Output(UInt(1.W)))
  val isInfty = IO(Output(UInt(1.W)))
  val is0 = IO(Output(UInt(1.W)))
  val isNaN = IO(Output(UInt(1.W)))
  val NaNFractionValue = IO(Output(UInt(1.W)))

  val (resultSign, resultExponent, resultFraction, resultOverflow, isResultInfty, isResult0, isResultNaN, resultNaNFractionValue) =
    (a * b)(roundingMode)

  when(enable === 1.U) {
    sign := resultSign
    exponent := resultExponent
    fraction := resultFraction
    overflow := resultOverflow
    isInfty := isResultInfty
    is0 := isResult0
    isNaN := isResultNaN
    NaNFractionValue := resultNaNFractionValue
  }.otherwise {
    sign := 0.U
    exponent := 0.U
    fraction := 0.U
    overflow := 0.U
    isInfty := 0.U
    is0 := 0.U
    isNaN := 0.U
    NaNFractionValue := 0.U
  }
}