package fpu8

import chisel3._
import chisel3.util.Cat

class GenerateFinalResult(val e5m2: Boolean) extends Module {
  val exponentLength = if (e5m2) 5 else 4
  val mantissaLength = if (e5m2) 2 else 3
  val maxExponent = if (e5m2) 31 else 15
  val maxMantissa = if (e5m2) 3 else 7

  val enable = IO(Input(UInt(1.W)))
  val sign = IO(Input(UInt(1.W)))
  val exponent = IO(Input(UInt(exponentLength.W)))
  val mantissa = IO(Input(UInt(mantissaLength.W)))
  val roundingMode = IO(Input(UInt(2.W)))
  val overflow = IO(Input(UInt(1.W)))
  val saturationMode = IO(Input(UInt(1.W)))
  val isInfty = IO(Input(UInt(1.W)))
  val is0 = IO(Input(UInt(1.W)))
  val isNaN = IO(Input(UInt(1.W)))
  val NaNFractionValue = IO(Input(UInt(1.W)))
  val z = IO(Output(UInt(8.W)))

  val result = {
    if (e5m2) finalResult(sign, exponent, mantissa, roundingMode, overflow.asBool, saturationMode,
      isInfty.asBool, is0.asBool, isNaN.asBool, NaNFractionValue)
    else finalResult(sign, exponent, mantissa, roundingMode, overflow.asBool, saturationMode, is0.asBool, isNaN.asBool)
  }

  when(enable === 1.U){
    z := result
  }.otherwise{
    z := 0.U
  }

  def finalResult(sign: UInt, exponent: UInt, mantissa: UInt, roundingMode: UInt, overflow: Bool,
                  saturationMode: UInt, is0: Bool, isNaN: Bool): UInt = {
    val z = Wire(UInt(8.W))
    when(!is0 && !isNaN) {
      when(overflow) {
        when((roundingMode === 0.U && saturationMode === 0.U) ||
          ((roundingMode === 1.U || roundingMode === 2.U) && saturationMode === 0.U && sign === 0.U)) {
          z := Cat(0.U, maxExponent.U(exponentLength.W), maxMantissa.U(mantissaLength.W))
        }.elsewhen((roundingMode === 0.U && saturationMode === 1.U) || (roundingMode === 3.U && saturationMode === 0.U)) {
          z := Cat(sign, maxExponent.U(exponentLength.W), (maxMantissa - 1).U(mantissaLength.W))
        }.elsewhen((roundingMode === 1.U && saturationMode === 1.U && sign === 0.U) || (roundingMode === 2.U && sign === 1.U)) {
          z := Cat(1.U, maxExponent.U(exponentLength.W), (maxMantissa - 1).U(mantissaLength.W))
        }.elsewhen((roundingMode === 1.U && sign === 1.U) || (roundingMode === 2.U && saturationMode === 1.U && sign === 0.U)) {
          z := Cat(0.U, maxExponent.U(exponentLength.W), (maxMantissa - 1).U(mantissaLength.W))
        }.otherwise {
          z := 0.U
        }
      }.otherwise {
        z := Cat(sign, exponent, mantissa)
      }
    }.elsewhen(is0 && !isNaN) {
      z := Cat(0.U, 0.U(exponentLength.W), 0.U(mantissaLength.W))
    }.elsewhen(isNaN) {
      z := Cat(0.U, maxExponent.U(exponentLength.W), maxMantissa.U(mantissaLength.W))
    }.otherwise {
      z := 0.U
    }
    z
  }

  def finalResult(sign: UInt, exponent: UInt, mantissa: UInt, roundingMode: UInt, overflow: Bool,
                  saturationMode: UInt, isInfty: Bool, is0: Bool, isNaN: Bool, NaNFractionValue: UInt): UInt = {
    val z = Wire(UInt(8.W))
    when(!isInfty && !is0 && !isNaN) {
      when(overflow) {
        when(roundingMode === 0.U && saturationMode === 0.U) {
          z := Cat(sign, maxExponent.U(exponentLength.W), 0.U(mantissaLength.W))
        }.elsewhen((roundingMode === 0.U && saturationMode === 1.U) || roundingMode === 3.U) {
          z := Cat(sign, (maxExponent - 1).U(exponentLength.W), maxMantissa.U(mantissaLength.W))
        }.elsewhen(roundingMode === 1.U && saturationMode === 0.U && sign === 0.U) {
          z := Cat(1.U, maxExponent.U(exponentLength.W), 0.U(mantissaLength.W))
        }.elsewhen((roundingMode === 1.U && saturationMode === 1.U && sign === 0.U) || (roundingMode === 2.U && sign === 1.U)) {
          z := Cat(1.U, (maxExponent - 1).U(exponentLength.W), maxMantissa.U(mantissaLength.W))
        }.elsewhen((roundingMode === 1.U && sign === 1.U) || (roundingMode === 2.U && saturationMode === 1.U && sign === 0.U)) {
          z := Cat(0.U, (maxExponent - 1).U(exponentLength.W), maxMantissa.U(mantissaLength.W))
        }.elsewhen(roundingMode === 2.U && saturationMode === 0.U && sign === 0.U) {
          z := Cat(0.U, maxExponent.U(exponentLength.W), 0.U(mantissaLength.W))
        }.otherwise {
          z := 0.U
        }
      }.otherwise {
        z := Cat(sign, exponent, mantissa)
      }
    }.elsewhen(isInfty && !is0 && !isNaN) {
      z := Cat(sign, maxExponent.U(exponentLength.W), 0.U(mantissaLength.W))
    }.elsewhen(!isInfty && is0 && !isNaN) {
      z := Cat(sign, 0.U(exponentLength.W), 0.U(mantissaLength.W))
    }.elsewhen(isNaN) {
      z := Cat(0.U, maxExponent.U(exponentLength.W), 1.U, NaNFractionValue)
    }.otherwise {
      z := 0.U
    }
    z
  }
}
