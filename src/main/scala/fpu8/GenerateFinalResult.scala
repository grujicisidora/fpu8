package fpu8

import chisel3._
import chisel3.util.Cat

class GenerateFinalResult(val e5m2: Boolean) extends Module {
  val exponentLength = if (e5m2) 5 else 4
  val mantissaLength = if (e5m2) 2 else 3

  def finalResult(overflow: Bool, sign: UInt, finalExponent: UInt, finalFraction: UInt)(roundingMode: UInt, saturationMode: UInt, isInfty: Bool, is0: Bool, isNaN: Bool): UInt = {
    val z = Wire(UInt(8.W))
    when(!isInfty && !is0 && !isNaN) {
      when(overflow) {
        when(roundingMode === 0.U && saturationMode === 0.U) {
          z := Cat(sign, ((1 << exponentLength) - 1).U(exponentLength.W), 0.U(mantissaLength.W))
        }.elsewhen((roundingMode === 0.U && saturationMode === 1.U) || roundingMode === 3.U) {
          z := Cat(sign, ((1 << (exponentLength - 1)) - 1).U((exponentLength - 1).W), 0.U, ((1 << mantissaLength) - 1).U(mantissaLength.W))
        }.elsewhen(roundingMode === 1.U && saturationMode === 0.U && sign === 0.U) {
          z := Cat(1.U, ((1 << exponentLength) - 1).U(exponentLength.W))
        }.elsewhen((roundingMode === 1.U && saturationMode === 1.U && sign === 0.U) || (roundingMode === 2.U && sign === 1.U)) {
          z := Cat(1.U, ((1 << (exponentLength - 1)) - 1).U((exponentLength - 1).W), 0.U, ((1 << mantissaLength) - 1).U(mantissaLength.W))
        }.elsewhen((roundingMode === 1.U && sign === 1.U) || (roundingMode === 2.U && saturationMode === 1.U && sign === 0.U)) {
          z := Cat(0.U, ((1 << (exponentLength - 1)) - 1).U((exponentLength - 1).W), 0.U, ((1 << mantissaLength) - 1).U(mantissaLength.W))
        }.elsewhen(roundingMode === 2.U && saturationMode === 0.U && sign === 0.U) {
          z := Cat(0.U, ((1 << exponentLength) - 1).U(exponentLength.W), 0.U(mantissaLength.W))
        }.otherwise {
          z := 0.U
        }
      }.otherwise {
        z := Cat(sign, finalExponent(exponentLength - 1, 0), finalFraction(mantissaLength - 1, 0))
      }
    }.elsewhen(isInfty && !is0 && !isNaN) {
      z := Cat(sign, ((1 << exponentLength) - 1).U(exponentLength.W), 0.U(mantissaLength.W))
    }.elsewhen(!isInfty && is0 && !isNaN) {
      z := Cat(sign, 0.U(exponentLength.W), 0.U(mantissaLength.W))
    }.elsewhen(isNaN) {
      z := Cat(0.U, ((1 << exponentLength) - 1).U(exponentLength.W), ((1 << mantissaLength) - 1).U(mantissaLength.W))
    }.otherwise {
      z := 0.U
    }
    z
  }

}
