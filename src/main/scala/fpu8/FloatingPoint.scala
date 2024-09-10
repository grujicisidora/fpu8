package fpu8

import chisel3._
import chisel3.util._

import scala.math.pow

class FloatingPoint(format: Int) extends Bundle {
  val data = Input(UInt(8.W))

  val exponentLength = {
    if(format == 0) 4
    else 5
  }

  val mantissaLength = 7 - exponentLength

  val maxExponent = pow(2, exponentLength).toInt - 1

  val maxMantissa = pow(2, mantissaLength).toInt - 1

  def sign: UInt = data(7)

  def exponent: UInt = data(6, 7 - exponentLength)

  def mantissa: UInt = data(mantissaLength - 1, 0)

  def isGreater(other: FloatingPoint): Bool = this.data(6, 0) > other.data(6, 0)

  def isEqualTo(other: FloatingPoint): Bool = this.data(6, 0) === other.data(6, 0)

  def isExp0: Bool = exponent === 0.U

  def isExpMax: Bool = exponent === maxExponent.U

  def isMantissa0: Bool = mantissa === 0.U

  def isMantissaMax: Bool = mantissa === maxMantissa.U

  def isNaN: Bool = isExpMax && !isMantissa0

  def isInfty: Bool = isExpMax && isMantissa0

  def is0: Bool = isExp0 && isMantissa0

  // ova funkcija moze da se sredi i iskoristi u okviru funkcije normalize
  def shiftToMSB1(value: UInt): (UInt, UInt) = {
    val shift4places = {
      if (mantissaLength + 1 <= 3) false.B
      else !value(mantissaLength, mantissaLength - 3).orR
    }

    val firstShift = {
      if (shift4places == true.B) {
        if (mantissaLength == 3) Fill(4, 0.U)
        else if (mantissaLength == 4) Cat(value(0), Fill(4, 0.U))
        else Cat(value(mantissaLength - 3, 0), Fill(4, 0.U))
      }
      else value
    }

    val shift2places = !firstShift(mantissaLength, mantissaLength - 1).orR

    val secondShift = {
      if (shift2places == true.B) {
        if (mantissaLength == 2) Cat(firstShift(0), Fill(2, 0.U))
        else Cat(firstShift(mantissaLength - 2, 0), Fill(2, 0.U))
      }
      else firstShift
    }

    val shift1place = !secondShift(mantissaLength)

    val shiftedValue = {
      if (shift1place == true.B) Cat(secondShift(mantissaLength - 1, 0), 0.U)
      else secondShift
    }

    val shiftPlaces = 0.U + {
      if (shift4places == true.B) 4.U
      else 0.U
    } + {
      if (shift2places == true.B) 2.U
      else 0.U
    } + {
      if (shift1place == true.B) 1.U
      else 0.U
    }

    (shiftedValue, shiftPlaces)
  }

  def alignForAddition(other: FloatingPoint)(subtract: UInt): (UInt, UInt, UInt, UInt, UInt) = {
    val compare = this.isGreater(other)
    val greaterOperand = Mux(compare, this, other)
    val smallerOperand = Mux(compare, other, this)
    val sign = Mux(compare, this.sign, other.sign ^ subtract)
    val subtraction = subtract ^ greaterOperand.sign ^ smallerOperand.sign
    val greaterExp = greaterOperand.exponent
    val greaterOperandFraction = Cat(!greaterOperand.isExp0, greaterOperand.mantissa)
    val smallerOperandFraction = Cat(!smallerOperand.isExp0, smallerOperand.mantissa)
    val isOnlySmallerDenormalized = !greaterOperand.isExp0 && smallerOperand.isExp0
    val shift = Mux(
      isOnlySmallerDenormalized,
      greaterOperand.exponent - smallerOperand.exponent - 1.U,
      greaterOperand.exponent - smallerOperand.exponent
    )
    val shiftedFraction = {
      val maxShift = mantissaLength + 3
      val shifted = Mux(
        shift >= maxShift.U,
        Cat(Fill(maxShift, 0.U), smallerOperandFraction),
        Cat(smallerOperandFraction, Fill(maxShift, 0.U)) >> shift
      )
      Cat(shifted(maxShift + mantissaLength, mantissaLength + 1), shifted(mantissaLength, 0).orR.asUInt)
    }
    val paddedGreaterOperandFraction = Cat(0.U, greaterOperandFraction, Fill(3, 0.U))
    val paddedSmallerOperandFraction = Cat(0.U, shiftedFraction)
    (sign, paddedGreaterOperandFraction, paddedSmallerOperandFraction, greaterExp, subtraction)
  }

  def alignForDivision(other: FloatingPoint): (UInt, UInt, UInt, UInt) = {
    val sign = this.sign ^ other.sign
    val tempDividendFraction = Mux(this.isExp0,
      Cat(this.mantissa, 0.U),
      Cat(1.U, this.mantissa))
    val tempDivisorFraction = Mux(other.isExp0,
      Cat(other.mantissa, 0.U),
      Cat(1.U, other.mantissa))

    val (dividendFraction, dividendShift) = shiftToMSB1(tempDividendFraction)
    val (divisorFraction, divisorShift) = shiftToMSB1(tempDivisorFraction)

    val tempExponent = Cat(Fill(2, 0.U), this.exponent) -&
      Cat(Fill(2, 0.U), other.exponent) +&
      Fill(mantissaLength, 1.U) // dodaje se bias na stvarni eksponent

    val exponent = tempExponent -& dividendShift +& divisorShift

    (sign, dividendFraction, divisorFraction, exponent)
  }

  def normalize(sign: UInt, exponent: UInt, calculatedValue: UInt, roundingMode: UInt): (Bool, UInt, UInt, Int) = {
    val tempFractionLength = mantissaLength + 4

    val roundedFractionMSBIndex = mantissaLength + 1

    val calculatedValueLength = tempFractionLength + 1

    val originalCalculatedValue = calculatedValue.asTypeOf(UInt(calculatedValueLength.W))

    val paddedCalcValue = Wire(UInt(8.W))

    // dodala sam jednu nulu desno zbog dela koji sledi - brisanja nula sa leve strane, odnosno pomeranja
    // u sustini se dobije kao rezultat sabiranja

    paddedCalcValue := {
      if (format == 0) originalCalculatedValue
      else Cat(originalCalculatedValue, Fill(8 - (calculatedValueLength), 0.U))
    }

    val moveDecPoint4 = !paddedCalcValue(6, 3).orR

    val firstShift = {
      if (moveDecPoint4 == true.B) Cat(paddedCalcValue(2, 0), Fill(4, 0.U))
      else paddedCalcValue(6, 0)
    }

    val moveDecPoint2 = !firstShift(6, 5).orR

    val secondShift = {
      if (moveDecPoint2 == true.B) Cat(firstShift(4, 0), Fill(2, 0.U))
      else firstShift
    }

    val moveDecPoint1 = !secondShift(6)

    // ovime se dobije da fraction sigurno pocinje sa 1
    val shiftedCalcValue = {
      if (moveDecPoint1 == true.B) Cat(secondShift(5, 0), 0.U)
      else secondShift
    }

    val moveDecPoint = 0.U + {
      if (moveDecPoint4 == true.B) 4.U
      else 0.U
    } + {
      if (moveDecPoint2 == true.B) 2.U
      else 0.U
    } + {
      if (moveDecPoint1 == true.B) 1.U
      else 0.U
    }

    val tempExponent = Wire(UInt(exponentLength.W))
    val tempFraction = Wire(UInt(7.W))

    when(exponent.andR && paddedCalcValue(7)) { // exponent overflow
      tempExponent := exponent
      tempFraction := 127.U
    }.elsewhen(!exponent.andR && paddedCalcValue(7)) { // normalizovani broj koji pocinje sa 1 bez pomeranja zapisa
      tempExponent := exponent +& 1.U // ali se uzima u obzir pomeranje decimalnog zareza
      tempFraction := Cat(paddedCalcValue(7, 2), paddedCalcValue(1, 0).orR.asUInt)
    }.elsewhen(exponent > moveDecPoint && shiftedCalcValue(6)) { // normalizovani broj koji je pomeren tako da pocinje sa 1
      tempExponent := exponent - moveDecPoint
      tempFraction := shiftedCalcValue
    }.otherwise {
      tempExponent := 0.U // denormalizovani broj ili 0
      when(exponent > 0.U) {
        tempFraction := paddedCalcValue(6, 0) << (exponent - 1.U) // pomeri se koliko moze
      }.otherwise {
        tempFraction := paddedCalcValue(6, 0) // nema koliko vise da se pomeri
      }
    }
    // morala je da se napravi nova promenljiva zbog faze zaokruzivanja broja
    val tempFraction1 = tempFraction(6, 7 - tempFractionLength)
    val addOne = (!roundingMode(1) && !roundingMode(0) && tempFraction1(2) && (tempFraction1(1) || tempFraction1(0))) || // zaokruzivanje do najblizeg
      (!roundingMode(1) && !roundingMode(0) && tempFraction1(2) && !tempFraction1(1) && !tempFraction1(0) && tempFraction1(3)) || // zaokruzivanje do najblizeg
      (!roundingMode(1) && roundingMode(0) && (tempFraction1(2) || tempFraction1(1) || tempFraction1(0)) && sign.asBool) || // prema -infty
      (roundingMode(1) && !roundingMode(0) && (tempFraction1(2) || tempFraction1(1) || tempFraction1(0)) && !sign.asBool) // prema +infty

    val roundedFraction = Cat(0.U, tempFraction1(tempFractionLength - 1, 3) +& addOne.asUInt)

    val finalFraction = Mux(roundedFraction(roundedFractionMSBIndex) === 1.U, roundedFraction(roundedFractionMSBIndex, 1), roundedFraction(roundedFractionMSBIndex - 1, 0))

    val finalExponent = Wire(UInt((exponentLength + 1).W))

    finalExponent := Mux(roundedFraction(roundedFractionMSBIndex) === 1.U || (roundedFraction(roundedFractionMSBIndex - 1) === 1.U && !tempExponent.orR),
      tempExponent +& 1.U,
      tempExponent
    )
    val overflow = (finalExponent >= (maxExponent.U +& 1.U)) || (tempExponent === maxExponent.U)
    (overflow, finalExponent, finalFraction, roundedFractionMSBIndex)
  }

  def additionFinalResult(overflow: Bool, sign: UInt, finalExponent: UInt, finalFraction: UInt, roundedFractionMSBIndex: Int)(roundingMode: UInt, saturationMode: UInt, isInfty: Bool, is0: Bool, isNaN: Bool) : UInt = {
    val z = Wire(UInt(8.W))
    when(overflow && roundingMode === 0.U && saturationMode === 0.U && !isInfty && !is0 && !isNaN) { // infinity
      z := Cat(sign, Fill(exponentLength, 1.U), Fill(mantissaLength, 0.U))
    }.elsewhen(overflow && roundingMode === 0.U && saturationMode === 1.U && !isInfty && !is0 && !isNaN) { // max value
      z := Cat(sign, Fill(exponentLength - 1, 1.U), 0.U, Fill(mantissaLength, 1.U))
    }.elsewhen(overflow && roundingMode === 1.U && saturationMode === 0.U && sign === 0.U && !isInfty && !is0 && !isNaN) { // -infty
      z := Cat(1.U, Fill(exponentLength, 1.U), Fill(mantissaLength, 0.U))
    }.elsewhen(overflow && roundingMode === 1.U && saturationMode === 1.U && sign === 0.U && !isInfty && !is0 && !isNaN) { // -max value
      z := Cat(1.U, Fill(exponentLength - 1, 1.U), 0.U, Fill(mantissaLength, 1.U))
    }.elsewhen(overflow && roundingMode === 1.U && sign === 1.U && !isInfty && !is0 && !isNaN) { // +max value
      z := Cat(0.U, Fill(exponentLength - 1, 1.U), 0.U, Fill(mantissaLength, 1.U))
    }.elsewhen(overflow && roundingMode === 2.U && saturationMode === 0.U && sign === 0.U && !isInfty && !is0 && !isNaN) { // +infty
      z := Cat(0.U, Fill(exponentLength, 1.U), Fill(mantissaLength, 0.U))
    }.elsewhen(overflow && roundingMode === 2.U && saturationMode === 1.U && sign === 0.U && !isInfty && !is0 && !isNaN) { // +max value
      z := Cat(0.U, Fill(exponentLength - 1, 1.U), 0.U, Fill(mantissaLength, 1.U))
    }.elsewhen(overflow && roundingMode === 2.U && sign === 1.U && !isInfty && !is0 && !isNaN) { // - max value
      z := Cat(1.U, Fill(exponentLength - 1, 1.U), 0.U, Fill(mantissaLength, 1.U))
    }.elsewhen(overflow && roundingMode === 3.U && !isInfty && !is0 && !isNaN) { // max value
      z := Cat(sign, Fill(exponentLength - 1, 1.U), 0.U, Fill(mantissaLength, 1.U))
    }.elsewhen(!overflow && !isInfty && !is0 && !isNaN) { // normalized mantissa
      z := Cat(sign, finalExponent(exponentLength - 1, 0), finalFraction(roundedFractionMSBIndex - 2, 0))
    }.elsewhen(isInfty && !is0 && !isNaN) { // infty
      z := Cat(sign, Fill(exponentLength, 1.U), Fill(mantissaLength, 0.U))
    }.elsewhen(!isInfty && is0 && !isNaN) {
      z := Cat(sign, Fill(exponentLength, 0.U), Fill(mantissaLength, 0.U))
    }.elsewhen(isNaN) {
      z := Cat(0.U, Fill(exponentLength, 1.U), Fill(mantissaLength, 1.U))
    }.otherwise {
      z := 0.U
    }
    z
  }


  def +(other: FloatingPoint)(roundingMode: UInt, saturationMode: UInt, subtract: UInt): UInt = {
    val (sign, greaterOperandFraction, smallerOperandFraction, exponent, subtraction) = this.alignForAddition(other)(subtract)

    val isResultNaN = WireDefault(this.isNaN || other.isNaN || (this.isInfty && other.isInfty && subtraction.asBool))
    val isResultInfty = WireDefault((this.isInfty || other.isInfty) & !isResultNaN)
    val isResult0 = WireDefault(this.isEqualTo(other) && subtraction.asBool && !isResultNaN && !isResultInfty)

    val calculatedValue = Mux(subtraction === 1.U,
      greaterOperandFraction -& smallerOperandFraction,
      greaterOperandFraction +& smallerOperandFraction)

    val (overflow, finalExponent, finalFraction, roundedFractionMSBIndex) = normalize(sign, exponent, calculatedValue, roundingMode)

    additionFinalResult(overflow, sign, finalExponent, finalFraction, roundedFractionMSBIndex)(roundingMode, saturationMode, isResultInfty, isResult0, isResultNaN)
  }

  def /(other: FloatingPoint)(roundingMode: UInt, saturationMode: UInt) = {
    val isResultNan = WireDefault(this.isNaN || other.isNaN || (this.is0 && other.is0) || (this.isInfty && other.isInfty))
    val isResultInfty = WireDefault((other.is0 && !this.is0 && !this.isInfty && !isNaN) || (this.isInfty && !other.isNaN))
    val isResult0 = WireDefault((this.is0 && !other.is0 && !other.isNaN) || (other.isInfty && !this.isInfty && !this.isNaN))
  }
}
