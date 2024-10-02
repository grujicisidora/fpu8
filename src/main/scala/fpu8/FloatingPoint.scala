package fpu8

import chisel3._
import chisel3.util._

import scala.math.pow

class FloatingPoint(e5m2: Boolean) extends Bundle {

  val data = UInt(8.W)

  val exponentLength = {
    if(!e5m2) 4
    else 5
  }

  val mantissaLength = 7 - exponentLength

  val maxExponent = pow(2, exponentLength).toInt - 1

  val maxMantissa = pow(2, mantissaLength).toInt - 1

  def sign: UInt = data(7)

  def exponent: UInt = data(6, 7 - exponentLength)

  def mantissa: UInt = data(mantissaLength - 1, 0)

  private def isAbsValGreater(other: FloatingPoint): Bool = this.data(6, 0) > other.data(6, 0)

  private def isAbsValEqualTo(other: FloatingPoint): Bool = this.data(6, 0) === other.data(6, 0)

  private def isExp0: Bool = exponent === 0.U

  private def isExpMax: Bool = exponent === maxExponent.U

  private def isMantissa0: Bool = mantissa === 0.U

  private def isMantissaMax: Bool = mantissa === maxMantissa.U

  def isNaN: Bool = isExpMax && {
    if (e5m2) !isMantissa0
    else isMantissaMax
  }

  def isInfty: Bool = {
    if (e5m2) isExpMax && isMantissa0
    else false.B
  }

  def is0: Bool = isExp0 && isMantissa0

  private def shiftToMSB1(value: UInt): (UInt, UInt) = {
    val width = value.getWidth
    val leadingZeros = Wire(UInt(width.W))
    val shiftedValue = Wire(UInt(width.W))

    leadingZeros := PriorityEncoder(Reverse(value))

    shiftedValue := value << leadingZeros

    (shiftedValue, leadingZeros)
  }

  private def roundValue(value: UInt, resLength: Int): UInt = {
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

  private def generateROMforDiv: Vec[UInt] = {
    val vecSize = pow(2, mantissaLength).toInt

    val romDiv = VecInit(Seq.fill(vecSize)(0.U((mantissaLength + 1).W)))

    val add = if (e5m2) 1 else 0

    for (i <- 0 until vecSize) {
      romDiv(i) := (14 - i * 2 + add).U
    }
    romDiv
  }

  private def multiply(a: UInt, b: UInt): UInt = {
    val lengthA = a.getWidth
    val lengthB = b.getWidth
    val partialProducts = Seq.tabulate(lengthB)(i => {
      val compare = Mux(b(i), ((1 << lengthA) - 1).U(lengthA.W), 0.U(lengthA.W))
      ((a & compare) << i).asUInt
    })

    def reducePartialProducts(pp: Seq[UInt]): UInt = {
      if (pp.size == 1) {
        pp.head
      } else if (pp.size == 2) {
        pp(0) +& pp(1)
      } else {
        val partialSums = pp.grouped(3).toSeq.map {
          case Seq(a, b, c) => a +& b +& c
          case Seq(a, b) => a +& b
          case Seq(a) => a
        }
        reducePartialProducts(partialSums)
      }
    }
    reducePartialProducts(partialProducts)
  }

  private def getResultNaNFractionValue(other: FloatingPoint): UInt = {
    if (e5m2) Mux(this.mantissa > other.mantissa, this.mantissa(0), other.mantissa(0))
    else 0.U
  }

  private def prepareForAddition(other: FloatingPoint, subtract: UInt): (UInt, UInt, UInt, UInt, UInt) = {
    val compare = this.isAbsValGreater(other)
    val greaterOperand = Mux(compare, this, other)
    val smallerOperand = Mux(compare, other, this)
    val sign = Mux(compare, this.sign, other.sign ^ subtract)
    val subtraction = subtract ^ greaterOperand.sign ^ smallerOperand.sign
    val greaterExp = greaterOperand.exponent
    val greaterOperandFraction = Cat(!greaterOperand.isExp0, greaterOperand.mantissa) // 1.xx(x) ili 0.xx(x)
    val smallerOperandFraction = Cat(!smallerOperand.isExp0, smallerOperand.mantissa) // 1.xx(x) ili 0.xx(x)
    val isOnlySmallerDenormalized = !greaterOperand.isExp0 && smallerOperand.isExp0
    val shift = Mux(
      isOnlySmallerDenormalized,
      greaterOperand.exponent - 1.U,
      greaterOperand.exponent - smallerOperand.exponent
    )
    val shiftedFraction = {
      val maxShift = mantissaLength + 3
      val shifted = Mux(
        shift >= maxShift.U,
        Cat(0.U(maxShift.W), smallerOperandFraction),
        Cat(smallerOperandFraction, 0.U(maxShift.W)) >> shift
      )
      Cat(shifted(maxShift + mantissaLength, mantissaLength + 1), shifted(mantissaLength, 0).orR.asUInt)
    }
    val extendedGreaterOperandFraction = Cat(greaterOperandFraction, 0.U(3.W))
    (sign, extendedGreaterOperandFraction, shiftedFraction, greaterExp, subtraction)
  }

  private def prepareForMultiplication(other: FloatingPoint): (UInt, UInt, UInt, UInt) = {
    val sign = this.sign ^ other.sign
    val exponent = {
      if (!e5m2)
        Mux(this.isExp0 && other.isExp0,
          52.U(6.W), // -12
          Mux(this.isExp0 ^ other.isExp0,
            Cat(0.U(2.W), this.exponent) + Cat(0.U(2.W), other.exponent) - 6.U(6.W),
            Cat(0.U(2.W), this.exponent) + Cat(0.U(2.W), other.exponent) - 7.U(6.W)))
      else
        Mux(this.isExp0 && other.isExp0,
          28.U(7.W), // -28
          Mux(this.isExp0 ^ other.isExp0,
            Cat(0.U(2.W), this.exponent) + Cat(0.U(2.W), other.exponent) - 14.U(7.W),
            Cat(0.U(2.W), this.exponent) + Cat(0.U(2.W), other.exponent) - 15.U(7.W)))
    }
    val firstOperandFraction = Cat(!this.isExp0, this.mantissa)
    val secondOperandFraction = Cat(!other.isExp0, other.mantissa)

    (sign, firstOperandFraction, secondOperandFraction, exponent)
  }

  private def prepareForDivision(other: FloatingPoint): (UInt, UInt, UInt, UInt) = {
    val sign = this.sign ^ other.sign
    val tempDividendFraction = Mux(this.isExp0,
      Cat(this.mantissa, 0.U),
      Cat(1.U, this.mantissa))

    val tempDivisorFraction = Mux(other.isExp0,
      Cat(other.mantissa, 0.U),
      Cat(1.U, other.mantissa))

    val tempExponent = Cat(0.U(2.W), this.exponent) -
      Cat(0.U(2.W), other.exponent) +
      ((1 << (exponentLength - 1)) - 1).U((exponentLength - 1).W)

    val (dividendFraction, dividendShift) = shiftToMSB1(tempDividendFraction)

    val (divisorFraction, divisorShift) = shiftToMSB1(tempDivisorFraction)

    val exponent = tempExponent - dividendShift + divisorShift

    (sign, dividendFraction, divisorFraction, exponent)
  }

  private def newtonDiv(dividendFraction: UInt, divisorFraction: UInt): UInt = {
    val rom = generateROMforDiv

    val initGuess = Cat(1.U(2.W), rom(divisorFraction(mantissaLength - 1, 0)))

    def iteration(xi: UInt, divisorFrac: UInt): UInt = {
      val firstStep = multiply(xi, divisorFrac)

      val roundingLength = if (!e5m2) 6 else 5
      val firstStepLength = firstStep.getWidth

      val firstStepRnd = roundValue(firstStep(firstStepLength - 2, 0), roundingLength)

      // 2 - x_i * divisorFrac
      val secondStep = (~firstStepRnd(roundingLength - 1, 0)).asUInt + 1.U

      // x_i * (2 - x_i * divisorFrac)
      val finalStepLength = 2 * mantissaLength + 6
      val finalStep = Wire(UInt(finalStepLength.W))
      finalStep := multiply(xi, secondStep)

      val res = roundValue(finalStep(finalStepLength - 2, 0), roundingLength)

      res
    }

    val secondGuess = iteration(initGuess, divisorFraction)

    val finalGuess = iteration(secondGuess(mantissaLength + 2, 0), divisorFraction)

    multiply(finalGuess(mantissaLength + 2, 0), dividendFraction) // 0x.xxxxxx(xx)
  }

  private def roundAndNormalize(sign: UInt, tempFraction: UInt, fractionMSB: Int, fractionLSB: Int, tempExponent: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val addOne = ((roundingMode === 0.U) && tempFraction(2) && (tempFraction(1) || tempFraction(0))) ||
      ((roundingMode === 0.U) && tempFraction(2) && !tempFraction(1) && !tempFraction(0) && tempFraction(3)) ||
      ((roundingMode === 1.U) && (tempFraction(2) || tempFraction(1) || tempFraction(0)) && sign.asBool) ||
      ((roundingMode === 2.U) && (tempFraction(2) || tempFraction(1) || tempFraction(0)) && !sign.asBool)

    val roundedFraction = tempFraction(fractionMSB, fractionLSB) +& addOne.asUInt

    val finalFraction = Mux(roundedFraction(mantissaLength + 1) === 1.U,
      roundedFraction(mantissaLength + 1, 1), roundedFraction(mantissaLength, 0))

    val finalExponent = Mux(roundedFraction(mantissaLength + 1) === 1.U || (roundedFraction(mantissaLength) && tempExponent === 0.U),
      tempExponent + 1.U, tempExponent)

    val overflow = {
      if (e5m2) (finalExponent >= (maxExponent.U +& 1.U)) || (tempExponent === maxExponent.U)
      else (finalExponent > maxExponent.U) || ((finalExponent(exponentLength - 1, 0) === maxExponent.U) && (finalFraction === 15.U)) ||
        ((tempExponent >= maxExponent.U) && (tempFraction(fractionMSB, fractionLSB) === 15.U))
    }

    (overflow, finalExponent, finalFraction)
  }

  private def normalizeAfterAddition(sign: UInt, exponent: UInt, calculatedValue: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val tempFractionLength = mantissaLength + 4

    val calculatedValueLength = tempFractionLength + 1

    val originalCalculatedValue = calculatedValue.asTypeOf(UInt(calculatedValueLength.W))

    val paddedCalcValue = Wire(UInt(8.W))

    paddedCalcValue := {
      if (!e5m2) originalCalculatedValue
      else Cat(originalCalculatedValue, 0.U((8 - calculatedValueLength).W))
    }

    val (shiftedCalcValue, shift) = shiftToMSB1(paddedCalcValue(6, 0))

    val tempExponent = Wire(UInt(exponentLength.W))
    val tempFraction = Wire(UInt(7.W))

    when(exponent.andR && paddedCalcValue(7)) {
      tempExponent := exponent
      tempFraction := 127.U
    }.elsewhen(!exponent.andR && paddedCalcValue(7)) {
      tempExponent := exponent + 1.U
      tempFraction := Cat(paddedCalcValue(7, 2), paddedCalcValue(1, 0).orR.asUInt)
    }.elsewhen(exponent > shift && shiftedCalcValue(6)) {
      tempExponent := exponent - shift
      tempFraction := shiftedCalcValue
    }.otherwise {
      tempExponent := 0.U
      when(exponent > 0.U) {
        tempFraction := paddedCalcValue(6, 0) << (exponent - 1.U)
      }.otherwise {
        tempFraction := paddedCalcValue(6, 0)
      }
    }

    roundAndNormalize(sign, tempFraction(6, 7 - tempFractionLength), tempFractionLength - 1, 3, tempExponent, roundingMode)
  }

  private def normalizeAfterMultiplication(sign: UInt, exponent: UInt, calculatedValue: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val calculatedValueLength = calculatedValue.getWidth

    val (shiftedCalcValue, shift) = shiftToMSB1(calculatedValue(calculatedValueLength - 2, 0))

    val exponentShiftRight = 0.U((exponentLength + 2).W) - exponent

    val exponentShiftLeft = exponent - 1.U((exponentLength + 2).W)

    val tempExponent = Wire(UInt(5.W))

    val tempFraction = Wire(UInt((calculatedValueLength - 1).W))

    when(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) >= maxExponent.U)
      && calculatedValue(calculatedValueLength - 1)) {
      tempExponent := maxExponent.U
      tempFraction := { if (!e5m2) 127.U else 31.U }
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) < maxExponent.U)
      && calculatedValue(calculatedValueLength - 1)) {
      tempExponent := exponent(4, 0) + 1.U
      tempFraction := Cat(calculatedValue(calculatedValueLength - 1, 2), calculatedValue(1, 0).orR.asUInt)
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) > shift)
      && shiftedCalcValue(calculatedValueLength - 2)) {
      tempExponent := exponent(exponentLength, 0) - shift
      tempFraction := shiftedCalcValue
    }.otherwise {
      tempExponent := 0.U
      when(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) > 0.U)) {
        tempFraction := calculatedValue(calculatedValueLength - 2, 0) << exponentShiftLeft
      }.otherwise {
        tempFraction := Cat(calculatedValue(calculatedValueLength - 1, 2), calculatedValue(1, 0).orR.asUInt) >> exponentShiftRight
      }
    }

    roundAndNormalize(sign, tempFraction, calculatedValueLength - 2, mantissaLength, tempExponent, roundingMode)
  }

  private def normalizeAfterDivision(sign: UInt, exponent: UInt, calculatedValue: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val calculatedValueLength = calculatedValue.getWidth

    val (shiftedCalcValue, shift) = shiftToMSB1(calculatedValue(calculatedValueLength - 2, 0))

    val exponentShiftRight = 0.U((exponentLength + 2).W) - exponent

    val exponentShiftLeft = exponent - 1.U((exponentLength + 2).W)

    val tempExponent = Wire(UInt((exponentLength + 1).W))

    val tempFraction = Wire(UInt((mantissaLength + 5).W))

    val makeFractionShorter = if(e5m2) 1 else 0

    when(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) >= maxExponent.U)
      && calculatedValue(calculatedValueLength - 1)) {
      tempExponent := maxExponent.U
      tempFraction := ((1 << (8 - makeFractionShorter)) - 1).U((8 - makeFractionShorter).W)
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) < maxExponent.U)
      && calculatedValue(calculatedValueLength - 1)) {
      tempExponent := exponent(exponentLength, 0) + 1.U
      tempFraction := {
        if (!e5m2)
          calculatedValue(calculatedValueLength - 2, calculatedValueLength - 8) +
            calculatedValue(calculatedValueLength - 9, 0).andR.asUInt
        else
          calculatedValue(calculatedValueLength - 2, calculatedValueLength - 7)
      }
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) > shift)
      && shiftedCalcValue(calculatedValueLength - 2)) {
      tempExponent := exponent(exponentLength, 0) - shift
      tempFraction := {
        if (!e5m2)
          shiftedCalcValue(calculatedValueLength - 2, calculatedValueLength - 8) +
            shiftedCalcValue(calculatedValueLength - 9, 0).andR.asUInt
        else
          shiftedCalcValue(calculatedValueLength - 2, calculatedValueLength - 7)
      }
    }.otherwise {
      tempExponent := 0.U
      when(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) > 0.U)) {
        tempFraction := {
          if (!e5m2)
            (calculatedValue(calculatedValueLength - 2, calculatedValueLength - 8) +
              calculatedValue(calculatedValueLength - 9, 0).andR.asUInt) << exponentShiftLeft
          else
            calculatedValue(calculatedValueLength - 2, calculatedValueLength - 7) << exponentShiftLeft
        }
      }.otherwise {
        tempFraction := (calculatedValue(calculatedValueLength - 1, calculatedValueLength - 7 + makeFractionShorter) +
          calculatedValue(calculatedValueLength - 8 + makeFractionShorter, calculatedValueLength - 9 + makeFractionShorter).andR.asUInt) >> exponentShiftRight
      }
    }

    roundAndNormalize(sign, tempFraction, 6 - makeFractionShorter, 3, tempExponent, roundingMode)
  }

  def +(other: FloatingPoint): (UInt, UInt) => (UInt, UInt, UInt, Bool, Bool, Bool, Bool, UInt) = {
    (roundingMode: UInt, subtract: UInt) => {
      require(this.exponentLength == other.exponentLength, "Required same FP8 encoding.")
      val (sign, greaterOperandFraction, smallerOperandFraction, exponent, subtraction) = prepareForAddition(other, subtract)

      val isResultNaN = {
        if (e5m2) WireDefault(this.isNaN || other.isNaN || (this.isInfty && other.isInfty && subtraction.asBool))
        else WireDefault(this.isNaN || other.isNaN)
      }

      val isResultInfty = {
        if (e5m2) WireDefault((this.isInfty || other.isInfty) & !isResultNaN)
        else false.B
      }

      val isResult0 = WireDefault(this.isAbsValEqualTo(other) && subtraction.asBool && !isResultNaN && !isResultInfty)

      val resultNaNFractionValue = getResultNaNFractionValue(other)

      val calculatedValue = Mux(subtraction === 1.U,
        greaterOperandFraction -& smallerOperandFraction, // xx.xxx...
        greaterOperandFraction +& smallerOperandFraction) // xx.xxx...

      val (overflow, finalExponent, finalFraction) = normalizeAfterAddition(sign, exponent, calculatedValue, roundingMode)

      (sign, finalExponent, finalFraction, overflow, isResultInfty, isResult0, isResultNaN, resultNaNFractionValue)
    }
  }

  def *(other: FloatingPoint): UInt => (UInt, UInt, UInt, Bool, Bool, Bool, Bool, UInt) = {
    (roundingMode: UInt) => {
      require(this.exponentLength == other.exponentLength, "Required same FP8 encoding.")

      val isResultNaN = {
        if (e5m2) WireDefault((this.isInfty && other.is0) || this.isNaN || (other.isInfty && this.is0) || other.isNaN)
        else WireDefault(this.isNaN || other.isNaN)
      }

      val isResultInfty = {
        if (e5m2) WireDefault((this.isInfty && !other.is0 && !other.isNaN) || (other.isInfty && !this.is0 && !this.isNaN))
        else false.B
      }

      val isResult0 = WireDefault((this.is0 && !other.isNaN) || (other.is0 && !this.isNaN))

      val resultNaNFractionValue = getResultNaNFractionValue(other)

      val (sign, firstOperandFraction, secondOperandFraction, exponent) = prepareForMultiplication(other)

      val product = multiply(firstOperandFraction, secondOperandFraction)

      val (overflow, finalExponent, finalFraction) = normalizeAfterMultiplication(sign, exponent, product, roundingMode)

      (sign, finalExponent, finalFraction, overflow, isResultInfty, isResult0, isResultNaN, resultNaNFractionValue)
    }
  }

  def /(other: FloatingPoint): UInt => (UInt, UInt, UInt, Bool, Bool, Bool, Bool, UInt) = {
    (roundingMode: UInt) => {
      require(this.exponentLength == other.exponentLength, "Required same FP8 encoding.")

      val isResultNaN = {
        if (e5m2) WireDefault(this.isNaN || other.isNaN || (this.is0 && other.is0) || (this.isInfty && other.isInfty))
        else this.isNaN || other.isNaN || other.is0
      }

      val isResultInfty = {
        if (e5m2) WireDefault((other.is0 && !this.is0 && !this.isInfty && !this.isNaN) || (this.isInfty && !other.isNaN))
        else false.B
      }

      val isResult0 = {
        if (e5m2) WireDefault((this.is0 && !other.is0 && !other.isNaN) || (other.isInfty && !this.isInfty && !this.isNaN))
        else is0 && !other.is0 && !other.isNaN
      }

      val resultNaNFractionValue = getResultNaNFractionValue(other)

      val (sign, dividendFraction, divisorFraction, exponent) = prepareForDivision(other)

      val quotient = newtonDiv(dividendFraction, divisorFraction)

      val (overflow, finalExponent, finalFraction) = normalizeAfterDivision(sign, exponent, quotient, roundingMode)

      (sign, finalExponent, finalFraction, overflow, isResultInfty, isResult0, isResultNaN, resultNaNFractionValue)
    }
  }

  def <(other: FloatingPoint): UInt = {
    require(this.exponentLength == other.exponentLength, "Required same FP8 encoding.")
    val result = Wire(UInt(8.W))

    val isResultNaN = this.isNaN || other.isNaN
    val resultNaNFractionValue = getResultNaNFractionValue(other)

    when(isResultNaN){
      result := {
        if (e5m2) Cat(0.U, maxExponent.U, 1.U, resultNaNFractionValue)
        else Cat(0.U, maxExponent.U, maxMantissa.U)
      }
    }.elsewhen(this.sign > other.sign || (this.sign === 1.U && isAbsValGreater(other)) || (this.sign === 0.U && !isAbsValGreater(other))){
      // true -> 1
      result := Cat(0.U, ((maxExponent - 1)/2).U, 0.U(mantissaLength.W))
    }.otherwise{
      result := 0.U(8.W)
    }
    result
  }

  def >(other: FloatingPoint): UInt = {
    require(this.exponentLength == other.exponentLength, "Required same FP8 encoding.")
    val result = Wire(UInt(8.W))

    val isResultNaN = this.isNaN || other.isNaN
    val resultNaNFractionValue = getResultNaNFractionValue(other)

    when(isResultNaN){
      result := {
        if (e5m2) Cat(0.U, maxExponent.U, 1.U, resultNaNFractionValue)
        else Cat(0.U, maxExponent.U, maxMantissa.U)
      }
    }.elsewhen(this.sign < other.sign || (this.sign === 1.U && !isAbsValGreater(other)) || (this.sign === 0.U && isAbsValGreater(other))) {
      // true -> 1
      result := Cat(0.U, ((maxExponent - 1) / 2).U, 0.U(mantissaLength.W))
    }.otherwise {
      result := 0.U(8.W)
    }
    result
  }

  def ==(other: FloatingPoint): UInt = {
    require(this.exponentLength == other.exponentLength, "Required same FP8 encoding.")
    val result = Wire(UInt(8.W))

    val isResultNaN = this.isNaN || other.isNaN
    val resultNaNFractionValue = getResultNaNFractionValue(other)

    when(isResultNaN) {
      result := {
        if (e5m2) Cat(0.U, maxExponent.U, 1.U, resultNaNFractionValue)
        else Cat(0.U, maxExponent.U, maxMantissa.U)
      }
    }.elsewhen(this.data === other.data) {
      // true -> 1
      result := Cat(0.U, ((maxExponent - 1) / 2).U, 0.U(mantissaLength.W))
    }.otherwise {
      result := 0.U(8.W)
    }
    result
  }

  def <=(other: FloatingPoint): UInt = {
    require(this.exponentLength == other.exponentLength, "Required same FP8 encoding.")
    val result = Wire(UInt(8.W))

    val isResultNaN = this.isNaN || other.isNaN
    val resultNaNFractionValue = getResultNaNFractionValue(other)

    when(isResultNaN) {
      result := {
        if (e5m2) Cat(0.U, maxExponent.U, 1.U, resultNaNFractionValue)
        else Cat(0.U, maxExponent.U, maxMantissa.U)
      }
    }.otherwise{
      result := (this < other) ^ (this == other)
    }
    result
  }

  def >=(other: FloatingPoint): UInt = {
    require(this.exponentLength == other.exponentLength, "Required same FP8 encoding.")
    val result = Wire(UInt(8.W))

    val isResultNaN = this.isNaN || other.isNaN
    val resultNaNFractionValue = getResultNaNFractionValue(other)

    when(isResultNaN) {
      result := {
        if (e5m2) Cat(0.U, maxExponent.U, 1.U, resultNaNFractionValue)
        else Cat(0.U, maxExponent.U, maxMantissa.U)
      }
    }.otherwise {
      result := (this > other) ^ (this == other)
    }
    result
  }

  def !=(other: FloatingPoint): UInt = {
    require(this.exponentLength == other.exponentLength, "Required same FP8 encoding.")
    val result = Wire(UInt(8.W))

    val isResultNaN = this.isNaN || other.isNaN
    val resultNaNFractionValue = getResultNaNFractionValue(other)

    when(isResultNaN) {
      result := {
        if (e5m2) Cat(0.U, maxExponent.U, 1.U, resultNaNFractionValue)
        else Cat(0.U, maxExponent.U, maxMantissa.U)
      }
    }.elsewhen(this.data =/= other.data) {
      // true -> 1
      result := Cat(0.U, ((maxExponent - 1) / 2).U, 0.U(mantissaLength.W))
    }.otherwise {
      result := 0.U(8.W)
    }
    result
  }

  def convert(): UInt = {
    require(!this.e5m2, "Wrong FP8 encoding.")
    val fraction = Cat(!isExp0.asUInt, mantissa)
    val (shiftedFraction, shift) = shiftToMSB1(fraction)

    val tempExponent = Mux(isExp0, 9.U -& shift, 8.U +& exponent)
    val tempFraction = shiftedFraction(mantissaLength, 1)

    val addOne = exponent.orR.asUInt & shiftedFraction(mantissaLength - 2, 0).andR.asUInt

    val roundedFraction = tempFraction +& addOne

    val finalExponent = Mux(roundedFraction(mantissaLength) === 1.U, tempExponent + 1.U, tempExponent)
    val finalFraction = Mux(roundedFraction(mantissaLength) === 1.U,
      roundedFraction(mantissaLength, mantissaLength - 2),
      roundedFraction(mantissaLength - 1, mantissaLength - 3))

    val result = Wire(UInt(8.W))

    when(!is0 && !isNaN) {
      result := Cat(sign, finalExponent, finalFraction(mantissaLength - 2, mantissaLength - 3))
    }.elsewhen(is0 && !isNaN) {
      result := Cat(sign, 0.U((exponentLength + 1).W), 0.U((mantissaLength - 1).W))
    }.elsewhen(isNaN) {
      result := Cat(0.U, (maxExponent * 2 + 1).U, ((maxMantissa - 1) / 2).U)
    }.otherwise {
      result := 0.U
    }
    result
  }

  def convert(roundingMode: UInt, saturationMode: UInt): UInt = {
    require(this.e5m2, "Wrong FP8 encoding.")

    val isResultNaN = isNaN || isInfty

    val fraction = Cat(!isExp0.asUInt, mantissa)

    val tempExponent = exponent -& 8.U

    val isDenormalized = tempExponent(exponentLength) || (tempExponent(exponentLength - 1, 0) === 0.U)
    val overflow = !tempExponent(exponentLength) && tempExponent(exponentLength - 1)
    val shift = Mux(isDenormalized, 1.U -& tempExponent, 0.U)

    val tempFraction = Cat(fraction, 0.U((exponentLength - 1).W)) >> shift

    val addOne = ((roundingMode === 0.U) && tempFraction(2) && (tempFraction(1) || tempFraction(0))) ||
      ((roundingMode === 0.U) && tempFraction(2) && !tempFraction(1) && !tempFraction(0) && tempFraction(3)) ||
      ((roundingMode === 1.U) && (tempFraction(2) || tempFraction(1) || tempFraction(0)) && sign.asBool) ||
      ((roundingMode === 2.U) && (tempFraction(2) || tempFraction(1) || tempFraction(0)) && !sign.asBool)

    val roundedFraction = tempFraction(mantissaLength + 4, mantissaLength + 1) +& addOne.asUInt

    val finalExponent = Mux(overflow, ((maxExponent - 1) / 2).U,
      Mux(isDenormalized, 0.U, tempExponent(exponentLength - 2, 0)))

    val finalFraction = roundedFraction(mantissaLength + 1, 0)

    val result = Wire(UInt(8.W))

    when(!isResultNaN) {
      when(overflow) {
        when((roundingMode === 0.U && saturationMode === 0.U) ||
          (roundingMode === 1.U && saturationMode === 0.U && sign === 0.U) ||
          (roundingMode === 2.U && saturationMode === 0.U && sign === 0.U)) {
          result := Cat(0.U, ((maxExponent - 1) / 2).U, (maxMantissa * 2 + 1).U)
        }.elsewhen((roundingMode === 0.U && saturationMode === 1.U) ||
          (roundingMode === 3.U && saturationMode === 0.U)) {
          result := Cat(sign, ((maxExponent - 1) / 2).U, (maxMantissa * 2).U)
        }.elsewhen((roundingMode === 1.U && saturationMode === 1.U && sign === 0.U) ||
          (roundingMode === 2.U && sign === 1.U)) {
          result := Cat(sign, ((maxExponent - 1) / 2).U, (maxMantissa * 2).U)
        }.elsewhen((roundingMode === 1.U && sign === 1.U) ||
          (roundingMode === 2.U && saturationMode === 1.U && sign === 0.U)) {
          result := Cat(sign, ((maxExponent - 1) / 2).U, (maxMantissa * 2).U)
        }.otherwise {
          result := 0.U
        }
      }.otherwise {
        result := Cat(sign, finalExponent, finalFraction(mantissaLength, 0))
      }
    }.elsewhen(isResultNaN) {
      result := Cat(0.U, ((maxExponent - 1) / 2).U, (maxMantissa * 2 + 1).U)
    }.otherwise {
      result := 0.U
    }
    result
  }
}