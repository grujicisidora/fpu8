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

  def isAbsValGreater(other: FloatingPoint): Bool = this.data(6, 0) > other.data(6, 0)

  def isAbsValEqualTo(other: FloatingPoint): Bool = this.data(6, 0) === other.data(6, 0)

  def isExp0: Bool = exponent === 0.U

  def isExpMax: Bool = exponent === maxExponent.U

  def isMantissa0: Bool = mantissa === 0.U

  def isMantissaMax: Bool = mantissa === maxMantissa.U

  //def isNaN: Bool = isExpMax && !isMantissa0
  def isNaN: Bool = isExpMax && {
    if (e5m2) !isMantissa0
    else isMantissaMax
  }

  //def isInfty: Bool = isExpMax && isMantissa0
  def isInfty: Bool = {
    if (e5m2) isExpMax && isMantissa0
    else false.B
  }

  def is0: Bool = isExp0 && isMantissa0

  def shiftToMSB1(value: UInt): (UInt, UInt) = {
    val width = value.getWidth
    val leadingZeros = Wire(UInt(width.W))
    val shiftedValue = Wire(UInt(width.W))

    leadingZeros := PriorityEncoder(Reverse(value))

    shiftedValue := value << leadingZeros

    (shiftedValue, leadingZeros)
  }

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

  def generateROMforDiv: Vec[UInt] = {
    val vecSize = pow(2, mantissaLength).toInt

    val romDiv = VecInit(Seq.fill(vecSize)(0.U((mantissaLength + 1).W)))

    val add = if (e5m2) 1 else 0

    for (i <- 0 until vecSize) {
      romDiv(i) := (14 - i * 2 + add).U
    }
    romDiv
  }

  def multiply(a: UInt, b: UInt): UInt = {
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

  def getResultNaNFractionValue(other: FloatingPoint): UInt = {
    if (e5m2) Mux(this.mantissa > other.mantissa, this.mantissa(0), other.mantissa(0))
    else 0.U
  }

  def prepareForAddition(other: FloatingPoint, subtract: UInt): (UInt, UInt, UInt, UInt, UInt) = {
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

  def prepareForMultiplication(other: FloatingPoint): (UInt, UInt, UInt, UInt) = {
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

  def prepareForDivision(other: FloatingPoint): (UInt, UInt, UInt, UInt) = {
    val sign = this.sign ^ other.sign
    val tempDividendFraction = Mux(this.isExp0,
      Cat(this.mantissa, 0.U),
      Cat(1.U, this.mantissa))

    //printf(cf"PRINTF: tempDividendFraction $tempDividendFraction\n")

    val tempDivisorFraction = Mux(other.isExp0,
      Cat(other.mantissa, 0.U),
      Cat(1.U, other.mantissa))

    //printf(cf"PRINTF: tempDivisorFraction $tempDivisorFraction\n")

    val tempExponent = Cat(0.U(2.W), this.exponent) -
      Cat(0.U(2.W), other.exponent) +
      ((1 << (exponentLength - 1)) - 1).U((exponentLength - 1).W) // dodaje se bias na stvarni eksponent

    //printf(cf"PRINTF: tempExponent $tempExponent\n")

    val (dividendFraction, dividendShift) = shiftToMSB1(tempDividendFraction)
    //printf(cf"PRINTF: dividendFraction $dividendFraction\n")

    val (divisorFraction, divisorShift) = shiftToMSB1(tempDivisorFraction)
    //printf(cf"PRINTF: divisorFraction $divisorFraction\n")

    val exponent = tempExponent - dividendShift + divisorShift
    //printf(cf"PRINTF: exponent $exponent\n")

    (sign, dividendFraction, divisorFraction, exponent)
  }

  def newtonDiv(dividendFraction: UInt, divisorFraction: UInt): UInt = {
    val rom = generateROMforDiv

    val initGuess = Cat(1.U(2.W), rom(divisorFraction(mantissaLength - 1, 0)))
    //printf(cf"PRINTF: initGuess $initGuess\n")

    def iteration(xi: UInt, divisorFrac: UInt): UInt = {
      // x_i * divisorFrac
      val firstStep = multiply(xi, divisorFrac)
      //printf(cf"PRINTF: firstStep $firstStep\n")

      val roundingLength = if (!e5m2) 6 else 5
      val firstStepLength = firstStep.getWidth
      //printf(cf"PRINTF: firstStepLength $firstStepLength\n")

      val firstStepRnd = roundValue(firstStep(firstStepLength - 2, 0), roundingLength)
      //printf(cf"PRINTF: firstStepRnd $firstStepRnd\n")

      // 2 - x_i * divisorFrac
      val secondStep = (~firstStepRnd(roundingLength - 1, 0)).asUInt + 1.U
      //printf(cf"PRINTF: secondStep $secondStep\n")

      // x_i * (2 - x_i * divisorFrac)
      val finalStepLength = 2 * mantissaLength + 6
      val finalStep = Wire(UInt(finalStepLength.W)) // mozda je i suvisno eksplicitno navodjenje duzine, ali neka ostane
      finalStep := multiply(xi, secondStep)
      //printf(cf"PRINTF: finalStep $finalStep\n")

      val res = roundValue(finalStep(finalStepLength - 2, 0), roundingLength)
      //printf(cf"PRINTF: res $res\n")

      res
    }

    val secondGuess = iteration(initGuess, divisorFraction)
    //printf(cf"PRINTF: secondGuess $secondGuess\n")

    val finalGuess = iteration(secondGuess(mantissaLength + 2, 0), divisorFraction)
    //printf(cf"PRINTF: finalGuess $finalGuess\n")

    multiply(finalGuess(mantissaLength + 2, 0), dividendFraction) // 0x.xxxxxx(xx)
  }

  def roundAndNormalize(sign: UInt, tempFraction: UInt, fractionMSB: Int, fractionLSB: Int, tempExponent: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val addOne = ((roundingMode === 0.U) && tempFraction(2) && (tempFraction(1) || tempFraction(0))) || // zaokruzivanje do najblizeg
      ((roundingMode === 0.U) && tempFraction(2) && !tempFraction(1) && !tempFraction(0) && tempFraction(3)) || // zaokruzivanje do najblizeg
      ((roundingMode === 1.U) && (tempFraction(2) || tempFraction(1) || tempFraction(0)) && sign.asBool) || // prema -infty tj. prema nizem
      ((roundingMode === 2.U) && (tempFraction(2) || tempFraction(1) || tempFraction(0)) && !sign.asBool) // prema +infty tj. prema visem

    val roundedFraction = tempFraction(fractionMSB, fractionLSB) +& addOne.asUInt

    val finalFraction = Mux(roundedFraction(mantissaLength + 1) === 1.U,
      roundedFraction(mantissaLength + 1, 1), roundedFraction(mantissaLength, 0))

    val finalExponent = Mux(roundedFraction(mantissaLength + 1) === 1.U || (roundedFraction(mantissaLength) && tempExponent === 0.U),
      tempExponent + 1.U, tempExponent)

    //val overflow = (finalExponent >= (maxExponent.U +& 1.U)) || (tempExponent === maxExponent.U)
    val overflow = {
      if (e5m2) (finalExponent >= (maxExponent.U +& 1.U)) || (tempExponent === maxExponent.U)
      else (finalExponent > maxExponent.U) || ((finalExponent(exponentLength - 1, 0) === maxExponent.U) && (finalFraction === 15.U)) ||
        ((tempExponent >= maxExponent.U) && (tempFraction(fractionMSB, fractionLSB) === 15.U))
    }

    (overflow, finalExponent, finalFraction)
  }

  def normalizeAfterAddition(sign: UInt, exponent: UInt, calculatedValue: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val tempFractionLength = mantissaLength + 4

    val calculatedValueLength = tempFractionLength + 1

    val originalCalculatedValue = calculatedValue.asTypeOf(UInt(calculatedValueLength.W)) // zadrzavam duzinu, mada mozda moze i bez ovoga

    val paddedCalcValue = Wire(UInt(8.W))

    // dodala sam jednu nulu desno zbog dela koji sledi - brisanja nula sa leve strane, odnosno pomeranja
    // u sustini se dobije kao rezultat sabiranja

    paddedCalcValue := {
      if (!e5m2) originalCalculatedValue
      else Cat(originalCalculatedValue, 0.U((8 - calculatedValueLength).W))
    }

    // ovaj deo se radi samo zbog slucaja kada se ne pomera eksponent, odnosno kada je rezultat 0x.xxx...
    val (shiftedCalcValue, shift) = shiftToMSB1(paddedCalcValue(6, 0))

    val tempExponent = Wire(UInt(exponentLength.W))
    val tempFraction = Wire(UInt(7.W))

    when(exponent.andR && paddedCalcValue(7)) { // exponent overflow
      tempExponent := exponent
      tempFraction := 127.U
    }.elsewhen(!exponent.andR && paddedCalcValue(7)) { // normalizovani broj koji pocinje sa 1 bez pomeranja zapisa
      tempExponent := exponent + 1.U // ali se uzima u obzir pomeranje decimalnog zareza
      tempFraction := Cat(paddedCalcValue(7, 2), paddedCalcValue(1, 0).orR.asUInt)
    }.elsewhen(exponent > shift && shiftedCalcValue(6)) { // normalizovani broj koji je pomeren tako da pocinje sa 1
      tempExponent := exponent - shift
      tempFraction := shiftedCalcValue
    }.otherwise {
      tempExponent := 0.U // denormalizovani broj ili 0
      when(exponent > 0.U) {
        tempFraction := paddedCalcValue(6, 0) << (exponent - 1.U) // pomeri se koliko moze
      }.otherwise {
        tempFraction := paddedCalcValue(6, 0) // nema koliko vise da se pomeri
      }
    }

    roundAndNormalize(sign, tempFraction(6, 7 - tempFractionLength), tempFractionLength - 1, 3, tempExponent, roundingMode)
  }

  def normalizeAfterMultiplication(sign: UInt, exponent: UInt, calculatedValue: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val calculatedValueLength = calculatedValue.getWidth
    //printf(cf"PRINTF: calculatedValueLength $calculatedValueLength\n")

    val (shiftedCalcValue, shift) = shiftToMSB1(calculatedValue(calculatedValueLength - 2, 0))

    val exponentShiftRight = 0.U((exponentLength + 2).W) - exponent

    val exponentShiftLeft = exponent - 1.U((exponentLength + 2).W)

    val tempExponent = Wire(UInt(5.W))

    val tempFraction = Wire(UInt((calculatedValueLength - 1).W))

    when(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) >= maxExponent.U)
      && calculatedValue(calculatedValueLength - 1)) { // exponent overflow
      tempExponent := maxExponent.U
      tempFraction := { if (!e5m2) 127.U else 31.U }
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) < maxExponent.U)
      && calculatedValue(calculatedValueLength - 1)) { // fraction overflow
      tempExponent := exponent(4, 0) + 1.U
      tempFraction := Cat(calculatedValue(calculatedValueLength - 1, 2), calculatedValue(1, 0).orR.asUInt)
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) > shift)
      && shiftedCalcValue(calculatedValueLength - 2)) { // normalizovani broj koji je pomeren tako da pocinje sa 1
      tempExponent := exponent(exponentLength, 0) - shift
      tempFraction := shiftedCalcValue
    }.otherwise {
      tempExponent := 0.U // denormalizovani broj ili 0
      when(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) > 0.U)) {
        tempFraction := calculatedValue(calculatedValueLength - 2, 0) << exponentShiftLeft
      }.otherwise {
        tempFraction := Cat(calculatedValue(calculatedValueLength - 1, 2), calculatedValue(1, 0).orR.asUInt) >> exponentShiftRight
      }
    }

    roundAndNormalize(sign, tempFraction, calculatedValueLength - 2, mantissaLength, tempExponent, roundingMode)
  }

  def normalizeAfterDivision(sign: UInt, exponent: UInt, calculatedValue: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val calculatedValueLength = calculatedValue.getWidth
    //printf(cf"PRINTF: calculatedValueLength $calculatedValueLength\n")

    val (shiftedCalcValue, shift) = shiftToMSB1(calculatedValue(calculatedValueLength - 2, 0))

    val exponentShiftRight = 0.U((exponentLength + 2).W) - exponent

    val exponentShiftLeft = exponent - 1.U((exponentLength + 2).W)

    val tempExponent = Wire(UInt((exponentLength + 1).W))

    val tempFraction = Wire(UInt((mantissaLength + 5).W))

    val makeFractionShorter = if(e5m2) 1 else 0

    when(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) >= maxExponent.U)
      && calculatedValue(calculatedValueLength - 1)) { // exponent overflow
      tempExponent := maxExponent.U
      tempFraction := ((1 << (8 - makeFractionShorter)) - 1).U((8 - makeFractionShorter).W)
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) < maxExponent.U)
      && calculatedValue(calculatedValueLength - 1)) { // fraction overflow
      tempExponent := exponent(exponentLength, 0) + 1.U
      tempFraction := {
        if (!e5m2)
          calculatedValue(calculatedValueLength - 2, calculatedValueLength - 8) +
            calculatedValue(calculatedValueLength - 9, 0).andR.asUInt
        else
          calculatedValue(calculatedValueLength - 2, calculatedValueLength - 7)
      }
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) > shift)
      && shiftedCalcValue(calculatedValueLength - 2)) { // normalizovani broj koji je pomeren tako da pocinje sa 1
      tempExponent := exponent(exponentLength, 0) - shift
      tempFraction := {
        if (!e5m2)
          shiftedCalcValue(calculatedValueLength - 2, calculatedValueLength - 8) +
            shiftedCalcValue(calculatedValueLength - 9, 0).andR.asUInt
        else
          shiftedCalcValue(calculatedValueLength - 2, calculatedValueLength - 7)
      }
    }.otherwise {
      tempExponent := 0.U // denormalizovani broj ili 0
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

    //printf(cf"PRINTF: tempExponent $tempExponent\n")
    //printf(cf"PRINTF: tempFraction $tempFraction\n")

    roundAndNormalize(sign, tempFraction, 6 - makeFractionShorter, 3, tempExponent, roundingMode)
  }

  // ovu funkciju je potrebno srediti, potencijalno smestiti logiku u poseban modul zbog razlike kod formata
  // implementirati razliku kod formata
  /*def finalResult(overflow: Bool, sign: UInt, finalExponent: UInt, finalFraction: UInt)(roundingMode: UInt, saturationMode: UInt, isInfty: Bool, is0: Bool, isNaN: Bool) : UInt = {
    val z = Wire(UInt(8.W))
    when(!isInfty && !is0 && !isNaN) {
      when(overflow) {
        when(roundingMode === 0.U && saturationMode === 0.U){
          z := Cat(sign, ((1 << exponentLength) - 1).U(exponentLength.W), 0.U(mantissaLength.W))
        }.elsewhen((roundingMode === 0.U && saturationMode === 1.U) || roundingMode === 3.U){
          z := Cat(sign, ((1 << (exponentLength - 1)) - 1).U((exponentLength - 1).W), 0.U, ((1 << mantissaLength) - 1).U(mantissaLength.W))
        }.elsewhen(roundingMode === 1.U && saturationMode === 0.U && sign === 0.U){
          z := Cat(1.U, ((1 << exponentLength) - 1).U(exponentLength.W))
        }.elsewhen((roundingMode === 1.U && saturationMode === 1.U && sign === 0.U) || (roundingMode === 2.U && sign === 1.U)){
          z := Cat(1.U, ((1 << (exponentLength - 1)) - 1).U((exponentLength - 1).W), 0.U, ((1 << mantissaLength) - 1).U(mantissaLength.W))
        }.elsewhen((roundingMode === 1.U && sign === 1.U) || (roundingMode === 2.U && saturationMode === 1.U && sign === 0.U)){
          z := Cat(0.U, ((1 << (exponentLength - 1)) - 1).U((exponentLength - 1).W), 0.U, ((1 << mantissaLength) - 1).U(mantissaLength.W))
        }.elsewhen(roundingMode === 2.U && saturationMode === 0.U && sign === 0.U){
          z := Cat(0.U, ((1 << exponentLength) - 1).U(exponentLength.W), 0.U(mantissaLength.W))
        }.otherwise{
          z := 0.U
        }
      }.otherwise{
        z := Cat(sign, finalExponent(exponentLength - 1, 0), finalFraction(mantissaLength - 1, 0))
      }
    }.elsewhen(isInfty && !is0 && !isNaN){
      z := Cat(sign, ((1 << exponentLength) - 1).U(exponentLength.W), 0.U(mantissaLength.W))
    }.elsewhen(!isInfty && is0 && !isNaN){
      z := Cat(sign, 0.U(exponentLength.W), 0.U(mantissaLength.W))
    }.elsewhen(isNaN){
      z := Cat(0.U, ((1 << exponentLength) - 1).U(exponentLength.W), ((1 << mantissaLength) - 1).U(mantissaLength.W))
    }.otherwise{
      z := 0.U
    }
    z
  }*/


  def +(other: FloatingPoint): (UInt, UInt) => (UInt, UInt, UInt, Bool, Bool, Bool, Bool) = {
    (roundingMode: UInt, subtract: UInt) => {
      val (sign, greaterOperandFraction, smallerOperandFraction, exponent, subtraction) = prepareForAddition(other, subtract)

      //val isResultNaN = WireDefault(this.isNaN || other.isNaN || (this.isInfty && other.isInfty && subtraction.asBool))
      val isResultNaN = {
        if (e5m2) WireDefault(this.isNaN || other.isNaN || (this.isInfty && other.isInfty && subtraction.asBool))
        else WireDefault(this.isNaN || other.isNaN)
      }
      //val isResultInfty = WireDefault((this.isInfty || other.isInfty) & !isResultNaN)
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

      //finalResult(overflow, sign, finalExponent, finalFraction)(roundingMode, saturationMode, isResultInfty, isResult0, isResultNaN)

      (sign, finalExponent, finalFraction, overflow, isResultInfty, isResult0, isResultNaN)
    }
  }

  def *(other: FloatingPoint): UInt => (UInt, UInt, UInt, Bool, Bool, Bool, Bool) = {
    (roundingMode: UInt) => {
      //val isResultNaN = WireDefault((this.isInfty && other.is0) || this.isNaN || (other.isInfty && this.is0) || other.isNaN)
      val isResultNaN = {
        if (e5m2) WireDefault((this.isInfty && other.is0) || this.isNaN || (other.isInfty && this.is0) || other.isNaN)
        else WireDefault(this.isNaN || other.isNaN)
      }

      //val isResultInfty = WireDefault((this.isInfty && !other.is0 && !other.isNaN) || (other.isInfty && !this.is0 && !this.isNaN))
      val isResultInfty = {
        if (e5m2) WireDefault((this.isInfty && !other.is0 && !other.isNaN) || (other.isInfty && !this.is0 && !this.isNaN))
        else false.B
      }

      val isResult0 = WireDefault((this.is0 && !other.isNaN) || (other.is0 && !this.isNaN))

      val resultNaNFractionValue = getResultNaNFractionValue(other)

      //val (sign, firstOperandFraction, secondOperandFraction, exponent) = this.prepareForMultiplication(other)
      val (sign, firstOperandFraction, secondOperandFraction, exponent) = prepareForMultiplication(other)

      val product = multiply(firstOperandFraction, secondOperandFraction)

      val (overflow, finalExponent, finalFraction) = normalizeAfterMultiplication(sign, exponent, product, roundingMode)

      //finalResult(overflow, sign, finalExponent, finalFraction)(roundingMode, saturationMode, isResultInfty, isResult0, isResultNaN)
      (sign, finalExponent, finalFraction, overflow, isResultInfty, isResult0, isResultNaN)
    }
  }

  def /(other: FloatingPoint): UInt => (UInt, UInt, UInt, Bool, Bool, Bool, Bool) = {
    (roundingMode: UInt) => {
      //val isResultNaN = WireDefault(this.isNaN || other.isNaN || (this.is0 && other.is0) || (this.isInfty && other.isInfty))
      val isResultNaN = {
        if (e5m2) WireDefault(this.isNaN || other.isNaN || (this.is0 && other.is0) || (this.isInfty && other.isInfty))
        else this.isNaN || other.isNaN || other.is0
      }

      //val isResultInfty = WireDefault((other.is0 && !this.is0 && !this.isInfty && !isNaN) || (this.isInfty && !other.isNaN))
      val isResultInfty = {
        if (e5m2) WireDefault((other.is0 && !this.is0 && !this.isInfty && !this.isNaN) || (this.isInfty && !other.isNaN))
        else false.B
      }

      //val isResult0 = WireDefault((this.is0 && !other.is0 && !other.isNaN) || (other.isInfty && !this.isInfty && !this.isNaN))
      val isResult0 = {
        if (e5m2) WireDefault((this.is0 && !other.is0 && !other.isNaN) || (other.isInfty && !this.isInfty && !this.isNaN))
        else is0 && !other.is0 && !other.isNaN
      }

      val resultNaNFractionValue = getResultNaNFractionValue(other)

      //val (sign, dividendFraction, divisorFraction, exponent) = this.prepareForDivision(other)
      val (sign, dividendFraction, divisorFraction, exponent) = prepareForDivision(other)

      //printf(cf"PRINTF: exponent1 $exponent\n")

      val quotient = newtonDiv(dividendFraction, divisorFraction)

      //printf(cf"PRINTF: quotient $quotient\n") // ovo mi ne racuna kako treba

      val (overflow, finalExponent, finalFraction) = normalizeAfterDivision(sign, exponent, quotient, roundingMode)

      //printf(cf"PRINTF: finalExponent $finalExponent; finalFraction $finalFraction\n")

      //finalResult(overflow, sign, finalExponent, finalFraction)(roundingMode, saturationMode, isResultInfty, isResult0, isResultNaN)
      (sign, finalExponent, finalFraction, overflow, isResultInfty, isResult0, isResultNaN)
    }
  }

  def <(other: FloatingPoint): UInt = {
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
}