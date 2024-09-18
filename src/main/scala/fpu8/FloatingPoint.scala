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

  def generateROMforDiv: Vec[UInt] = {
    /*val vecSize = pow(2, mantissaLength).toInt

    val romDiv = Vec(vecSize, UInt((mantissaLength + 1).W))

    for (i <- 0 until vecSize) {
      romDiv(i) := (14 - i * 2 + format).U
    }

    romDiv*/

    val vecSize = pow(2, mantissaLength).toInt

    // Initialize a Vec with VecInit and a sequence of values
    val romDiv = VecInit(Seq.fill(vecSize)(0.U((mantissaLength + 1).W)))

    for (i <- 0 until vecSize) {
      romDiv(i) := (14 - i * 2 + format).U
    }

    romDiv
  }

  def multiply(lengthA: Int, lengthB: Int, a: UInt, b: UInt): UInt = {
    val partialProducts = Seq.tabulate(lengthB)(i => {
      val compare = Mux(b(i), ((1 << lengthA) - 1).U(lengthA.W), 0.U(lengthA.W))
      ((a & compare) << i).asUInt
    })

    def reducePartialProducts(pp: Seq[UInt]): UInt = {
      if (pp.size == 1) {
        //val res = UInt((lengthA + lengthB).W)
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
    printf(cf"PRINTF: dividendFraction $dividendFraction\n")

    val (divisorFraction, divisorShift) = shiftToMSB1(tempDivisorFraction)
    printf(cf"PRINTF: divisorFraction $divisorFraction\n")

    // zamenila sam -& i +& operatore sa - i +
    val tempExponent = Cat(Fill(2, 0.U), this.exponent) -
      Cat(Fill(2, 0.U), other.exponent) +
      Fill(exponentLength - 1, 1.U) // dodaje se bias na stvarni eksponent

    val exponent = tempExponent -& dividendShift +& divisorShift
    printf(cf"PRINTF: exponent $exponent\n")

    (sign, dividendFraction, divisorFraction, exponent)
  }

  def newtonDiv(dividendFraction: UInt, divisorFraction: UInt) = {
    val rom = generateROMforDiv

    val initGuess = Cat(1.U(2.W), rom(divisorFraction(mantissaLength - 1, 0)))
    printf(cf"PRINTF: initGuess $initGuess\n")

    // ovo moze da se parametrizuje bolje i iskoristi jos negde sigurno
    def round(value: UInt): UInt = {
      val valueLength = value.getWidth
      val msbIndex = valueLength - 2
      val lsbIndex = valueLength - 7 + format
      val roundedValue = {
        if (valueLength >= 7) {
          // stavila sam + umesto +& operatora
          Cat(0.U, value(msbIndex, lsbIndex)) + (value(lsbIndex - 1) & value(lsbIndex - 2, lsbIndex - 3).orR.asUInt)
        } else
          Cat(0.U, value(msbIndex, lsbIndex))
      }

      roundedValue
    }

    def iteration(xi: UInt, divisorFrac: UInt): UInt = {
      // x_i * divisorFrac
      val firstStep = multiply(mantissaLength + 3, mantissaLength + 1, xi, divisorFrac)
      printf(cf"PRINTF: firstStep $firstStep\n")


      val firstStepRnd = round(firstStep)
      printf(cf"PRINTF: firstStepRnd $firstStepRnd\n")

      val firstStepRndLength = firstStepRnd.getWidth
      //val firstStepRndLength = 2*mantissaLength + 1
      printf(cf"PRINTF: firstStepRndLength $firstStepRndLength\n")

      // 2 - x_i * divisorFrac
      // stavila + umesto +& da isprobam
      val secondStep = (~firstStepRnd(firstStepRndLength - 2, 0)).asUInt + 1.U
      printf(cf"PRINTF: secondStep $secondStep\n")

      // x_i * (2 - x_i * divisorFrac)
      //val finalStep = multiply(mantissaLength + 3, mantissaLength + 3, xi, secondStep)
      val finalStep = Wire(UInt((2*mantissaLength + 6).W))
      finalStep := multiply(mantissaLength + 3, mantissaLength + 3, xi, secondStep)
      printf(cf"PRINTF: finalStep $finalStep\n")

      val res = round(finalStep)
      printf(cf"PRINTF: res $res\n")

      val resLength = res.getWidth
      printf(cf"PRINTF: resLength $resLength\n")

      // ovo gore posle moze i da se izbaci
      res
    }

    val secondGuess = iteration(initGuess, divisorFraction)
    printf(cf"PRINTF: secondGuess $secondGuess\n")

    val finalGuess = iteration(secondGuess(mantissaLength + 2, 0), divisorFraction)
    printf(cf"PRINTF: finalGuess $finalGuess\n")

    multiply(mantissaLength + 3, mantissaLength + 1, finalGuess(mantissaLength + 2, 0), dividendFraction)
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

  def normalizeForDivision(sign: UInt, exponent: UInt, calculatedValue: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val calculatedValueLength = calculatedValue.getWidth

    val moveDecPoint8 = {
      if (calculatedValueLength < 9) false.B
      else !calculatedValue(calculatedValueLength - 2, calculatedValueLength - 9).orR
    }

    val firstShift = {
      if (moveDecPoint8 == true.B) Cat(calculatedValue(0), Fill(8, 0.U))
      else calculatedValue(calculatedValueLength - 2, 0)
    }

    val moveDecPoint4 = !firstShift(calculatedValueLength - 2, calculatedValueLength - 5).orR

    val secondShift = {
      if (moveDecPoint4 == true.B) Cat(calculatedValue(calculatedValueLength - 6, 0), Fill(4, 0.U))
      else firstShift
    }

    val moveDecPoint2 = !secondShift(calculatedValueLength - 2, calculatedValueLength - 3).orR

    val thirdShift = {
      if (moveDecPoint2 == true.B) Cat(secondShift(calculatedValueLength - 4, 0), Fill(2, 0.U))
      else secondShift
    }

    val moveDecPoint1 = !thirdShift(calculatedValueLength - 2)

    val shiftedCalcValue = {
      if (moveDecPoint1 == true.B) Cat(thirdShift(calculatedValueLength - 3, 0), 0.U)
      else thirdShift
    }

    val moveDecPoint = 0.U + {
      if (moveDecPoint8 == true.B) 8.U
      else 0.U
    } + {
      if (moveDecPoint4 == true.B) 4.U
      else 0.U
    } + {
      if (moveDecPoint2 == true.B) 2.U
      else 0.U
    } + {
      if (moveDecPoint1 == true.B) 1.U
      else 0.U
    }

    val exponentShiftRight = 0.U((exponentLength + 2).W) -& exponent
    val exponentShiftLeft = exponent -& 1.U((exponentLength + 2).W)

    val tempExponent = Wire(UInt((exponentLength + 1).W))
    val tempFraction = Wire(UInt((calculatedValueLength - 2 + format).W))

    when(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) >= maxExponent.U((exponentLength + 1).W))
      && calculatedValue(calculatedValueLength - 1)) { // exponent overflow
      tempExponent := maxExponent.U((exponentLength + 1).W)
      tempFraction := ((1 << (8 - format)) - 1).U((8 - format).W)
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) < maxExponent.U((exponentLength + 1).W))
      && calculatedValue(calculatedValueLength - 1)) { // fraction overflow
      tempExponent := exponent(exponentLength, 0) +& 1.U
      tempFraction := {
        if (calculatedValueLength >= 10)
          calculatedValue(calculatedValueLength - 2, calculatedValueLength - 8 + format) +&
            calculatedValue(calculatedValueLength - 9, 0).andR.asUInt
        else
          calculatedValue(calculatedValueLength - 2, calculatedValueLength - 8 + format)
      }
    }.elsewhen(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) >= moveDecPoint)
      && shiftedCalcValue(calculatedValueLength - 2)) { // normalizovani broj koji je pomeren tako da pocinje sa 1
      tempExponent := exponent(exponentLength, 0) -& moveDecPoint
      tempFraction := {
        if (calculatedValueLength >= 10)
          shiftedCalcValue(calculatedValueLength - 2, calculatedValueLength - 8 + format) +&
            shiftedCalcValue(calculatedValueLength - 9, 0).andR.asUInt
        else
          shiftedCalcValue(calculatedValueLength - 2, calculatedValueLength - 8 + format)
      }
    }.otherwise {
      tempExponent := 0.U // denormalizovani broj ili 0
      when(!exponent(exponentLength + 1) && (exponent(exponentLength, 0) >= 0.U)) {
        tempFraction := {
          if (calculatedValueLength >= 10)
            (calculatedValue(calculatedValueLength - 2, calculatedValueLength - 8 + format) +&
              calculatedValue(calculatedValueLength - 9, 0).andR.asUInt) << exponentShiftLeft
          else
            calculatedValue(calculatedValueLength - 2, calculatedValueLength - 8 + format) << exponentShiftLeft
        }
      }.otherwise {
        tempFraction := (calculatedValue(calculatedValueLength - 1, calculatedValueLength - 7 + format) +&
          calculatedValue(calculatedValueLength - 8 + format, calculatedValueLength - 9 + format).andR.asUInt) >> exponentShiftRight// nema koliko vise da se pomeri
      }
    }
    // morala je da se napravi nova promenljiva zbog faze zaokruzivanja broja

    printf(cf"PRINTF: tempExponent $tempExponent\n")

    val addOne = (!roundingMode(1) && !roundingMode(0) && tempFraction(2) && (tempFraction(1) || tempFraction(0))) || // zaokruzivanje do najblizeg
      (!roundingMode(1) && !roundingMode(0) && tempFraction(2) && !tempFraction(1) && !tempFraction(0) && tempFraction(3)) || // zaokruzivanje do najblizeg
      (!roundingMode(1) && roundingMode(0) && (tempFraction(2) || tempFraction(1) || tempFraction(0)) && sign.asBool) || // prema -infty
      (roundingMode(1) && !roundingMode(0) && (tempFraction(2) || tempFraction(1) || tempFraction(0)) && !sign.asBool) // prema +infty

    val roundedFraction = Cat(0.U, tempFraction(6 - format, 3)) + addOne.asUInt

    val finalFraction = Mux(roundedFraction(mantissaLength + 1) === 1.U,
      roundedFraction(mantissaLength + 1, 1), roundedFraction(mantissaLength, 0))

    val finalExponent = Mux(roundedFraction(mantissaLength + 1) === 1.U || (roundedFraction(mantissaLength) && tempExponent === 0.U),
      tempExponent +& 1.U((exponentLength + 1).W), tempExponent)

    val overflow = (finalExponent >= (maxExponent.U +& 1.U)) || (tempExponent === maxExponent.U) ||
      (finalFraction === ((1 << mantissaLength) - 1).U(mantissaLength.W))

    (overflow, finalExponent(exponentLength - 1, 0), finalFraction(mantissaLength - 1, 0))
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
    val isResultNaN = WireDefault(this.isNaN || other.isNaN || (this.is0 && other.is0) || (this.isInfty && other.isInfty))
    val isResultInfty = WireDefault((other.is0 && !this.is0 && !this.isInfty && !isNaN) || (this.isInfty && !other.isNaN))
    val isResult0 = WireDefault((this.is0 && !other.is0 && !other.isNaN) || (other.isInfty && !this.isInfty && !this.isNaN))

    val (sign, dividendFraction, divisorFraction, exponent) = this.alignForDivision(other)

    //printf(cf"PRINTF: exponent $exponent\n")

    val quotient = newtonDiv(dividendFraction, divisorFraction)

    printf(cf"PRINTF: quotient $quotient\n") // ovo mi ne racuna kako treba

    val quotientLength = quotient.getWidth

    printf(cf"PRINTF: quotientLength $quotientLength\n")

    val (overflow, finalExponent, finalFraction) = normalizeForDivision(sign, exponent, quotient, roundingMode)

    printf(cf"PRINTF: finalExponent $finalExponent; finalFraction $finalFraction\n")

    additionFinalResult(overflow, sign, finalExponent, finalFraction, mantissaLength + 1)(roundingMode, saturationMode, isResultInfty, isResult0, isResultNaN)
  }

}
