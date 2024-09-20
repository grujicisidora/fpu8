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

  def isAbsValGreater(other: FloatingPoint): Bool = this.data(6, 0) > other.data(6, 0)

  // ova funkcija bi trebalo da promeni naziv, jer se porede apsolutne vrednosti brojeva
  def isAbsValEqualTo(other: FloatingPoint): Bool = this.data(6, 0) === other.data(6, 0)

  def isExp0: Bool = exponent === 0.U

  def isExpMax: Bool = exponent === maxExponent.U

  def isMantissa0: Bool = mantissa === 0.U

  def isMantissaMax: Bool = mantissa === maxMantissa.U

  def isNaN: Bool = isExpMax && !isMantissa0

  def isInfty: Bool = isExpMax && isMantissa0

  def is0: Bool = isExp0 && isMantissa0

  def shiftToMSB1(value: UInt): (UInt, UInt) = {
    val width = value.getWidth
    val leadingZeros = Wire(UInt(width.W))
    val shiftedValue = Wire(UInt(width.W))

    leadingZeros := PriorityEncoder(Reverse(value))

    shiftedValue := value << leadingZeros

    (shiftedValue, leadingZeros)
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

  def prepareForAddition(other: FloatingPoint)(subtract: UInt): (UInt, UInt, UInt, UInt, UInt) = {
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

  def normalizeAfterAddition(sign: UInt, exponent: UInt, calculatedValue: UInt, roundingMode: UInt): (Bool, UInt, UInt) = {
    val tempFractionLength = mantissaLength + 4

    val roundedFractionMSBIndex = mantissaLength + 1

    val calculatedValueLength = tempFractionLength + 1

    val originalCalculatedValue = calculatedValue.asTypeOf(UInt(calculatedValueLength.W)) // zadrzavam duzinu, mada mozda moze i bez ovoga

    val paddedCalcValue = Wire(UInt(8.W))

    // dodala sam jednu nulu desno zbog dela koji sledi - brisanja nula sa leve strane, odnosno pomeranja
    // u sustini se dobije kao rezultat sabiranja

    paddedCalcValue := {
      if (format == 0) originalCalculatedValue
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
    // morala je da se napravi nova promenljiva zbog faze zaokruzivanja broja
    val tempFraction1 = tempFraction(6, 7 - tempFractionLength)
    val addOne = (!roundingMode(1) && !roundingMode(0) && tempFraction1(2) && (tempFraction1(1) || tempFraction1(0))) || // zaokruzivanje do najblizeg
      (!roundingMode(1) && !roundingMode(0) && tempFraction1(2) && !tempFraction1(1) && !tempFraction1(0) && tempFraction1(3)) || // zaokruzivanje do najblizeg
      (!roundingMode(1) && roundingMode(0) && (tempFraction1(2) || tempFraction1(1) || tempFraction1(0)) && sign.asBool) || // prema -infty tj. prema nizem
      (roundingMode(1) && !roundingMode(0) && (tempFraction1(2) || tempFraction1(1) || tempFraction1(0)) && !sign.asBool) // prema +infty tj. prema visem

    val roundedFraction = tempFraction1(tempFractionLength - 1, 3) +& addOne.asUInt

    val finalFraction = Mux(roundedFraction(roundedFractionMSBIndex) === 1.U, roundedFraction(roundedFractionMSBIndex, 1), roundedFraction(roundedFractionMSBIndex - 1, 0))

    val finalExponent = Wire(UInt((exponentLength + 1).W))

    finalExponent := Mux(roundedFraction(roundedFractionMSBIndex) === 1.U || (roundedFraction(roundedFractionMSBIndex - 1) === 1.U && !tempExponent.orR),
      tempExponent +& 1.U,
      tempExponent
    )

    val overflow = (finalExponent >= (maxExponent.U +& 1.U)) || (tempExponent === maxExponent.U)
    (overflow, finalExponent, finalFraction)
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

  def additionFinalResult(overflow: Bool, sign: UInt, finalExponent: UInt, finalFraction: UInt)(roundingMode: UInt, saturationMode: UInt, isInfty: Bool, is0: Bool, isNaN: Bool) : UInt = {
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
  }


  def +(other: FloatingPoint)(roundingMode: UInt, saturationMode: UInt, subtract: UInt): UInt = {
    val (sign, greaterOperandFraction, smallerOperandFraction, exponent, subtraction) = this.prepareForAddition(other)(subtract)

    val isResultNaN = WireDefault(this.isNaN || other.isNaN || (this.isInfty && other.isInfty && subtraction.asBool))
    val isResultInfty = WireDefault((this.isInfty || other.isInfty) & !isResultNaN)
    val isResult0 = WireDefault(this.isAbsValEqualTo(other) && subtraction.asBool && !isResultNaN && !isResultInfty)

    val calculatedValue = Mux(subtraction === 1.U,
      greaterOperandFraction -& smallerOperandFraction, // xx.xxx...
      greaterOperandFraction +& smallerOperandFraction) // xx.xxx...

    val (overflow, finalExponent, finalFraction) = normalizeAfterAddition(sign, exponent, calculatedValue, roundingMode)

    additionFinalResult(overflow, sign, finalExponent, finalFraction)(roundingMode, saturationMode, isResultInfty, isResult0, isResultNaN)
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

    additionFinalResult(overflow, sign, finalExponent, finalFraction)(roundingMode, saturationMode, isResultInfty, isResult0, isResultNaN)
  }

}
