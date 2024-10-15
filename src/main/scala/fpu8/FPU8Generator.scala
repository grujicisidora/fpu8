package fpu8

import chisel3._

class FPU8Generator(val e5m2: Boolean) extends Module {
  val exponentLength = if (e5m2) 5 else 4
  val mantissaLength = if (e5m2) 2 else 3

  val enable = IO(Input(UInt(1.W)))
  val a = IO(Input(new FloatingPoint(e5m2)))
  val b = IO(Input(new FloatingPoint(e5m2)))
  val opCode = IO(Input(UInt(4.W)))
  val roundingMode = IO(Input(UInt(2.W)))
  val saturationMode = IO(Input(UInt(1.W)))
  val z = IO(Output(UInt(8.W)))
  val status = IO(Output(UInt(5.W)))

  val addSub = Module(new Add(e5m2))

  val multiply = Module(new Multiply(e5m2))

  val divide = Module(new Divide(e5m2))

  val compare = Module(new Compare(e5m2))

  val convert = Module(new Convert(e5m2))

  val generateFinalResult = Module(new GenerateFinalResult(e5m2))

  addSub.enable := Mux(opCode === 0.U || opCode === 1.U, enable, 0.U)
  addSub.a := a
  addSub.b := b
  addSub.subtract := Mux(opCode === 1.U, 1.U, 0.U)
  addSub.roundingMode := roundingMode

  multiply.enable := Mux(opCode === 2.U, enable, 0.U)
  multiply.a := a
  multiply.b := b
  multiply.roundingMode := roundingMode

  divide.enable := Mux(opCode === 3.U, enable, 0.U)
  divide.a := a
  divide.b := b
  divide.roundingMode := roundingMode

  compare.enable := Mux(opCode === 4.U || opCode === 5.U || opCode === 6.U || opCode === 7.U || opCode === 8.U || opCode === 9.U,
    enable, 0.U)
  compare.compareMode := Mux(opCode === 4.U, 0.U,
    Mux(opCode === 5.U, 1.U,
      Mux(opCode === 6.U, 2.U,
        Mux(opCode === 7.U, 3.U,
          Mux(opCode === 8.U, 4.U, 5.U)))))
  compare.a := a
  compare.b := b

  convert.enable := Mux(opCode === 10.U, enable, 0.U)
  convert.a := a
  convert.roundingMode := roundingMode
  convert.saturationMode := saturationMode

  generateFinalResult.enable := enable

  when(opCode === 0.U || opCode === 1.U) {
    generateFinalResult.sign := addSub.sign
    generateFinalResult.exponent := addSub.exponent(exponentLength - 1, 0)
    generateFinalResult.mantissa := addSub.fraction(mantissaLength - 1, 0)
    generateFinalResult.roundingMode := roundingMode
    generateFinalResult.overflow := addSub.status(4)
    generateFinalResult.saturationMode := saturationMode
    generateFinalResult.isInfty := addSub.status(1)
    generateFinalResult.is0 := addSub.status(0)
    generateFinalResult.isNaN := addSub.status(2)
    generateFinalResult.NaNFractionValue := addSub.NaNFractionValue

    z := generateFinalResult.z
    status := addSub.status
  }.elsewhen(opCode === 2.U) {
    generateFinalResult.sign := multiply.sign
    generateFinalResult.exponent := multiply.exponent(exponentLength - 1, 0)
    generateFinalResult.mantissa := multiply.fraction(mantissaLength - 1, 0)
    generateFinalResult.roundingMode := roundingMode
    generateFinalResult.overflow := multiply.status(4)
    generateFinalResult.saturationMode := saturationMode
    generateFinalResult.isInfty := multiply.status(1)
    generateFinalResult.is0 := multiply.status(0)
    generateFinalResult.isNaN := multiply.status(2)
    generateFinalResult.NaNFractionValue := multiply.NaNFractionValue

    z := generateFinalResult.z
    status := multiply.status
  }.elsewhen(opCode === 3.U) {
    generateFinalResult.sign := divide.sign
    generateFinalResult.exponent := divide.exponent(exponentLength - 1, 0)
    generateFinalResult.mantissa := divide.fraction(mantissaLength - 1, 0)
    generateFinalResult.roundingMode := roundingMode
    generateFinalResult.overflow := divide.status(4)
    generateFinalResult.saturationMode := saturationMode
    generateFinalResult.isInfty := divide.status(1)
    generateFinalResult.is0 := divide.status(0)
    generateFinalResult.isNaN := divide.status(2)
    generateFinalResult.NaNFractionValue := divide.NaNFractionValue

    z := generateFinalResult.z
    status := divide.status
  }.elsewhen(opCode === 4.U || opCode === 5.U || opCode === 6.U || opCode === 7.U || opCode === 8.U || opCode === 9.U){
    generateFinalResult.sign := 0.U
    generateFinalResult.exponent := 0.U
    generateFinalResult.mantissa := 0.U
    generateFinalResult.roundingMode := 0.U
    generateFinalResult.overflow := 0.U
    generateFinalResult.saturationMode := 0.U
    generateFinalResult.isInfty := 0.U
    generateFinalResult.is0 := 0.U
    generateFinalResult.isNaN := 0.U
    generateFinalResult.NaNFractionValue := 0.U

    z := compare.z
    status := compare.status
  }.elsewhen(opCode === 10.U){
    generateFinalResult.sign := 0.U
    generateFinalResult.exponent := 0.U
    generateFinalResult.mantissa := 0.U
    generateFinalResult.roundingMode := 0.U
    generateFinalResult.overflow := 0.U
    generateFinalResult.saturationMode := 0.U
    generateFinalResult.isInfty := 0.U
    generateFinalResult.is0 := 0.U
    generateFinalResult.isNaN := 0.U
    generateFinalResult.NaNFractionValue := 0.U

    z := convert.z
    status := convert.status
  }.otherwise{
    generateFinalResult.sign := 0.U
    generateFinalResult.exponent := 0.U
    generateFinalResult.mantissa := 0.U
    generateFinalResult.roundingMode := 0.U
    generateFinalResult.overflow := 0.U
    generateFinalResult.saturationMode := 0.U
    generateFinalResult.isInfty := 0.U
    generateFinalResult.is0 := 0.U
    generateFinalResult.isNaN := 0.U
    generateFinalResult.NaNFractionValue := 0.U

    z := 0.U
    status := 0.U
  }
}
