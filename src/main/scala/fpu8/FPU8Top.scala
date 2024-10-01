package fpu8

import chisel3._

class FPU8Top extends Module {
  val enable = IO(Input(UInt(1.W)))
  val encoding = IO(Input(UInt(1.W)))
  val a = IO(Input(UInt(8.W)))
  val b = IO(Input(UInt(8.W)))
  val opCode = IO(Input(UInt(4.W)))
  val roundingMode = IO(Input(UInt(2.W)))
  val saturationMode = IO(Input(UInt(1.W)))
  val z = IO(Output(UInt(8.W)))

  val aE4M3 = Wire(new FloatingPoint(false))
  val bE4M3 = Wire(new FloatingPoint(false))

  aE4M3.data := a
  bE4M3.data := b

  val aE5M2 = Wire(new FloatingPoint(true))
  val bE5M2 = Wire(new FloatingPoint(true))

  aE5M2.data := a
  bE5M2.data := b

  val addSubE4M3 = Module(new Add(false))
  val addSubE5M2 = Module(new Add(true))

  val multiplyE4M3 = Module(new Multiply(false))
  val multiplyE5M2 = Module(new Multiply(true))

  val divideE4M3 = Module(new Divide(false))
  val divideE5M2 = Module(new Divide(true))

  val compareE4M3 = Module(new Compare(false))
  val compareE5M2 = Module(new Compare(true))

  val generateFinalResultE4M3 = Module(new GenerateFinalResult(false))
  val generateFinalResultE5M2 = Module(new GenerateFinalResult(true))

  addSubE4M3.enable := Mux(encoding === 0.U && (opCode === 0.U || opCode === 1.U), enable, 0.U)
  addSubE4M3.a := aE4M3
  addSubE4M3.b := bE4M3
  addSubE4M3.subtract := Mux(opCode === 1.U, 1.U, 0.U)
  addSubE4M3.roundingMode := roundingMode

  addSubE5M2.enable := Mux(encoding === 1.U && (opCode === 0.U || opCode === 1.U), enable, 0.U)
  addSubE5M2.a := aE5M2
  addSubE5M2.b := bE5M2
  addSubE5M2.subtract := Mux(opCode === 1.U, 1.U, 0.U)
  addSubE5M2.roundingMode := roundingMode

  multiplyE4M3.enable := Mux(encoding === 0.U && opCode === 2.U, enable, 0.U)
  multiplyE4M3.a := aE4M3
  multiplyE4M3.b := bE4M3
  multiplyE4M3.roundingMode := roundingMode

  multiplyE5M2.enable := Mux(encoding === 1.U && opCode === 2.U, enable, 0.U)
  multiplyE5M2.a := aE5M2
  multiplyE5M2.b := bE5M2
  multiplyE5M2.roundingMode := roundingMode

  divideE4M3.enable := Mux(encoding === 0.U && opCode === 3.U, enable, 0.U)
  divideE4M3.a := aE4M3
  divideE4M3.b := bE4M3
  divideE4M3.roundingMode := roundingMode

  divideE5M2.enable := Mux(encoding === 1.U && opCode === 3.U, enable, 0.U)
  divideE5M2.a := aE5M2
  divideE5M2.b := bE5M2
  divideE5M2.roundingMode := roundingMode

  compareE4M3.enable := Mux(encoding === 0.U &&
    (opCode === 4.U || opCode === 5.U || opCode === 6.U || opCode === 7.U || opCode === 8.U || opCode === 9.U),
    enable, 0.U)
  compareE4M3.compareMode := Mux(opCode === 4.U, 0.U,
    Mux(opCode === 5.U, 1.U,
      Mux(opCode === 6.U, 2.U,
        Mux(opCode === 7.U, 3.U,
          Mux(opCode === 8.U, 4.U, 5.U)))))
  compareE4M3.a := aE4M3
  compareE4M3.b := bE4M3

  compareE5M2.enable := Mux(encoding === 1.U &&
    (opCode === 4.U || opCode === 5.U || opCode === 6.U || opCode === 7.U || opCode === 8.U || opCode === 9.U),
    enable, 0.U)
  compareE5M2.compareMode := Mux(opCode === 4.U, 0.U,
    Mux(opCode === 5.U, 1.U,
      Mux(opCode === 6.U, 2.U,
        Mux(opCode === 7.U, 3.U,
          Mux(opCode === 8.U, 4.U, 5.U)))))
  compareE5M2.a := aE5M2
  compareE5M2.b := bE5M2

  generateFinalResultE4M3.enable := Mux(encoding === 0.U, enable, 0.U)
  generateFinalResultE5M2.enable := Mux(encoding === 1.U, enable, 0.U)

  when(encoding === 0.U){
    generateFinalResultE5M2.sign := 0.U
    generateFinalResultE5M2.exponent := 0.U
    generateFinalResultE5M2.mantissa := 0.U
    generateFinalResultE5M2.roundingMode := 0.U
    generateFinalResultE5M2.overflow := 0.U
    generateFinalResultE5M2.saturationMode := 0.U
    generateFinalResultE5M2.isInfty := 0.U
    generateFinalResultE5M2.is0 := 0.U
    generateFinalResultE5M2.isNaN := 0.U
    generateFinalResultE5M2.NaNFractionValue := 0.U

    when(opCode === 0.U || opCode === 1.U){
      generateFinalResultE4M3.sign := addSubE4M3.sign
      generateFinalResultE4M3.exponent := addSubE4M3.exponent(3, 0)
      generateFinalResultE4M3.mantissa := addSubE4M3.fraction(2, 0)
      generateFinalResultE4M3.roundingMode := roundingMode
      generateFinalResultE4M3.overflow := addSubE4M3.overflow
      generateFinalResultE4M3.saturationMode := saturationMode
      generateFinalResultE4M3.isInfty := addSubE4M3.isInfty
      generateFinalResultE4M3.is0 := addSubE4M3.is0
      generateFinalResultE4M3.isNaN := addSubE4M3.isNaN
      generateFinalResultE4M3.NaNFractionValue := addSubE4M3.NaNFractionValue

      z := generateFinalResultE4M3.z
    }.elsewhen(opCode === 2.U) {
      generateFinalResultE4M3.sign := multiplyE4M3.sign
      generateFinalResultE4M3.exponent := multiplyE4M3.exponent(3, 0)
      generateFinalResultE4M3.mantissa := multiplyE4M3.fraction(2, 0)
      generateFinalResultE4M3.roundingMode := roundingMode
      generateFinalResultE4M3.overflow := multiplyE4M3.overflow
      generateFinalResultE4M3.saturationMode := saturationMode
      generateFinalResultE4M3.isInfty := multiplyE4M3.isInfty
      generateFinalResultE4M3.is0 := multiplyE4M3.is0
      generateFinalResultE4M3.isNaN := multiplyE4M3.isNaN
      generateFinalResultE4M3.NaNFractionValue := multiplyE4M3.NaNFractionValue

      z := generateFinalResultE4M3.z
    }.elsewhen(opCode === 3.U){
      generateFinalResultE4M3.sign := divideE4M3.sign
      generateFinalResultE4M3.exponent := divideE4M3.exponent(3, 0)
      generateFinalResultE4M3.mantissa := divideE4M3.fraction(2, 0)
      generateFinalResultE4M3.roundingMode := roundingMode
      generateFinalResultE4M3.overflow := divideE4M3.overflow
      generateFinalResultE4M3.saturationMode := saturationMode
      generateFinalResultE4M3.isInfty := divideE4M3.isInfty
      generateFinalResultE4M3.is0 := divideE4M3.is0
      generateFinalResultE4M3.isNaN := divideE4M3.isNaN
      generateFinalResultE4M3.NaNFractionValue := divideE4M3.NaNFractionValue

      z := generateFinalResultE4M3.z
    }.otherwise{
      generateFinalResultE4M3.sign := 0.U
      generateFinalResultE4M3.exponent := 0.U
      generateFinalResultE4M3.mantissa := 0.U
      generateFinalResultE4M3.roundingMode := 0.U
      generateFinalResultE4M3.overflow := 0.U
      generateFinalResultE4M3.saturationMode := 0.U
      generateFinalResultE4M3.isInfty := 0.U
      generateFinalResultE4M3.is0 := 0.U
      generateFinalResultE4M3.isNaN := 0.U
      generateFinalResultE4M3.NaNFractionValue := 0.U

      z := compareE4M3.z
    }
  }.otherwise{
    generateFinalResultE4M3.sign := 0.U
    generateFinalResultE4M3.exponent := 0.U
    generateFinalResultE4M3.mantissa := 0.U
    generateFinalResultE4M3.roundingMode := 0.U
    generateFinalResultE4M3.overflow := 0.U
    generateFinalResultE4M3.saturationMode := 0.U
    generateFinalResultE4M3.isInfty := 0.U
    generateFinalResultE4M3.is0 := 0.U
    generateFinalResultE4M3.isNaN := 0.U
    generateFinalResultE4M3.NaNFractionValue := 0.U

    when(opCode === 0.U || opCode === 1.U) {
      generateFinalResultE5M2.sign := addSubE5M2.sign
      generateFinalResultE5M2.exponent := addSubE5M2.exponent(4, 0)
      generateFinalResultE5M2.mantissa := addSubE5M2.fraction(1, 0)
      generateFinalResultE5M2.roundingMode := roundingMode
      generateFinalResultE5M2.overflow := addSubE5M2.overflow
      generateFinalResultE5M2.saturationMode := saturationMode
      generateFinalResultE5M2.isInfty := addSubE5M2.isInfty
      generateFinalResultE5M2.is0 := addSubE5M2.is0
      generateFinalResultE5M2.isNaN := addSubE5M2.isNaN
      generateFinalResultE5M2.NaNFractionValue := addSubE5M2.NaNFractionValue

      z := generateFinalResultE5M2.z
    }.elsewhen(opCode === 2.U){
      generateFinalResultE5M2.sign := multiplyE5M2.sign
      generateFinalResultE5M2.exponent := multiplyE5M2.exponent(4, 0)
      generateFinalResultE5M2.mantissa := multiplyE5M2.fraction(1, 0)
      generateFinalResultE5M2.roundingMode := roundingMode
      generateFinalResultE5M2.overflow := multiplyE5M2.overflow
      generateFinalResultE5M2.saturationMode := saturationMode
      generateFinalResultE5M2.isInfty := multiplyE5M2.isInfty
      generateFinalResultE5M2.is0 := multiplyE5M2.is0
      generateFinalResultE5M2.isNaN := multiplyE5M2.isNaN
      generateFinalResultE5M2.NaNFractionValue := multiplyE5M2.NaNFractionValue

      z := generateFinalResultE5M2.z
    }.elsewhen(opCode === 3.U) {
      generateFinalResultE5M2.sign := divideE5M2.sign
      generateFinalResultE5M2.exponent := divideE5M2.exponent(4, 0)
      generateFinalResultE5M2.mantissa := divideE5M2.fraction(1, 0)
      generateFinalResultE5M2.roundingMode := roundingMode
      generateFinalResultE5M2.overflow := divideE5M2.overflow
      generateFinalResultE5M2.saturationMode := saturationMode
      generateFinalResultE5M2.isInfty := divideE5M2.isInfty
      generateFinalResultE5M2.is0 := divideE5M2.is0
      generateFinalResultE5M2.isNaN := divideE5M2.isNaN
      generateFinalResultE5M2.NaNFractionValue := divideE5M2.NaNFractionValue

      z := generateFinalResultE5M2.z
    }.otherwise{
      generateFinalResultE5M2.sign := 0.U
      generateFinalResultE5M2.exponent := 0.U
      generateFinalResultE5M2.mantissa := 0.U
      generateFinalResultE5M2.roundingMode := 0.U
      generateFinalResultE5M2.overflow := 0.U
      generateFinalResultE5M2.saturationMode := 0.U
      generateFinalResultE5M2.isInfty := 0.U
      generateFinalResultE5M2.is0 := 0.U
      generateFinalResultE5M2.isNaN := 0.U
      generateFinalResultE5M2.NaNFractionValue := 0.U

      z := compareE5M2.z
    }
  }
}
