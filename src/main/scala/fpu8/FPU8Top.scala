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
  val status = IO(Output(UInt(5.W)))

  val aE4M3 = Wire(new FloatingPoint(false))
  val bE4M3 = Wire(new FloatingPoint(false))

  aE4M3.data := a
  bE4M3.data := b

  val aE5M2 = Wire(new FloatingPoint(true))
  val bE5M2 = Wire(new FloatingPoint(true))

  aE5M2.data := a
  bE5M2.data := b

  val FPU8E4M3 = Module(new FPU8Generator(false))
  val FPU8E5M2 = Module(new FPU8Generator(true))

  FPU8E4M3.enable := Mux(encoding === 0.U, enable, 0.U)
  FPU8E4M3.a := aE4M3
  FPU8E4M3.b := bE4M3
  FPU8E4M3.opCode := opCode
  FPU8E4M3.roundingMode := roundingMode
  FPU8E4M3.saturationMode := saturationMode

  FPU8E5M2.enable := Mux(encoding === 1.U, enable, 0.U)
  FPU8E5M2.a := aE5M2
  FPU8E5M2.b := bE5M2
  FPU8E5M2.opCode := opCode
  FPU8E5M2.roundingMode := roundingMode
  FPU8E5M2.saturationMode := saturationMode

  when(encoding === 0.U){
    z := FPU8E4M3.z
    status := FPU8E4M3.status
  }.elsewhen(encoding === 1.U){
    z := FPU8E5M2.z
    status := FPU8E5M2.status
  }.otherwise{
    z := 0.U
    status := 0.U
  }
}
