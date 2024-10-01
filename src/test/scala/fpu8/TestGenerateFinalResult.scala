package fpu8

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestGenerateFinalResult extends AnyFlatSpec with ChiselScalatestTester {
  "GenerateFinalResult" should "give the 8-bit floating point representation of the result" in {
    test(new GenerateFinalResult(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.sign.poke(0.U)
      dut.exponent.poke(12.U)
      dut.mantissa.poke(3.U)
      dut.roundingMode.poke(0.U)
      dut.overflow.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.isInfty.poke(0.U)
      dut.is0.poke(0.U)
      dut.isNaN.poke(0.U)
      dut.NaNFractionValue.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.sign.poke(1.U)
      dut.exponent.poke(12.U)
      dut.mantissa.poke(3.U)
      dut.roundingMode.poke(0.U)
      dut.overflow.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.isInfty.poke(0.U)
      dut.is0.poke(0.U)
      dut.isNaN.poke(0.U)
      dut.NaNFractionValue.poke(0.U)
      dut.clock.step()
      dut.z.expect(227.U)
    }
  }
}
