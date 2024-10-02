package fpu8

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestMultiply extends AnyFlatSpec with ChiselScalatestTester {
  "Multiply" should "calculate the value of expression a * b" in {
    test(new Multiply(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.roundingMode.poke(0.U) // to nearest
      dut.clock.step()
      dut.sign.expect(0.U)
      dut.exponent.expect(0.U)
      dut.fraction.expect(0.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.roundingMode.poke(0.U) // to nearest
      dut.clock.step()
      dut.sign.expect(0.U)
      dut.exponent.expect(7.U)
      dut.fraction.expect(14.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(170.U) // 1_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.roundingMode.poke(0.U) // to nearest
      dut.clock.step()
      dut.sign.expect(1.U)
      dut.exponent.expect(7.U)
      dut.fraction.expect(14.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()
    }
  }
}
