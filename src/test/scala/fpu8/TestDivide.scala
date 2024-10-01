package fpu8

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestDivide extends AnyFlatSpec with ChiselScalatestTester {
  "Divide" should "calculate the value of expression a / b" in {
    test(new Divide(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.clock.step()
      //dut.z.expect(0.U)
      dut.sign.expect(0.U)
      dut.exponent.expect(0.U)
      dut.fraction.expect(0.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(75.U) // 0_1001_011
      dut.b.data.poke(42.U) // 0_0101_010
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.clock.step()
      //dut.z.expect(89.U) // 0_1011_001
      dut.sign.expect(0.U)
      dut.exponent.expect(11.U)
      dut.fraction.expect(9.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.clock.step()
      //dut.z.expect(22.U) // 0_0010_110
      dut.sign.expect(0.U)
      dut.exponent.expect(2.U)
      dut.fraction.expect(14.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)
    }
  }
}
