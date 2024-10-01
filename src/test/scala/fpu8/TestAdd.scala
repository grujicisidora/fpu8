package fpu8

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestAdd extends AnyFlatSpec with ChiselScalatestTester {
  "Add" should "calculate the value of expression a + b" in {
    test(new Add(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.subtract.poke(0.U)
      dut.clock.step()
      //dut.z.expect(0.U)
      dut.sign.expect(0.U)
      dut.exponent.expect(0.U)
      dut.fraction.expect(0.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      /* val sign = IO(Output(UInt(1.W)))
      val exponent = IO(Output(UInt((exponentLength + 1).W)))
      val fraction = IO(Output(UInt((mantissaLength + 1).W)))
      val overflow = IO(Output(UInt(1.W)))
      val isInfty = IO(Output(UInt(1.W)))
      val is0 = IO(Output(UInt(1.W)))
      val isNaN = IO(Output(UInt(1.W))) */

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.subtract.poke(0.U)
      dut.clock.step()
      //dut.z.expect(76.U) // 0_1001_100
      dut.sign.expect(0.U)
      dut.exponent.expect(9.U)
      dut.fraction.expect(12.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(170.U) // 1_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.subtract.poke(0.U)
      dut.clock.step()
      //dut.z.expect(74.U) // 0_1001_010
      dut.sign.expect(0.U)
      dut.exponent.expect(9.U)
      dut.fraction.expect(10.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(79.U) // 0_1001_111
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.subtract.poke(0.U)
      dut.clock.step()
      //dut.z.expect(80.U) // 0_1010_000
      dut.sign.expect(0.U)
      dut.exponent.expect(10.U)
      dut.fraction.expect(8.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(170.U) // 1_0101_010
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.subtract.poke(0.U)
      dut.clock.step()
      //dut.z.expect(128.U) // 1_0000_000 - nula
      dut.sign.expect(1.U)
      dut.exponent.expect(0.U)
      dut.fraction.expect(0.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(1.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(250.U) // 1_1111_010
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.subtract.poke(0.U)
      dut.clock.step()
      //dut.z.expect(250.U) // 1_1111_010
      dut.sign.expect(1.U)
      dut.exponent.expect(15.U)
      dut.fraction.expect(10.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)

      dut.clock.step()
    }
  }

  it should "also calculate the value of expression a - b" in {
    test(new Add(false)) { dut =>
      dut.enable.poke(1.U)
      dut.a.data.poke(75.U) // 0_1001_011
      dut.b.data.poke(42.U) // 0_0101_010
      dut.roundingMode.poke(0.U) // to nearest
      //dut.saturationMode.poke(0.U)
      dut.subtract.poke(1.U)
      dut.clock.step()
      //dut.z.expect(74.U) // 0_1001_010
      dut.sign.expect(0.U)
      dut.exponent.expect(9.U)
      dut.fraction.expect(10.U)
      dut.overflow.expect(0.U)
      dut.isInfty.expect(0.U)
      dut.is0.expect(0.U)
      dut.isNaN.expect(0.U)
    }
  }
}
