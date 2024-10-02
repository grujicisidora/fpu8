package fpu8

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestFPU8Generator extends AnyFlatSpec with ChiselScalatestTester {
  "FPU8Generator" should "calculate various expressions between numbers in a E4M3 fp8 representation." in {
    test(new FPU8Generator(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(4.U) // <
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(4.U) // <
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(5.U) // >
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(6.U) // ==
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(7.U) // <=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(42.U)
      dut.opCode.poke(8.U) // >=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(9.U) // !=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(76.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(170.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(74.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(79.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(80.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(75.U)
      dut.b.data.poke(42.U)
      dut.opCode.poke(1.U) // -
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(74.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(2.U) // *
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(62.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(170.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(2.U) // *
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(190.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(75.U)
      dut.b.data.poke(42.U)
      dut.opCode.poke(3.U) // /
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(89.U)
    }
  }

  it should "also calculate various expressions between numbers in a E5M2 fp8 representation." in {
    test(new FPU8Generator(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(4.U) // <
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(4.U) // <
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(60.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(5.U) // >
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(6.U) // ==
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(7.U) // <=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(60.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(42.U)
      dut.opCode.poke(8.U) // >=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(60.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(9.U) // !=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(60.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(75.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(170.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(75.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(79.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(79.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(75.U)
      dut.b.data.poke(42.U)
      dut.opCode.poke(1.U) // -
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(75.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(2.U) // *
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(58.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(170.U)
      dut.b.data.poke(75.U)
      dut.opCode.poke(2.U) // *
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(186.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(75.U)
      dut.b.data.poke(42.U)
      dut.opCode.poke(3.U) // /
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(93.U)
    }
  }
}
