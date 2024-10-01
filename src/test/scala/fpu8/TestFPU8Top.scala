package fpu8

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestFPU8Top extends AnyFlatSpec with ChiselScalatestTester {
  "FPU8Top" should "calculate various expressions between numbers in two different fp8 representations." in {
    test(new FPU8Top).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(4.U) // <
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(4.U) // <
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(5.U) // >
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(6.U) // ==
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(7.U) // <=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(42.U)
      dut.opCode.poke(8.U) // >=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(9.U) // !=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U)
    }
  }
}
