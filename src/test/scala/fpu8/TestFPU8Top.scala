package fpu8

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestFPU8Top extends AnyFlatSpec with ChiselScalatestTester {
  "FPU8Top" should "calculate various expressions between numbers in two different fp8 representations." in {
    test(new FPU8Top).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // e4m3 encoding
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

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(76.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(170.U)
      dut.b.poke(75.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(74.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(79.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(80.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(75.U)
      dut.b.poke(42.U)
      dut.opCode.poke(1.U) // -
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(74.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(2.U) // *
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(62.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(170.U)
      dut.b.poke(75.U)
      dut.opCode.poke(2.U) // *
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(190.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(75.U)
      dut.b.poke(42.U)
      dut.opCode.poke(3.U) // /
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(89.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(0.U)
      dut.a.poke(75.U)
      dut.b.poke(42.U)
      dut.opCode.poke(10.U) // convert
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(70.U)

      // e5m2 encoding

      dut.enable.poke(0.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(4.U) // <
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(4.U) // <
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(60.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(5.U) // >
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(6.U) // ==
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(7.U) // <=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(60.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(42.U)
      dut.opCode.poke(8.U) // >=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(60.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(9.U) // !=
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(60.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(75.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(170.U)
      dut.b.poke(75.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(75.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(79.U)
      dut.opCode.poke(0.U) // +
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(79.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(75.U)
      dut.b.poke(42.U)
      dut.opCode.poke(1.U) // -
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(75.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(42.U)
      dut.b.poke(75.U)
      dut.opCode.poke(2.U) // *
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(58.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(170.U)
      dut.b.poke(75.U)
      dut.opCode.poke(2.U) // *
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(186.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(75.U)
      dut.b.poke(42.U)
      dut.opCode.poke(3.U) // /
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(93.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.encoding.poke(1.U)
      dut.a.poke(75.U)
      dut.b.poke(42.U)
      dut.opCode.poke(10.U) // convert
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(86.U)
    }
  }
}
