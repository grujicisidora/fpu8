package fpu8

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestConvert extends AnyFlatSpec with ChiselScalatestTester {
  "Convert" should "convert fpu8 from E4M3 encoding to E5M2 encoding." in {
    test(new Convert(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.a.data.poke(75.U)
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(75.U)
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(70.U)
    }
  }

  it should "also convert fpu8 from E5M2 encoding to E4M3 encoding." in {
    test(new Convert(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.a.data.poke(75.U)
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(75.U)
      dut.roundingMode.poke(0.U)
      dut.saturationMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(86.U)
    }
  }
}
