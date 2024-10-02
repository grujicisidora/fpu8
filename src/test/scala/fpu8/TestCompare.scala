package fpu8

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestCompare extends AnyFlatSpec with ChiselScalatestTester {

  "Compare" should "compare values a and b depending of a chosen mode" in {
    test(new Compare(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.enable.poke(0.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.compareMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(0.U)

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.compareMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U) // 0_0111_110

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.compareMode.poke(1.U)
      dut.clock.step()
      dut.z.expect(0.U) // 0_0111_110

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.compareMode.poke(2.U)
      dut.clock.step()
      dut.z.expect(0.U) // 0_0111_110

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.compareMode.poke(3.U)
      dut.clock.step()
      dut.z.expect(56.U) // 0_0111_110

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.compareMode.poke(4.U)
      dut.clock.step()
      dut.z.expect(0.U) // 0_0111_110

      dut.clock.step()

      dut.enable.poke(1.U)
      dut.a.data.poke(42.U) // 0_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.compareMode.poke(5.U)
      dut.clock.step()
      dut.z.expect(56.U) // 0_0111_110

      dut.clock.step()


      dut.enable.poke(1.U)
      dut.a.data.poke(170.U) // 1_0101_010
      dut.b.data.poke(75.U) // 0_1001_011
      dut.compareMode.poke(0.U)
      dut.clock.step()
      dut.z.expect(56.U) // 1_0111_110

      dut.clock.step()
    }
  }
}
