package fpu8

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestFunctionTester extends AnyFlatSpec with ChiselScalatestTester {
  "Function tester" should "round the given number to 4 digits" in {
    test(new FunctionTester(8, 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.a.poke(153.U)
      dut.clock.step()
      dut.z.expect(9.U)

      dut.clock.step()

      dut.a.poke(155.U)
      dut.clock.step()
      dut.z.expect(10.U)

      dut.clock.step()

      dut.a.poke(27.U)
      dut.clock.step()
      dut.z.expect(2.U)

      dut.clock.step()
    }
  }

  it should "also do the same thing as explained above" in {
    test(new FunctionTester(8, 6)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.a.poke(153.U)
      dut.clock.step()
      dut.z.expect(38.U)

      dut.clock.step()

      dut.a.poke(26.U)
      dut.clock.step()
      dut.z.expect(6.U)

      dut.clock.step()
    }
  }

}
