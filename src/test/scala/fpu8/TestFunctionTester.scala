package fpu8

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestFunctionTester extends AnyFlatSpec with ChiselScalatestTester {
  "Function tester" should "shift the input value so the MSB becomes 1" in {
    test(new FunctionTester(8)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.a.poke(15.U)
      dut.clock.step()
      dut.z.expect(240.U)
      dut.y.expect(4.U)

      dut.clock.step()

      dut.a.poke(44.U)
      dut.clock.step()
      dut.z.expect(176.U)
      dut.y.expect(2.U)
    }
  }

}
