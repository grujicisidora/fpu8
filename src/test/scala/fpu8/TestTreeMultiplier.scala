package fpu8

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestTreeMultiplier extends AnyFlatSpec with ChiselScalatestTester {
  "Multiplier" should "calculate the result of a * b" in {
    test(new TreeMultiplier(4, 3)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.a.poke(13.U)
      dut.b.poke(5.U)
      dut.clock.step()
      dut.product.expect(65.U)
    }
  }

  it should "also calculate the same expression" in {
    test(new TreeMultiplier(5, 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.a.poke(26.U)
      dut.b.poke(10.U)
      dut.clock.step()
      dut.product.expect(260.U)
    }
  }
}
