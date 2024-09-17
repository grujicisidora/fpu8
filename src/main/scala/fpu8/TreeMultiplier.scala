package fpu8

import chisel3._

class TreeMultiplier(lengthA: Int, lengthB: Int) extends Module {
  val a = IO(Input(UInt(lengthA.W)))
  val b = IO(Input(UInt(lengthB.W)))
  val product = IO(Output(UInt((lengthA + lengthB).W)))

  val partialProducts = Seq.tabulate(lengthB)(i => {
    val compare = Mux(b(i), ((1 << lengthA) - 1).U(lengthA.W), 0.U(lengthA.W))
    ((a & compare) << i).asUInt
  })

  def reducePartialProducts(pp: Seq[UInt]): UInt = {
    if (pp.size == 1) {
      pp.head
    } else if (pp.size == 2) {
      pp(0) +& pp(1)
    } else {
      val partialSums = pp.grouped(3).toSeq.map {
        case Seq(a, b, c) => a +& b +& c
        case Seq(a, b) => a +& b
        case Seq(a) => a
      }
      reducePartialProducts(partialSums)
    }
  }

  product := reducePartialProducts(partialProducts)
}
