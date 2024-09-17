FIRRTL version 1.2.0
circuit TreeMultiplier :
  module TreeMultiplier :
    input clock : Clock
    input reset : UInt<1>
    input a : UInt<5> @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 7:13]
    input b : UInt<4> @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 8:13]
    output product : UInt<9> @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 9:19]

    node _partialProducts_compare_T = bits(b, 0, 0) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:24]
    node partialProducts_compare = mux(_partialProducts_compare_T, UInt<5>("h1f"), UInt<5>("h0")) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:22]
    node _partialProducts_T = and(a, partialProducts_compare) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 13:9]
    node partialProducts_0 = shl(_partialProducts_T, 0) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 13:20]
    node _partialProducts_compare_T_1 = bits(b, 1, 1) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:24]
    node partialProducts_compare_1 = mux(_partialProducts_compare_T_1, UInt<5>("h1f"), UInt<5>("h0")) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:22]
    node _partialProducts_T_1 = and(a, partialProducts_compare_1) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 13:9]
    node partialProducts_1 = shl(_partialProducts_T_1, 1) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 13:20]
    node _partialProducts_compare_T_2 = bits(b, 2, 2) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:24]
    node partialProducts_compare_2 = mux(_partialProducts_compare_T_2, UInt<5>("h1f"), UInt<5>("h0")) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:22]
    node _partialProducts_T_2 = and(a, partialProducts_compare_2) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 13:9]
    node partialProducts_2 = shl(_partialProducts_T_2, 2) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 13:20]
    node _partialProducts_compare_T_3 = bits(b, 3, 3) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:24]
    node partialProducts_compare_3 = mux(_partialProducts_compare_T_3, UInt<5>("h1f"), UInt<5>("h0")) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:22]
    node _partialProducts_T_3 = and(a, partialProducts_compare_3) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 13:9]
    node partialProducts_3 = shl(_partialProducts_T_3, 3) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 13:20]
    node _product_partialSums_T = add(partialProducts_0, partialProducts_1) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 23:32]
    node product_partialSums_0 = add(_product_partialSums_T, partialProducts_2) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 23:37]
    node _product_T = add(product_partialSums_0, partialProducts_3) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 20:13]
    product <= _product_T @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 31:11]
