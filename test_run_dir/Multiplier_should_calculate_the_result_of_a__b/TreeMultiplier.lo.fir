FIRRTL version 1.2.0
circuit TreeMultiplier :
  module TreeMultiplier :
    input clock : Clock
    input reset : UInt<1>
    input a : UInt<4> @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 6:13]
    input b : UInt<3> @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 7:13]
    output product : UInt<7> @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 8:19]

    node _partialProducts_compare_T = bits(b, 0, 0) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 11:24]
    node partialProducts_compare = mux(_partialProducts_compare_T, UInt<4>("hf"), UInt<4>("h0")) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 11:22]
    node _partialProducts_T = and(a, partialProducts_compare) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:9]
    node partialProducts_0 = shl(_partialProducts_T, 0) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:20]
    node _partialProducts_compare_T_1 = bits(b, 1, 1) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 11:24]
    node partialProducts_compare_1 = mux(_partialProducts_compare_T_1, UInt<4>("hf"), UInt<4>("h0")) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 11:22]
    node _partialProducts_T_1 = and(a, partialProducts_compare_1) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:9]
    node partialProducts_1 = shl(_partialProducts_T_1, 1) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:20]
    node _partialProducts_compare_T_2 = bits(b, 2, 2) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 11:24]
    node partialProducts_compare_2 = mux(_partialProducts_compare_T_2, UInt<4>("hf"), UInt<4>("h0")) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 11:22]
    node _partialProducts_T_2 = and(a, partialProducts_compare_2) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:9]
    node partialProducts_2 = shl(_partialProducts_T_2, 2) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 12:20]
    node _product_partialSums_T = add(partialProducts_0, partialProducts_1) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 22:32]
    node product_partialSums_0 = add(_product_partialSums_T, partialProducts_2) @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 22:37]
    product <= product_partialSums_0 @[\\src\\main\\scala\\fpu8\\TreeMultiplier.scala 30:11]
