FIRRTL version 1.2.0
circuit FunctionTester :
  module FunctionTester :
    input clock : Clock
    input reset : UInt<1>
    input a : UInt<8> @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 7:13]
    output z : UInt<6> @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 11:13]

    node _roundedValue_roundedValue_T = bits(a, 7, 2) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 27:23]
    node roundedValue = cat(UInt<1>("h0"), _roundedValue_roundedValue_T) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 27:12]
    z <= bits(roundedValue, 5, 0) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 16:5]
