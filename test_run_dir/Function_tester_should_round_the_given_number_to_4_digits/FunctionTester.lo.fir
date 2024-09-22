FIRRTL version 1.2.0
circuit FunctionTester :
  module FunctionTester :
    input clock : Clock
    input reset : UInt<1>
    input a : UInt<8> @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 7:13]
    output z : UInt<4> @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 11:13]

    node _roundedValue_roundedValue_T = bits(a, 7, 4) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 25:14]
    node _roundedValue_roundedValue_T_1 = bits(a, 3, 3) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 25:44]
    node _roundedValue_roundedValue_T_2 = bits(a, 2, 1) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 25:66]
    node _roundedValue_roundedValue_T_3 = orr(_roundedValue_roundedValue_T_2) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 25:95]
    node _roundedValue_roundedValue_T_4 = and(_roundedValue_roundedValue_T_1, _roundedValue_roundedValue_T_3) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 25:59]
    node roundedValue = add(_roundedValue_roundedValue_T, _roundedValue_roundedValue_T_4) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 25:35]
    z <= bits(roundedValue, 3, 0) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 16:5]
