FIRRTL version 1.2.0
circuit FunctionTester :
  module FunctionTester :
    input clock : Clock
    input reset : UInt<1>
    input a : UInt<4> @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 7:13]
    input b : UInt<4> @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 8:13]
    input c : UInt<1> @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 9:13]
    input d : UInt<1> @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 10:13]
    output z : UInt<6> @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 11:13]

    node _tempExponent_T = cat(UInt<2>("h0"), a) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 16:25]
    node _tempExponent_T_1 = cat(UInt<2>("h0"), b) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 16:44]
    node _tempExponent_T_2 = sub(_tempExponent_T, _tempExponent_T_1) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 16:39]
    node _tempExponent_T_3 = tail(_tempExponent_T_2, 1) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 16:39]
    node _tempExponent_T_4 = add(_tempExponent_T_3, UInt<3>("h7")) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 16:58]
    node tempExponent = tail(_tempExponent_T_4, 1) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 16:58]
    node _z_T = sub(tempExponent, c) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 18:21]
    node _z_T_1 = tail(_z_T, 1) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 18:21]
    node _z_T_2 = add(_z_T_1, d) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 18:25]
    node _z_T_3 = tail(_z_T_2, 1) @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 18:25]
    z <= _z_T_3 @[\\src\\main\\scala\\fpu8\\FunctionTester.scala 18:5]
