circuit counter :
  module counter :
    input clock : Clock
    input reset : UInt<1>
    output io_out : UInt<32>
    input io_opcode : UInt<2>

    reg counter1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), counter1) @[counter.scala 13:25]
    node _T = eq(io_opcode, UInt<2>("h0")) @[counter.scala 15:18]
    node _counter1_T = add(counter1, UInt<32>("h1")) @[counter.scala 16:26]
    node _counter1_T_1 = tail(_counter1_T, 1) @[counter.scala 16:26]
    node _T_1 = eq(io_opcode, UInt<2>("h1")) @[counter.scala 17:24]
    node _counter1_T_2 = sub(counter1, UInt<32>("h1")) @[counter.scala 18:26]
    node _counter1_T_3 = tail(_counter1_T_2, 1) @[counter.scala 18:26]
    skip
    skip
    node _GEN_1 = mux(_T_1, _counter1_T_3, counter1) @[counter.scala 17:33 counter.scala 18:14]
    node _GEN_2 = mux(_T, _counter1_T_1, _GEN_1) @[counter.scala 15:27 counter.scala 16:14]
    io_out <= counter1 @[counter.scala 25:10]
    counter1 <= mux(reset, UInt<32>("h0"), _GEN_2) @[counter.scala 13:25 counter.scala 13:25]