circuit counter :
  module counter :
    input clock : Clock
    input reset : UInt<1>
    output io : { out : UInt<32>, flip opcode : UInt<2>}

    reg counter1 : UInt<32>, clock with :
      reset => (reset, UInt<1>("h0")) @[counter.scala 13:25]
    node _T = eq(io.opcode, UInt<1>("h0")) @[counter.scala 15:18]
    node _counter1_T = add(counter1, UInt<1>("h1")) @[counter.scala 16:26]
    node _counter1_T_1 = tail(_counter1_T, 1) @[counter.scala 16:26]
    node _T_1 = eq(io.opcode, UInt<1>("h1")) @[counter.scala 17:24]
    node _counter1_T_2 = sub(counter1, UInt<1>("h1")) @[counter.scala 18:26]
    node _counter1_T_3 = tail(_counter1_T_2, 1) @[counter.scala 18:26]
    node _T_2 = eq(io.opcode, UInt<2>("h2")) @[counter.scala 19:24]
    node _GEN_0 = mux(_T_2, counter1, counter1) @[counter.scala 19:33 counter.scala 20:14 counter.scala 22:14]
    node _GEN_1 = mux(_T_1, _counter1_T_3, _GEN_0) @[counter.scala 17:33 counter.scala 18:14]
    node _GEN_2 = mux(_T, _counter1_T_1, _GEN_1) @[counter.scala 15:27 counter.scala 16:14]
    io.out <= counter1 @[counter.scala 25:10]
    counter1 <= _GEN_2