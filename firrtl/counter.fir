;buildInfoPackage: chisel3, version: 3.4.1, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit counter : 
  module counter : 
    input clock : Clock
    input reset : UInt<1>
    output io : {out : UInt<32>, flip opcode : UInt<2>}
    
    reg counter1 : UInt<32>, clock with : (reset => (reset, UInt<1>("h00"))) @[counter.scala 13:25]
    node _T = eq(io.opcode, UInt<1>("h00")) @[counter.scala 15:18]
    when _T : @[counter.scala 15:27]
      node _counter1_T = add(counter1, UInt<1>("h01")) @[counter.scala 16:26]
      node _counter1_T_1 = tail(_counter1_T, 1) @[counter.scala 16:26]
      counter1 <= _counter1_T_1 @[counter.scala 16:14]
      skip @[counter.scala 15:27]
    else : @[counter.scala 17:33]
      node _T_1 = eq(io.opcode, UInt<1>("h01")) @[counter.scala 17:24]
      when _T_1 : @[counter.scala 17:33]
        node _counter1_T_2 = sub(counter1, UInt<1>("h01")) @[counter.scala 18:26]
        node _counter1_T_3 = tail(_counter1_T_2, 1) @[counter.scala 18:26]
        counter1 <= _counter1_T_3 @[counter.scala 18:14]
        skip @[counter.scala 17:33]
      else : @[counter.scala 19:33]
        node _T_2 = eq(io.opcode, UInt<2>("h02")) @[counter.scala 19:24]
        when _T_2 : @[counter.scala 19:33]
          counter1 <= counter1 @[counter.scala 20:14]
          skip @[counter.scala 19:33]
        else : @[counter.scala 21:15]
          counter1 <= counter1 @[counter.scala 22:14]
          skip @[counter.scala 21:15]
    io.out <= counter1 @[counter.scala 25:10]
    
