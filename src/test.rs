use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufRead};
use std::path::Path;

use super::astbuilder::*;
use super::firrtl_ast::*;
use super::pest::Parser;

// #[ignore]
#[test]
fn firrtl_test() {
  (vec![
    "./firrtl/counter.fir",
    "./firrtl/counter.hi.fir",
    "./firrtl/counter.mid.fir",
    "./firrtl/counter.lo.fir",
    "./firrtl/syntaxerror.fir",
  ])
  .into_iter()
  .for_each(|str| {
    let mut file = File::open(str).expect("failed to open file");
    let mut code = String::new();
    file.read_to_string(&mut code).unwrap();

    let res = FIRRTLParesr::parse(Rule::firrtl_file, code.as_ref());

    if let Ok(mut pairs) = res {
      let pair_file = pairs.next().unwrap();
      let parsed_code = pair_file.as_str();
      if parsed_code.len() == code.len() {
        let parse_res = parse_file(pair_file);

        if let Ok(ast) = parse_res {
          println!("{}\n{}\n", str, ast);
        } else {
        }
      } else {
        panic!();
        //show error
      }
    } else {
      //show error line
      panic!();
    }
  });
}

#[ignore]
#[test]
fn buildinfo_test() {
  let code = r#";buildInfoPackage: chisel3, version: 3.4.1, scalaVersion: 2.12.12, sbtVersion: 1.3.10
not captured line"#;

  let pairs = FIRRTLParesr::parse(Rule::buildinfo, code).unwrap_or_else(|e| panic!("{}", e));
  for pair in pairs {
    println!("{}", pair.as_str());
  }
}

#[ignore]
#[test]
fn circuit_test() {
  let code = r#"circuit main : 
  module main : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip A : UInt<1>, flip B : UInt<1>, C : UInt<1>, flip flip : UInt<1>}
    
    node _io_C_T = and(io.A, io.B) @[main.scala 14:18]
    node _io_C_T_1 = and(_io_C_T, io.flip) @[main.scala 14:25]
    io.C <= _io_C_T_1 @[main.scala 14:10]
    
  module counter :
    input clock : Clock
    input reset : UInt<1>
    output io_out : UInt<32>
    input io_en : UInt<1>

    reg counter : UInt<32>, clock with :
      reset => (UInt<1>("h0"), counter) @[counter.scala 13:24]
    node _counter_T = add(counter, UInt<1>("h1")) @[counter.scala 16:24]
    node _counter_T_1 = tail(_counter_T, 1) @[counter.scala 16:24]
    node _GEN_0 = mux(io_en, _counter_T_1, counter) @[counter.scala 15:15 counter.scala 16:13 counter.scala 18:13]
    io_out <= counter @[counter.scala 21:10]
    counter <= mux(reset, UInt<1>("h0"), _GEN_0) @[counter.scala 13:24 counter.scala 13:24]
  "#;

  let pairs = FIRRTLParesr::parse(Rule::circuit, code).unwrap_or_else(|e| panic!("{}", e));
  for pair in pairs {
    println!("{}", parse_circuit(pair).unwrap());
  }
}

#[ignore]
#[test]
fn module_children_test() {
  let code = r#"  module main : @[hogehoge]
  module main1 : @[piyo]
    input clock : Clock
    input reset : UInt<1>
"#;

  let code2 = r#"  module main : @[code2]
    input clock : Clock
    input reset : UInt<1>
    output io : {flip A : UInt<1>, flip B : UInt<1>, C : UInt<1>, flip flip : UInt<1>}
    node _io_C_T = and(io.A, io.B) @[main.scala 14:18]
    node _io_C_T_1 = and(_io_C_T, io.flip) @[main.scala 14:25]
    io.C <= _io_C_T_1 @[main.scala 14:10]
  module main1 : @[piyo]
    input clock : Clock
    input reset : UInt<1>
"#;

  (vec![&code, &code2]).into_iter().for_each(|str| {
    let pairs = FIRRTLParesr::parse(Rule::module_children, str).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
      // println!("{:?}", parse_modulechildren(pair));
      for m in parse_modulechildren(pair).unwrap() {
        println!("{}", m);
      }
    }
  });
}

#[ignore]
#[test]
fn module_test() {
  let code = r#"module main : @[hogehoge]
    input clock : Clock
    input reset : UInt<1>
    output io : {flip A : UInt<1>, flip B : UInt<1>, C : UInt<1>, flip flip : UInt<1>}

    node _io_C_T = and(io.A, io.B) @[main.scala 14:18]
    node _io_C_T_1 = and(_io_C_T, io.flip) @[main.scala 14:25]
    io.C <= _io_C_T_1 @[main.scala 14:10]

    when io.en :
      skip
    else :
      skip
    skip
  "#;

  let pairs = FIRRTLParesr::parse(Rule::module, code).unwrap_or_else(|e| panic!("{}", e));
  for pair in pairs {
    println!("{:?}", parse_module(pair));
    // println!("{:?}", pair);
    // println!("{}", pair.as_str());
  }
}

#[ignore]
#[test]
fn port_test() {
  let _: Vec<_> = (vec![
    "input clock : Clock",
    "input reset : UInt<1>",
    "output io : {flip A : UInt<1>, flip   flip    :    UInt<1>   ,    C : UInt<1>}",
    "input flip : Analog",
    "input clock : ",
  ])
  .into_iter()
  .map(|str| {
    let res = FIRRTLParesr::parse(Rule::port, str);
    if let Ok(pairs) = res {
      for pair in pairs {
        let res = parse_port(pair);

        if let Ok(ast) = res {
          println!("{}", ast);
        } else {
          println!("{:?}", res);
        }
      }
    } else {
      println!("{:?}", res);
    }
  })
  .collect();
}

#[ignore]
#[test]
fn field_test() {
  (vec![
    "A : UInt<32>",
    "B : UInt",
    "flip C : SInt",
    "flip : Reset",
    "flip flip : Analog",
  ])
  .into_iter()
  .for_each(|str| {
    let pairs = FIRRTLParesr::parse(Rule::field, str).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
      println!("{:?}", parse_field(pair));
    }
  });
}

#[ignore]
#[test]
fn fieldid_test() {
  (vec!["_A_", "_Aw423$$fae", "15", "Analog", "flip"])
    .into_iter()
    .for_each(|str| {
      let pairs = FIRRTLParesr::parse(Rule::fieldid, &str).unwrap_or_else(|e| panic!("{}", e));
      for pair in pairs {
        println!("{:?}", parse_fieldid(pair));
      }
    });
}

#[ignore]
#[test]
pub fn wtype_test() {
  (vec![
    // "UInt<32>",
    // "UInt",
    // "SInt<8>",
    // "SInt",
    // "Analog",
    // "Analog<10>",
    // "Clock",
    // "Reset",
    // "AsyncReset",
    "{A :   UInt<aaa>}",
    // "{A : UInt<1>  ,    flip    B :    UInt<1>}",
  ])
  .into_iter()
  .for_each(|str| {
    let res = FIRRTLParesr::parse(Rule::wtype, str);
    if let Ok(pairs) = res {
      for pair in pairs {
        let res = parse_wtype(pair);

        if let Ok(ast) = res {
          println!("{}", ast);
        } else {
          println!("{:?}", res);
        }
      }
    } else {
      println!("{:?}", res);
    }
  });
}

#[ignore]
#[test]
fn num_test() {
  let res: Vec<_> = (vec![
    "0",
    "123",
    "+12312",
    "-100",
    "\"h64\"",
    "\"h-64\"",
    "\"o+144\"",
    "\"o-144\"",
    "\"b01100100\"",
    "\"b-01100100\"",
    "\"h7FFFFFFFFFFFFFFF\"", //i64 max
  ])
  .into_iter()
  .map(|str| {
    // let pairs = FIRRTLParesr::parse(Rule::intlit, str).unwrap_or_else(|e| panic!("{}", e));
    let res = FIRRTLParesr::parse(Rule::intlit, str);
    if let Ok(pairs) = res {
      for pair in pairs {
        println!("{}", parse_intlit(pair).unwrap());
      }
    } else {
      println!("{:?}", res);
    }

    ()
  })
  .collect();
}

#[ignore]
#[test]
fn id_test() {
  (vec!["abc", "_abc", "_$1a$2B$3c"])
    .into_iter()
    .for_each(|str| {
      let pairs = FIRRTLParesr::parse(Rule::id, str).unwrap_or_else(|e| panic!("{}", e));
      for pair in pairs {
        println!("{:?}", parse_ident(pair));
      }
    });
}

#[ignore]
#[test]
fn relaxedid_test() {
  (vec!["abc", "_abc", "_$1a$2B$3c"])
    .into_iter()
    .for_each(|str| {
      let pairs = FIRRTLParesr::parse(Rule::relaxedid, str).unwrap_or_else(|e| panic!("{}", e));
      for pair in pairs {
        println!("{:?}", parse_ident(pair));
      }
    });
}

#[ignore]
#[test]
fn fileinfo_test() {
  let pairs = FIRRTLParesr::parse(
    Rule::info,
    r#"@[hogehoge]
      hoge"#,
  )
  .unwrap_or_else(|e| panic!("{}", e));
  for pair in pairs {
    println!("{:?}", pair);
  }
}

#[ignore]
#[test]
fn exp_first_test() {
  (vec!["UInt(32)", "SInt<32>(1)", "hogehoge"])
    .into_iter()
    .for_each(|str| {
      let pairs = FIRRTLParesr::parse(Rule::exp_first, str).unwrap_or_else(|e| panic!("{}", e));
      for pair in pairs {
        println!("{:?}", pair);
      }
    });
}

#[ignore]
#[test]
fn exp_test() {
  (vec![
    "_io_C_T",
    "UInt<32>(100)",
    "SInt(123)",
    "mux(io.A, mux, add(1, 2))",
    "and(add(1, 2), and(1, 0), 3, 4)",
    "hoge[1]",
    "hoge[or(10, 100)]",
    "validif(or(a, 19), aaa)",
  ])
  .into_iter()
  .for_each(|str| {
    let pairs = FIRRTLParesr::parse(Rule::exp, str).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
      println!("{}", parse_exp(pair));
    }
  });
}

#[ignore]
#[test]
fn whenstmt_test() {
  let code = r#"when io.en : @[wheninfo]
  skip @[hogehoge]
else : @[elseinfo]
  skip @[hogehoge]
  skip @[hogehoge]
skip @[hogehoge]
"#;

  let code2 = r#"when io.en : @[counter.scala 15:15]
  node _counter_T = add(counter, UInt<1>("h01")) @[counter.scala 16:24]
  node _counter_T_1 = tail(_counter_T, 1) @[counter.scala 16:24]
  counter <= _counter_T_1 @[counter.scala 16:13]
  skip @[counter.scala 15:15]
else : @[counter.scala 17:15]
  counter <= counter @[counter.scala 18:13]
  skip @[counter.scala 17:15]
io.out <= counter @[counter.scala 21:10]
"#;
  //    io.out <= counter @[counter.scala 21:10] should not be included

  let code3 = r#"when io.en : @[wheninfo]
  counter <= counter @[counter.scala 18:13]
else when io.en2 : @[elsewheninfo]
  skip @[elsewhenstmt]
  skip @[elsewhenstmt]
else : @[elseinfo]
  skip @[elsestmt]
skip @[normalstmt]
"#;

  let code4 = r#"when io.en : @[counter.scala 19:15]
  node _counter1_T = add(counter1, UInt<1>("h01")) @[counter.scala 20:26]
  node _counter1_T_1 = tail(_counter1_T, 1) @[counter.scala 20:26]
  counter1 <= _counter1_T_1 @[counter.scala 20:14]
  when io.en2 : @[counter.scala 21:18]
    node _counter2_T = add(counter2, UInt<1>("h01")) @[counter.scala 22:28]
    node _counter2_T_1 = tail(_counter2_T, 1) @[counter.scala 22:28]
    counter2 <= _counter2_T_1 @[counter.scala 22:16]
    skip @[counter.scala 21:18]
  else : @[counter.scala 23:17]
    node _counter2_T_2 = sub(counter2, UInt<1>("h01")) @[counter.scala 24:28]
    node _counter2_T_3 = tail(_counter2_T_2, 1) @[counter.scala 24:28]
    counter2 <= _counter2_T_3 @[counter.scala 24:16]
    skip @[counter.scala 23:17]
  skip @[counter.scala 19:15]
else : @[counter.scala 26:15]
  node _counter1_T_2 = sub(counter1, UInt<1>("h01")) @[counter.scala 27:26]
  node _counter1_T_3 = tail(_counter1_T_2, 1) @[counter.scala 27:26]
  counter1 <= _counter1_T_3 @[counter.scala 27:14]
  when io.en2 : @[counter.scala 28:18]
    node _counter3_T = sub(counter3, UInt<1>("h01")) @[counter.scala 29:28]
    node _counter3_T_1 = tail(_counter3_T, 1) @[counter.scala 29:28]
    counter3 <= _counter3_T_1 @[counter.scala 29:16]
    skip @[counter.scala 28:18]
  else : @[counter.scala 30:17]
    node _counter3_T_2 = sub(counter3, UInt<1>("h01")) @[counter.scala 31:28]
    node _counter3_T_3 = tail(_counter3_T_2, 1) @[counter.scala 31:28]
    counter3 <= _counter3_T_3 @[counter.scala 31:16]
    skip @[counter.scala 30:17]
  skip @[counter.scala 26:15]
io.out <= counter1 @[counter.scala 35:10]
"#;

  (vec![&code, &code2, &code3, &code4])
    .into_iter()
    .for_each(|str| {
      let pairs = FIRRTLParesr::parse(Rule::whenstmt, str).unwrap_or_else(|e| panic!("{}", e));
      for pair in pairs {
        let res = parse_whenstmt(pair);

        if let Ok(ast) = res {
          println!("{}", ast);
        } else {
          println!("{:?}", res);
        }
      }
    });
}

#[ignore]
#[test]
fn stmt_test() {
  let code2 = r#"when io.en : @[counter.scala 15:15]
  node _counter_T = add(counter, UInt<1>("h01")) @[counter.scala 16:24]
  node _counter_T_1 = tail(_counter_T, 1) @[counter.scala 16:24]
  counter <= _counter_T_1 @[counter.scala 16:13]
  skip @[counter.scala 15:15]
else when io.en2 :
  counter <= counter   
  skip   
else : @[counter.scala 17:15]
  counter <= counter @[counter.scala 18:13]
  skip @[counter.scala 17:15]
io.out <= counter @[counter.scala 21:10]
"#;

  (vec![
    "wire hoge : UInt<32> @[puipui]",
    "node  _io_C_T = mux(io.A, mux, add(UInt<32>(100), 1)) @[hogehoge]",
    "skip @[hogehoge]",
    "counter <= _counter_T_1",
    "counter <- _counter_T_1",
    "reg counter : UInt<32>, clock @[piyo]",
    "reg counter : UInt<32>, clock    with : (reset => (reset, UInt<1>(\"h00\"))) @[counter.scala 13:24]",
    "reg counter : UInt<32>, clock with :\n\treset => (UInt<1>(\"hF\"), counter) @[counter.scala 13:24]",
    &code2,
  ])
  .into_iter()
  .for_each(|str| {
    let pairs = FIRRTLParesr::parse(Rule::stmt, str).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
        let res = parse_stmt(pair);

        if let Ok(ast) = res {
          println!("{}", ast);
        } else {
          println!("{:?}", res);
        }
      // println!("{:?}", pair);
    }
  });
}

#[ignore]
#[test]
fn simple_reset_test() {
  (vec![
    "reset => (UInt<32>(\"h0\") , counter)",
    "(reset => (reset , UInt<1>(\"h00\")))",
  ])
  .into_iter()
  .for_each(|str| {
    let pairs = FIRRTLParesr::parse(Rule::simple_reset, str).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
      println!("{:?}", parse_simple_reset(pair));
    }
  });
}

#[ignore]
#[test]
fn reset_block_test() {
  (vec![
    "reset => (UInt<32>(\"h0\") , counter)   @[hogehoge]",
    "(reset => (reset , UInt<1>(\"h00\")))",
  ])
  .into_iter()
  .for_each(|str| {
    let pairs = FIRRTLParesr::parse(Rule::reset_block, str).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
      println!("{:?}", parse_reset_block(pair));
    }
  });
}
