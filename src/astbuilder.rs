use std::i64;
use std::u64;

use super::error;
use super::firrtl_ast::*;
use super::pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct FIRRTLParesr;

pub fn parse_file(pair: pest::iterators::Pair<Rule>) -> Result<FIRRTLFile, error::Error> {
  let mut inner = pair.into_inner();
  let first_pair = inner.next().unwrap();

  let (info, pair_circuit) = match first_pair.as_rule() {
    Rule::buildinfo => {
      let info = first_pair.as_str().to_string();
      (info, inner.next().unwrap())
    }
    Rule::circuit => ("".to_string(), first_pair),
    _ => return Err(error::Error::Unexpected(first_pair.as_rule())),
  };

  let circuit = parse_circuit(pair_circuit)?;

  Ok(FIRRTLFile { info, circuit })
}

pub fn parse_circuit(pair: pest::iterators::Pair<Rule>) -> Result<Circuit, error::Error> {
  // println!("{:?}", pair);
  let mut inner = pair.into_inner();
  let id = parse_ident(inner.next().unwrap()).unwrap();
  let second_pair = inner.next().unwrap();

  let (info, pair_modules) = if let Some(pair_third) = inner.next() {
    let info = parse_info(second_pair);

    (info, pair_third)
  } else {
    ("".to_string(), second_pair)
  };

  let modules = parse_modulechildren(pair_modules)?;

  Ok(Circuit {
    id: id,
    modules,
    info,
  })
}

pub fn parse_modulechildren(
  pair: pest::iterators::Pair<Rule>,
) -> Result<Vec<Module>, error::Error> {
  // println!("{:?}", pair);
  let mut modules = vec![];
  let mut inner = pair.into_inner();
  while let Some(pair) = inner.next() {
    match pair.as_rule() {
      Rule::module => {
        let module = parse_module(pair)?;
        modules.push(module);
      }
      _ => {
        // panic!("Unexpected rule {:?}", pair.as_rule());
      }
    }
    // println!("{:?}", pair.as_rule());
  }
  Ok(modules)
}

pub fn parse_port_childre(pair: pest::iterators::Pair<Rule>) -> Result<Vec<Port>, error::Error> {
  let mut ports = vec![];
  for p in pair.into_inner() {
    ports.push(parse_port(p).unwrap());
  }

  Ok(ports)
}

pub fn parse_stmt_children_in_module(
  pair: pest::iterators::Pair<Rule>,
) -> Result<Vec<Stmt>, error::Error> {
  let mut stmts = vec![];
  for s in pair.into_inner() {
    let res = parse_stmt(s)?;
    stmts.push(res);
  }
  Ok(stmts)
}

pub fn parse_module(pair: pest::iterators::Pair<Rule>) -> Result<Module, error::Error> {
  // println!("{:?}", pair);
  let mut inner = pair.into_inner();
  let id = parse_ident(inner.next().unwrap()).unwrap(); //should have

  let mut info = "".to_string();
  let mut ports: Vec<Port> = vec![];
  let mut body: Vec<Stmt> = vec![];

  for p in inner {
    // println!("{:?}", p.as_rule());
    match p.as_rule() {
      Rule::info => {
        info = parse_info(p);
      }
      Rule::port_children => {
        ports = parse_port_childre(p).unwrap();
      }
      Rule::stmt_children_in_module => {
        body = parse_stmt_children_in_module(p)?;
      }
      _ => {
        // panic!("unknown direction {}", p.as_str());
        return Err(error::Error::Unexpected(p.as_rule()));
      }
    }
  }

  Ok(Module {
    info,
    id,
    ports,
    body,
  })
}

pub fn parse_dir(pair: pest::iterators::Pair<Rule>) -> Result<Direction, error::Error> {
  match pair.as_str() {
    "input" => Ok(Direction::Input),
    "output" => Ok(Direction::Output),
    _ => {
      // panic!("unknown direction {}", pair.as_str());
      // Err(error::Error::Unexpected(pair.as_rule()))
      unreachable!()
    }
  }
}

pub fn parse_port(pair: pest::iterators::Pair<Rule>) -> Result<Port, error::Error> {
  // println!("{:?}", pair);
  let mut inner = pair.into_inner();

  let dir = parse_dir(inner.next().unwrap());
  let name = parse_ident(inner.next().unwrap()).unwrap();
  let tpe = parse_wtype(inner.next().unwrap())?;

  Ok(Port {
    info: "".to_string(),
    name,
    direction: dir.unwrap(),
    tpe: tpe,
  })
}

pub fn parse_field(pair: pest::iterators::Pair<Rule>) -> Result<Field, error::Error> {
  // println!("{:?}", pair);
  let mut inner = pair.into_inner();
  let first_pair = inner.next().unwrap();
  let second_pair = inner.next().unwrap();

  let (isflip, pair_fieldid, pair_type) = match first_pair.as_rule() {
    Rule::isflip => (true, second_pair, inner.next().unwrap()),
    _ => (false, first_pair, second_pair),
  };

  let tpe = parse_wtype(pair_type)?;

  Ok(Field {
    isflip: isflip,
    id: parse_fieldid(pair_fieldid),
    tpe,
  })
}

pub fn parse_fieldid(pair: pest::iterators::Pair<Rule>) -> Ident {
  // println!("{:?}", pair);
  pair.as_str().to_string()
}

pub fn parse_wtype(pair: pest::iterators::Pair<Rule>) -> Result<Type, error::Error> {
  let mut inner = pair.into_inner();
  // println!("{:?}", inner);
  let first_pair = inner.next().unwrap();

  match first_pair.as_rule() {
    Rule::uint => {
      let w = if let Some(second_pair) = inner.next() {
        Width::IntWidth(parse_intlit(second_pair).unwrap())
      } else {
        Width::UnknownWidth
      };
      return Ok(Type::UInt(w));
    }
    Rule::sint => {
      let w = if let Some(second_pair) = inner.next() {
        Width::IntWidth(parse_intlit(second_pair).unwrap())
      } else {
        Width::UnknownWidth
      };
      return Ok(Type::SInt(w));
    }
    Rule::analog => {
      let w = if let Some(second_pair) = inner.next() {
        Width::IntWidth(parse_intlit(second_pair).unwrap())
      } else {
        Width::UnknownWidth
      };
      return Ok(Type::Analog(w));
    }
    Rule::clock => {
      return Ok(Type::Clock);
    }
    Rule::asyncreset => {
      return Ok(Type::AsyncReset);
    }
    Rule::reset => {
      return Ok(Type::Reset);
    }
    Rule::field => {
      let res = parse_field(first_pair)?;

      let mut fv = vec![res];
      while let Some(pair_field) = inner.next() {
        let res = parse_field(pair_field)?;

        fv.push(res);
      }
      return Ok(Type::Bundle(fv));
    }
    //Fixed
    //interval
    //Vector
    _ => return Err(error::Error::Unexpected(first_pair.as_rule())),
  }
}

pub fn parse_intlit(pair: pest::iterators::Pair<Rule>) -> Result<IntLit, error::Error> {
  let mut inner = pair.into_inner();
  if let Some(num_str) = inner.next() {
    match num_str.as_rule() {
      Rule::unsignedint => {
        // println!("{:?}", num.as_str());
        Ok(IntLit::UnsignedInt(
          num_str.as_str().parse::<u64>().unwrap(),
        ))
      }
      Rule::signedint => Ok(IntLit::SignedInt(
        i64::from_str_radix(num_str.as_str(), 10).unwrap(),
      )),
      Rule::hexlit => {
        let str = num_str.into_inner().concat();
        Ok(IntLit::HexLit(i64::from_str_radix(&str, 16).unwrap())) //Todo overflow number
      }
      Rule::octallit => {
        let str = num_str.into_inner().concat();
        Ok(IntLit::OctalLit(i64::from_str_radix(&str, 8).unwrap()))
      }
      Rule::binarylit => {
        let str = num_str.into_inner().concat();
        Ok(IntLit::BinaryLit(i64::from_str_radix(&str, 2).unwrap()))
      }
      _ => {
        Err(error::Error::Unexpected(num_str.as_rule()))
        // Err(error::Error::Type)
      }
    }
  } else {
    // panic!("Unexpected rule : {:?}\n", inner);
    // If reaching here, program bug.
    Err(error::Error::NoInner)
  }
}

pub fn parse_ident(pair: pest::iterators::Pair<Rule>) -> Result<String, error::Error> {
  //should be branch
  match pair.as_rule() {
    Rule::id | Rule::relaxedid | Rule::idandkeywordid => Ok(pair.as_str().to_string()),
    _ => {
      // panic!("Unexpected rule : {:?}", pair.as_rule())
      Err(error::Error::Unexpected(pair.as_rule()))
    }
  }
}

pub fn parse_info(pair: pest::iterators::Pair<Rule>) -> String {
  pair.as_str().to_string()
}

pub fn print_exp_rec(ppair: pest::iterators::Pair<Rule>, indent: usize) {
  println!(
    "{sp:width$}{rule:?} : {str}",
    sp = "",
    width = indent * 2,
    rule = ppair.as_rule(),
    str = ppair.as_str(),
  );
  let mut inner = ppair.into_inner();
  while let Some(pair) = inner.next() {
    print_exp_rec(pair, indent + 1);
    // println!(
    //   "{sp:>width$}{rule:?} : {str}",
    //   sp = " ",
    //   width = (indent + 1) * 2,
    //   rule = pair.as_rule(),
    //   str = pair.as_str(),
    // );
  }
}

pub fn parse_exp(ppair: pest::iterators::Pair<Rule>) -> Expr {
  let mut inner = ppair.into_inner();
  let pair_head = inner.next().unwrap();

  let mut tmp_exp = match pair_head.as_rule() {
    Rule::uint => {
      //one or two intlit
      let first_intlit = parse_intlit(inner.next().unwrap()).unwrap();
      let exp = if let Some(second) = inner.next() {
        let second_intlit = parse_intlit(second).unwrap();
        Expr::Const(Type::UInt(Width::IntWidth(first_intlit)), second_intlit)
      } else {
        Expr::Const(Type::UInt(Width::UnknownWidth), first_intlit)
      };
      exp
    }
    Rule::sint => {
      //one or two intlit
      let first_intlit = parse_intlit(inner.next().unwrap()).unwrap();
      let exp = if let Some(second) = inner.next() {
        let second_intlit = parse_intlit(second).unwrap();
        Expr::Const(Type::SInt(Width::IntWidth(first_intlit)), second_intlit)
      } else {
        Expr::Const(Type::SInt(Width::UnknownWidth), first_intlit)
      };
      exp
    }
    Rule::idandkeywordid => Expr::Reference(pair_head.as_str().to_string(), Type::UnknownType),
    Rule::mux => {
      let mux_s = parse_exp(inner.next().unwrap());
      let mux_a = parse_exp(inner.next().unwrap());
      let mux_b = parse_exp(inner.next().unwrap());
      Expr::Mux(Box::new(mux_s), Box::new(mux_a), Box::new(mux_b))
    }
    Rule::validif => {
      let a = parse_exp(inner.next().unwrap());
      let b = parse_exp(inner.next().unwrap());
      Expr::ValidIf(Box::new(a), Box::new(b))
    }
    Rule::primop => {
      let op = find_primop(pair_head.as_str());
      let mut expargs: Vec<Expr> = vec![];
      let mut intlitarg: Vec<IntLit> = vec![];
      while let Some(arg) = inner.next() {
        match arg.as_rule() {
          Rule::exp => {
            let exp = parse_exp(arg);
            expargs.push(exp);
          }
          Rule::intlit => {
            let intlit = parse_intlit(arg).unwrap();
            intlitarg.push(intlit);
          }
          _ => {
            panic!("{:?} cannot be arg of primop", arg.as_rule());
          }
        }
      }

      Expr::DoPrim(op, expargs, intlitarg, Type::UnknownType)
    }
    _ => {
      while let Some(pair) = inner.next() {
        print_exp_rec(pair, 0);
      }
      // Expr::Const(Type::UnknownType, IntLit::UnsignedInt(0))
      panic!("this rule cannot be head of exp {:?}", pair_head.as_rule());
    }
  };

  while let Some(exp_continue) = inner.next() {
    match exp_continue.as_rule() {
      Rule::op_dot => {
        let fieldid = parse_fieldid(inner.next().unwrap());
        tmp_exp = Expr::SubField(Box::new(tmp_exp), fieldid, Type::UnknownType);
      }
      Rule::intlit => {
        let intlit = parse_intlit(exp_continue).unwrap();
        tmp_exp = Expr::SubIndex(Box::new(tmp_exp), intlit, Type::UnknownType);
      }
      Rule::exp => {
        let exp = parse_exp(exp_continue);
        tmp_exp = Expr::SubAccess(Box::new(tmp_exp), Box::new(exp), Type::UnknownType);
      }
      _ => {
        while let Some(pair) = inner.next() {
          print_exp_rec(pair, 0);
        }
        panic!("Unexpected rule {:?}", exp_continue.as_rule());
        // Expr::Const(Type::UnknownType, IntLit::UnsignedInt(0))
      }
    }
  }

  tmp_exp
}

pub fn find_primop(primop: &str) -> PrimOp {
  match primop {
    "add(" => PrimOp::Add,
    "sub(" => PrimOp::Sub,
    "mul(" => PrimOp::Mul,
    "div(" => PrimOp::Div,
    "rem(" => PrimOp::Rem,
    "lt(" => PrimOp::Lt,
    "leq(" => PrimOp::Leq,
    "gt(" => PrimOp::Gt,
    "geq(" => PrimOp::Geq,
    "eq(" => PrimOp::Eq,
    "neq(" => PrimOp::Neq,
    "pad(" => PrimOp::Pad,
    "asUInt(" => PrimOp::AsUInt,
    "asAsyncReset(" => PrimOp::AsAsyncReset,
    "asSInt(" => PrimOp::AsSInt,
    "asClock(" => PrimOp::AsClock,
    "asFixedPoint(" => PrimOp::AsFixedPoint,
    "asInterval(" => PrimOp::AsInterval,
    "shl(" => PrimOp::Shl,
    "shr(" => PrimOp::Shr,
    "dshl(" => PrimOp::Dshl,
    "dshr(" => PrimOp::Dshr,
    "cvt(" => PrimOp::Cvt,
    "neg(" => PrimOp::Neg,
    "not(" => PrimOp::Not,
    "and(" => PrimOp::And,
    "or(" => PrimOp::Or,
    "xor(" => PrimOp::Xor,
    "andr(" => PrimOp::Andr,
    "orr(" => PrimOp::Orr,
    "xorr(" => PrimOp::Xorr,
    "cat(" => PrimOp::Cat,
    "bits(" => PrimOp::Bits,
    "head(" => PrimOp::Head,
    "tail(" => PrimOp::Tail,
    "incp(" => PrimOp::Incp,
    "decp(" => PrimOp::Decp,
    "setp(" => PrimOp::Setp,
    "wrap(" => PrimOp::Wrap,
    "clip(" => PrimOp::Clip,
    "squz(" => PrimOp::Squz,
    _ => PrimOp::Unknown,
  }
}

fn parse_stmt_children(pair: pest::iterators::Pair<Rule>) -> Result<Vec<Stmt>, error::Error> {
  // println!("{:?}", pair);
  let mut inner = pair.into_inner();
  let mut stmts = vec![];
  while let Some(t) = inner.next() {
    let res = parse_stmt(t)?;
    stmts.push(res);
  }
  Ok(stmts)
}

fn parse_whenelseblock(
  pair: pest::iterators::Pair<Rule>,
) -> Result<(Option<Expr>, Info, Vec<Stmt>), error::Error> {
  match pair.as_rule() {
    Rule::whenblock => {
      let mut inner = pair.into_inner();
      // println!("{:?}", inner);
      let mut info = "".to_string();
      let cond = parse_exp(inner.next().unwrap()); //should have
      let mut stmt = vec![];

      while let Some(t) = inner.next() {
        match t.as_rule() {
          Rule::info => {
            info = parse_info(t);
          }
          Rule::stmt_children_in_whenelse => {
            stmt = parse_stmt_children(t)?;
          }
          _ => {
            panic!("Unexpected Rule in when-else {:?}", t.as_rule());
          }
        }
      }

      return Ok((Some(cond), info, stmt));
    }
    Rule::elseblock => {
      let mut inner = pair.into_inner();
      let mut info = "".to_string();
      let mut stmt = vec![];

      while let Some(t) = inner.next() {
        match t.as_rule() {
          Rule::info => {
            info = parse_info(t);
          }
          Rule::stmt_children_in_whenelse => {
            let mut res = parse_stmt_children(t)?;
            stmt.append(&mut res);
          }
          Rule::whenstmt => {
            //else when ~
            let res = parse_whenstmt(t)?;
            stmt.push(res);
            // panic!();
          }
          _ => {
            panic!("Unexpected Rule in when-else {:?}", t.as_rule());
          }
        }
      }

      return Ok((None, info, stmt));
    }
    _ => Err(error::Error::Unexpected(pair.as_rule())),
  }
}

pub fn parse_whenstmt(pair: pest::iterators::Pair<Rule>) -> Result<Stmt, error::Error> {
  // println!("{:?}", pair);
  let mut inner = pair.into_inner();
  let whenblock = inner.next().unwrap(); //should be when
  let (cond, wheninfo, whenstmt) = parse_whenelseblock(whenblock)?;

  let mut elseinfo = "".to_string();
  let mut elsestmt = vec![];

  if let Some(elseblock) = inner.next() {
    let (_, info, mut stmts) = parse_whenelseblock(elseblock)?;
    elseinfo = info;
    elsestmt.append(&mut stmts);
  }

  Ok(Stmt::When {
    cond: cond.unwrap(),
    wheninfo,
    whenstmt,
    elseinfo,
    elsestmt,
  })
}

pub fn parse_stmt(pair: pest::iterators::Pair<Rule>) -> Result<Stmt, error::Error> {
  // println!("{:?}", pair);

  let mut inner = pair.into_inner();

  if let Some(first) = inner.next() {
    match first.as_rule() {
      Rule::node => {
        let id = parse_ident(inner.next().unwrap())?;
        let expr = parse_exp(inner.next().unwrap());
        let info = if let Some(infop) = inner.next() {
          parse_info(infop)
        } else {
          "".to_string()
        };

        return Ok(Stmt::Node { info, id, expr });
      }
      Rule::wire => {
        let id = parse_ident(inner.next().unwrap()).unwrap();
        let tpe = parse_wtype(inner.next().unwrap())?;
        let info = if let Some(infop) = inner.next() {
          parse_info(infop)
        } else {
          "".to_string()
        };

        return Ok(Stmt::DefWire { info, id, tpe });
      }
      Rule::skip => {
        let info = if let Some(infop) = inner.next() {
          parse_info(infop)
        } else {
          "".to_string()
        };
        return Ok(Stmt::Skip { info });
      }
      Rule::reg => {
        let id = parse_ident(inner.next().unwrap()).unwrap();
        let tpe = parse_wtype(inner.next().unwrap())?;
        let clock = parse_exp(inner.next().unwrap());
        let mut info = "".to_string();
        let mut reset = None;
        let mut init = None;
        if let Some(last) = inner.next() {
          //resetblock or info
          match last.as_rule() {
            Rule::info => {
              info = parse_info(last);
            }
            Rule::reset_block => {
              let (treset, tinit, tinfo) = parse_reset_block(last);
              reset = Some(treset);
              init = Some(tinit);
              info = tinfo;
            }
            _ => {
              panic!("{:?}", last.as_rule());
            }
          }
        }

        return Ok(Stmt::DefRegister {
          info,
          id,
          tpe,
          clock,
          reset,
          init,
        });
      }
      Rule::whenstmt => {
        let res = parse_whenstmt(first)?;
        return Ok(res);
      }
      Rule::exp => {
        //cononect or partialconnect or is invalid
        let pair_next = inner.next().unwrap();
        match pair_next.as_rule() {
          Rule::connect => {
            let lhs = parse_exp(first);
            let rhs = parse_exp(inner.next().unwrap());
            let info = if let Some(infop) = inner.next() {
              parse_info(infop)
            } else {
              "".to_string()
            };
            return Ok(Stmt::Connect { info, lhs, rhs });
          }
          Rule::partialconnect => {
            let lhs = parse_exp(first);
            let rhs = parse_exp(inner.next().unwrap());
            let info = if let Some(infop) = inner.next() {
              parse_info(infop)
            } else {
              "".to_string()
            };
            return Ok(Stmt::PartialConnect { info, lhs, rhs });
          }
          Rule::isinvalid => {}
          _ => {
            panic!("Unexpected Rule after exp {:?}", pair_next.as_rule());
          }
        }
      }
      _ => {
        println!("{:?}", first.as_rule());
      }
    }
  } else {
    panic!("no inner");
  }

  Ok(Stmt::Skip {
    info: "".to_string(),
  })
}

pub fn parse_simple_reset(ppair: pest::iterators::Pair<Rule>) -> (Expr, Expr) {
  // println!("{:?}", ppair);
  let mut inner = ppair.into_inner();
  let exp0 = parse_exp(inner.next().unwrap());
  let exp1 = parse_exp(inner.next().unwrap());
  (exp0, exp1)
}

pub fn parse_reset_block(ppair: pest::iterators::Pair<Rule>) -> (Expr, Expr, String) {
  // println!("{:?}", ppair);
  let mut inner = ppair.into_inner();
  let (exp0, exp1) = parse_simple_reset(inner.next().unwrap());
  let info = if let Some(t) = inner.next() {
    parse_info(t)
  } else {
    "".to_string()
  };
  (exp0, exp1, info)
}
