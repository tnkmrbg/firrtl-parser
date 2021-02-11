# FIRRTL parser (WIP)
FIRRTL parser using pest in Rust language

```Rust
extern crate firrtl_parser;

use std::fs::File;
use std::io::prelude::*;

fn main() {
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

        let res = firrtl_parser::parse(code);

        match res {
            Ok(ast) => {
                println!("{}", ast)
            }
            Err(err) => {
                println!("{:?}", err)
            }
        }
    });
```

## Implemented
Parser returns Result<T, E> tentatively (not enough).
### Circuit
### Module
- Module (normal)
### Port
### Direction
### Type
- Unsigned Integer
- Signed Integer
- Clock
- Analog
- Bundle
### Field
### Statement
- Wire
- Register
- Node
- Connect
- Partial Connect
- Invalidate
- Conditional
- Empty
### Info
- File Information To ...
### Expression
- Literal Unsigned Integer
- Literal Signed Integer
- Reference
- Sub-field
- Sub-index
- Sub-access
- Multiplexer
- Conditionally Valid
- Primitive Operation
### Primop
- All

## Todo
###
- [ ] Test
(Code->AST1->Code->Ast2 then compare AST1 with AST2)

### Module
- [ ] External Module
### Type
- [ ] Fixed
- [ ] interval
- [ ] Vector
### Statement
- [ ] Memory
- [ ] Instance
- [ ] Attach
- [ ] Stop
- [ ] Printf
### RUW
- [ ] Read Under Write ???

## Unimplemeted (Todo? If needed)
### Statement
- Conditional(one line)
### Expression
- Literal Unsigned Integer From Bits
- Literal Signed Integer From Bits

## Reference
Specification for the FIRRTL LanguageVersion 0.2.0
- [Specification for the FIRRTL Language](https://raw.githubusercontent.com/chipsalliance/firrtl/master/spec/spec.pdf)
- [FIRRTL.g4](https://github.com/chipsalliance/firrtl/blob/master/src/main/antlr4/FIRRTL.g4)
- [pest](https://pest.rs/)