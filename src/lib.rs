#![allow(dead_code)]
#![allow(unused_imports)]
extern crate pest;
extern crate thiserror;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

pub mod astbuilder;
pub mod error;
pub mod firrtl_ast;
pub mod test;

pub fn parse(code: String) -> Result<firrtl_ast::FIRRTLFile, error::Error> {
    let res = astbuilder::FIRRTLParesr::parse(astbuilder::Rule::firrtl_file, code.as_ref());

    match res {
        Ok(mut pairs) => {
            let pair_file = pairs.next().unwrap();
            let parsed_code = pair_file.as_str();
            if parsed_code.len() == code.len() {
                astbuilder::parse_file(pair_file)
            } else {
                // eprint!("{}", parsed_code);
                Err(error::Error::Parse(parsed_code.len() + 1))
            }
        }
        Err(res) => {
            // println!("{:?}", res);
            Err(error::Error::PestParse(res))
        }
    }
}

pub fn parse_wo_chk(code: String) -> Result<firrtl_ast::FIRRTLFile, error::Error> {
    let res = astbuilder::FIRRTLParesr::parse(astbuilder::Rule::firrtl_file, code.as_ref());

    match res {
        Ok(mut pairs) => {
            let pair_file = pairs.next().unwrap();
            astbuilder::parse_file(pair_file)
        }
        Err(res) => {
            // eprintln!("{:?}", res);
            Err(error::Error::PestParse(res))
        }
    }
}
