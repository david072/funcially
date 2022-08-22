extern crate strum;
extern crate core;

mod astgen;
pub mod common;
mod engine;

use common::Result;
use astgen::parser::parse;
use astgen::tokenizer::tokenize;
use engine::evaluate;

#[derive(Debug, Clone, Copy)]
pub enum Verbosity {
    None,
    Tokens,
    Ast,
}

impl std::str::FromStr for Verbosity {
    type Err = ();

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(match s {
            "tokens" => Verbosity::Tokens,
            "ast" => Verbosity::Ast,
            _ => Verbosity::None
        })
    }
}

pub fn calculate(input: &str, verbosity: Verbosity) -> Result<f64> {
    let tokens = tokenize(input)?;
    if matches!(verbosity, Verbosity::Tokens | Verbosity::Ast) {
        println!("Tokens:");
        for token in &tokens {
            println!("{} => {:?}", token.text, token.ty);
        }
    }

    let ast = parse(&tokens)?;
    if matches!(verbosity, Verbosity::Ast) {
        println!("AST:");
        for node in &ast {
            println!("{}", node);
        }
    }

    let result = evaluate(ast)?;
    Ok(result)
}