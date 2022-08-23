extern crate strum;
extern crate core;

mod astgen;
mod common;
mod engine;
mod variables;

use std::fmt::{Display, Formatter};
use common::Result;
use astgen::parser::parse;
use astgen::tokenizer::tokenize;
use engine::evaluate;
use variables::Variables;

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

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Format { Decimal, Hex, Binary }

impl Display for Format {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Format::Decimal => write!(f, "decimal"),
            Format::Hex => write!(f, "hex"),
            Format::Binary => write!(f, "binary"),
        }
    }
}

/// A struct containing information about the calculated result
pub struct CalculatorResult {
    pub result: f64,
    pub format: Format,
}

impl CalculatorResult {
    pub fn new(result: f64, format: Format) -> CalculatorResult {
        CalculatorResult { result, format }
    }
}

pub struct Calculator {
    variables: Variables,
}

impl Calculator {
    pub fn new() -> Calculator {
        Calculator { variables: Variables::new() }
    }

    pub fn calculate(&mut self, input: &str, verbosity: Verbosity) -> Result<CalculatorResult> {
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

        let result = evaluate(ast, &self.variables)?;
        self.variables.set("ans", result.result);

        Ok(result)
    }
}
