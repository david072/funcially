extern crate strum;
extern crate core;
extern crate rust_decimal;

mod astgen;
mod common;
mod engine;
mod variables;
mod functions;
mod units;

use std::fmt::{Display, Formatter};
use rust_decimal::Decimal;
use common::Result;
use astgen::parser::{parse, ParserResult};
use astgen::tokenizer::tokenize;
use engine::evaluate;
use variables::Variables;
use rust_decimal::prelude::*;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
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
pub enum CalculatorResult {
    Number {
        result: f64,
        unit: Option<String>,
        format: Format,
    },
    Boolean(bool),
}

impl CalculatorResult {
    pub fn number(result: f64, unit: Option<String>, format: Format) -> CalculatorResult {
        CalculatorResult::Number { result, unit, format }
    }

    pub fn bool(bool: bool) -> CalculatorResult {
        CalculatorResult::Boolean(bool)
    }
}

pub struct Calculator {
    variables: Variables,
}

impl Default for Calculator {
    fn default() -> Self {
        Calculator::new()
    }
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
            println!();
        }

        match parse(&tokens)? {
            ParserResult::Calculation(ast) => {
                if verbosity == Verbosity::Ast {
                    println!("AST:");
                    for node in &ast { println!("{}", node); }
                    println!();
                }

                let result = evaluate(ast, &self.variables)?;
                self.variables.set("ans", result.result);

                Ok(CalculatorResult::number(result.result, result.unit, result.format))
            }
            ParserResult::EqualityCheck(lhs, rhs) => {
                if verbosity == Verbosity::Ast {
                    println!("Equality check:\nLHS:");
                    for node in &lhs { println!("{}", node); }
                    println!("RHS:");
                    for node in &rhs { println!("{}", node); }
                    println!();
                }

                let lhs_res = evaluate(lhs, &self.variables)?.result;
                let rhs_res = evaluate(rhs, &self.variables)?.result;

                Ok(CalculatorResult::bool(lhs_res == rhs_res))
            }
        }
    }
}

pub fn round_dp(n: f64, dp: u32) -> String {
    Decimal::from_f64(n).unwrap().round_dp(dp).to_string()
}