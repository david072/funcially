/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

extern crate strum;
extern crate core;
extern crate rust_decimal;
extern crate eframe;
extern crate thiserror;

mod astgen;
mod common;
mod engine;
mod environment;
mod color;

use std::fmt::{Display, Formatter};
use rust_decimal::Decimal;
use common::Result;
use astgen::parser::{parse, ParserResult};
use astgen::tokenizer::tokenize;
use engine::evaluate;
pub use environment::{Environment, Variable};
use rust_decimal::prelude::*;
pub use color::Segment;
use environment::Function;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Verbosity {
    None,
    Tokens,
    Ast,
}

impl FromStr for Verbosity {
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
pub enum CalculatorResultData {
    Nothing,
    Number {
        result: f64,
        unit: Option<String>,
        format: Format,
    },
    Boolean(bool),
    Function(String, usize), // name, # of arguments
}

pub struct CalculatorResult {
    pub data: CalculatorResultData,
    pub color_segments: Vec<Segment>,
}

impl CalculatorResult {
    pub fn nothing(color_segments: Vec<Segment>) -> CalculatorResult {
        Self {
            data: CalculatorResultData::Nothing,
            color_segments,
        }
    }

    pub fn number(result: f64, unit: Option<String>, format: Format, segments: Vec<Segment>) -> Self {
        Self {
            data: CalculatorResultData::Number { result, unit, format },
            color_segments: segments,
        }
    }

    pub fn bool(bool: bool, segments: Vec<Segment>) -> Self {
        Self {
            data: CalculatorResultData::Boolean(bool),
            color_segments: segments,
        }
    }

    pub fn function(name: String, arg_count: usize, segments: Vec<Segment>) -> Self {
        Self {
            data: CalculatorResultData::Function(name, arg_count),
            color_segments: segments,
        }
    }
}

pub fn calculate(input: &str, environment: &mut Environment, verbosity: Verbosity) -> Result<CalculatorResult> {
    let tokens = tokenize(input)?;
    if matches!(verbosity, Verbosity::Tokens | Verbosity::Ast) {
        println!("Tokens:");
        for token in &tokens {
            println!("{} => {:?}", token.text, token.ty);
        }
        println!();
    }

    let color_segments = tokens.iter().map(Segment::from).collect::<Vec<_>>();

    match parse(&tokens, environment)? {
        ParserResult::Calculation(ast) => {
            if verbosity == Verbosity::Ast {
                println!("AST:");
                for node in &ast { println!("{}", node); }
                println!();
            }

            let result = evaluate(ast, environment)?;
            environment.set_variable("ans", Variable(result.result, result.unit.clone())).unwrap();

            Ok(CalculatorResult::number(result.result, result.unit,
                                        result.format, color_segments))
        }
        ParserResult::EqualityCheck(lhs, rhs) => {
            if verbosity == Verbosity::Ast {
                println!("Equality check:\nLHS:");
                for node in &lhs { println!("{}", node); }
                println!("RHS:");
                for node in &rhs { println!("{}", node); }
                println!();
            }

            let lhs_res = evaluate(lhs, environment)?.result;
            let rhs_res = evaluate(rhs, environment)?.result;

            Ok(CalculatorResult::bool(lhs_res == rhs_res, color_segments))
        }
        ParserResult::VariableDefinition(name, ast) => {
            match ast {
                Some(ast) => {
                    let res = evaluate(ast, environment)?;
                    environment.set_variable(&name, Variable(res.result, res.unit)).unwrap();
                    Ok(CalculatorResult::nothing(color_segments))
                }
                None => {
                    environment.remove_variable(&name).unwrap();
                    Ok(CalculatorResult::nothing(color_segments))
                }
            }
        }
        ParserResult::FunctionDefinition { name, args, ast } => {
            match ast {
                Some(ast) => {
                    let arg_count = args.len();
                    environment.set_function(&name, Function(args, ast)).unwrap();
                    Ok(CalculatorResult::function(name, arg_count, color_segments))
                }
                None => {
                    environment.remove_function(&name).unwrap();
                    Ok(CalculatorResult::nothing(color_segments))
                }
            }
        }
    }
}

pub fn round_dp(n: f64, dp: u32) -> String {
    if n.is_nan() { return "NaN".to_owned(); }
    Decimal::from_f64(n).unwrap().round_dp(dp).to_string()
}