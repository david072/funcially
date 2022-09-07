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
extern crate phf;

mod astgen;
mod common;
mod engine;
mod environment;
mod color;

use std::fmt::{Display, Formatter, Write};
use rust_decimal::Decimal;
use common::Result;
use astgen::parser::{parse, ParserResult};
use astgen::tokenizer::tokenize;
use engine::evaluate;
pub use environment::{Environment, Variable};
use rust_decimal::prelude::*;
pub use color::ColorSegment;
use environment::Function;
use environment::units::format as format_unit;

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
    pub color_segments: Vec<ColorSegment>,
}

impl CalculatorResult {
    pub fn nothing(color_segments: Vec<ColorSegment>) -> CalculatorResult {
        Self {
            data: CalculatorResultData::Nothing,
            color_segments,
        }
    }

    pub fn number(result: f64, unit: Option<String>, format: Format, segments: Vec<ColorSegment>) -> Self {
        Self {
            data: CalculatorResultData::Number { result, unit, format },
            color_segments: segments,
        }
    }

    pub fn bool(bool: bool, segments: Vec<ColorSegment>) -> Self {
        Self {
            data: CalculatorResultData::Boolean(bool),
            color_segments: segments,
        }
    }

    pub fn function(name: String, arg_count: usize, segments: Vec<ColorSegment>) -> Self {
        Self {
            data: CalculatorResultData::Function(name, arg_count),
            color_segments: segments,
        }
    }
}

pub fn colorize_text(input: &str) -> Vec<ColorSegment> {
    match tokenize(input) {
        Ok(tokens) => tokens.iter().map(ColorSegment::from).collect::<Vec<_>>(),
        Err(_) => Vec::new()
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

    let color_segments = tokens.iter().map(ColorSegment::from).collect::<Vec<_>>();

    match parse(&tokens, environment)? {
        ParserResult::Calculation(ast) => {
            if verbosity == Verbosity::Ast {
                println!("AST:");
                for node in &ast { println!("{}", node); }
                println!();
            }

            let result = evaluate(ast, environment)?;
            environment.set_variable("ans", Variable(result.result, result.unit.clone())).unwrap();

            let unit = result.unit.as_ref().map(|unit| {
                if result.is_long_unit {
                    format_unit(&unit, result.result != 1.0)
                } else {
                    unit.to_string()
                }
            });
            Ok(CalculatorResult::number(result.result, unit, result.format, color_segments))
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
    if !n.is_finite() { return "infinity".to_owned(); }
    Decimal::from_f64(n).unwrap().round_dp(dp).to_string()
}

macro_rules! writeln_or_err {
    ($dst:expr) => {
        writeln_or_err!($dst, "")
    };
    ($dst:expr, $str:expr) => {
        if writeln!($dst, $str).is_err() {
            return Ok("Error writing to string".to_string());
        }
    };
    ($dst:expr, $str:expr, $($arg:expr),*) => {
        if writeln!($dst, $str, $($arg),*).is_err() {
            return Ok("Error writing to string".to_string());
        }
    };
}

pub fn get_debug_info(input: &str, env: &Environment, verbosity: Verbosity) -> Result<String> {
    let mut output = String::new();

    let tokens = tokenize(input)?;
    if matches!(verbosity, Verbosity::Tokens | Verbosity::Ast) {
        writeln_or_err!(&mut output, "Tokens:");
        for token in &tokens {
            writeln_or_err!(&mut output, "{} => {:?}", token.text, token.ty);
        }
        writeln_or_err!(&mut output);
    }

    if verbosity == Verbosity::Ast {
        match parse(&tokens, env)? {
            ParserResult::Calculation(ast) => {
                writeln_or_err!(&mut output, "AST:");
                for node in &ast { writeln_or_err!(&mut output, "{}", node); }
                writeln_or_err!(&mut output);
            }
            ParserResult::EqualityCheck(lhs, rhs) => {
                writeln_or_err!(&mut output, "Equality check:\nLHS:");
                for node in &lhs { writeln_or_err!(&mut output, "{}", node); }
                writeln_or_err!(&mut output, "RHS:");
                for node in &rhs { writeln_or_err!(&mut output, "{}", node); }
                writeln_or_err!(&mut output);
            }
            ParserResult::VariableDefinition(name, ast) => {
                if let Some(ast) = ast {
                    writeln_or_err!(&mut output, "Variable Definition: {}\nAST:", name);
                    for node in &ast { writeln_or_err!(&mut output, "{}", node); }
                } else {
                    writeln_or_err!(&mut output, "Variable removal: {}", name);
                }
            }
            ParserResult::FunctionDefinition { name, args, ast } => {
                if let Some(ast) = ast {
                    writeln_or_err!(&mut output, "Function Definition: {}", name);
                    writeln_or_err!(&mut output, "Arguments: {:?}\nAST:", args);
                    for node in &ast { writeln_or_err!(&mut output, "{}", node); }
                } else {
                    writeln_or_err!(&mut output, "Function removal: {}", name);
                }
            }
        }
    }

    Ok(output)
}
