/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

mod astgen;
mod common;
mod engine;
mod environment;
mod color;

use std::fmt::Write;
use common::Result;
use astgen::{tokenizer::tokenize, parser::{parse, ParserResult}};
use engine::Engine;
use rust_decimal::prelude::*;
use environment::{
    units::format as format_unit,
    currencies::Currencies,
    Variable,
};
use crate::engine::Format;

pub use color::{ColorSegment, Color};
pub use environment::{Environment, Function};
pub use common::data_dir;

const CRASH_REPORTS_DIR: &str = "crash_reports";

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

/// A struct containing information about the calculated result
pub enum ResultData {
    Nothing,
    Number {
        result: f64,
        unit: Option<String>,
        format: Format,
    },
    Boolean(bool),
    /// `name`, `argument count`
    Function {
        name: String,
        arg_count: usize,
        function: Function,
    },
}

pub struct CalculatorResult {
    pub data: ResultData,
    pub color_segments: Vec<ColorSegment>,
}

impl CalculatorResult {
    pub fn nothing(color_segments: Vec<ColorSegment>) -> CalculatorResult {
        Self {
            data: ResultData::Nothing,
            color_segments,
        }
    }

    pub fn number(result: f64, unit: Option<String>, format: Format, segments: Vec<ColorSegment>) -> Self {
        Self {
            data: ResultData::Number { result, unit, format },
            color_segments: segments,
        }
    }

    pub fn bool(bool: bool, segments: Vec<ColorSegment>) -> Self {
        Self {
            data: ResultData::Boolean(bool),
            color_segments: segments,
        }
    }

    pub fn function(name: String, arg_count: usize, function: Function, segments: Vec<ColorSegment>) -> Self {
        Self {
            data: ResultData::Function { name, arg_count, function },
            color_segments: segments,
        }
    }
}

pub fn colorize_text(input: &str) -> Option<Vec<ColorSegment>> {
    match tokenize(input) {
        Ok(tokens) => Some(ColorSegment::all(&tokens)),
        Err(_) => None,
    }
}

enum Env<'a> {
    Owned(Environment),
    Ref(&'a mut Environment),
}

pub struct Calculator<'a> {
    environment: Env<'a>,
    pub currencies: std::sync::Arc<Currencies>,
    verbosity: Verbosity,
}

impl<'a> Default for Calculator<'a> {
    fn default() -> Self {
        Calculator::set_panic_hook();

        Calculator {
            environment: Env::Owned(Environment::new()),
            currencies: Currencies::new_arc(),
            verbosity: Verbosity::None,
        }
    }
}

impl<'a> Calculator<'a> {
    pub fn update_currencies() { Currencies::update(); }

    pub fn new(verbosity: Verbosity) -> Calculator<'a> {
        Calculator::set_panic_hook();

        Calculator {
            environment: Env::Owned(Environment::new()),
            currencies: Currencies::new_arc(),
            verbosity,
        }
    }

    pub fn with_environment(verbosity: Verbosity, environment: &'a mut Environment) -> Calculator<'a> {
        Calculator::set_panic_hook();

        Calculator {
            environment: Env::Ref(environment),
            currencies: Currencies::new_arc(),
            verbosity,
        }
    }

    /// Sets a panic hook, writing stack trace + PanicInfo to a file
    fn set_panic_hook() {
        // Write stack trace + PanicInfo to a file
        std::panic::set_hook(Box::new(|info| {
            let backtrace = backtrace::Backtrace::new();
            let current_millis = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs();

            let contents = format!("{:?}\n\n{}", backtrace, info);

            let path = common::data_dir().join(CRASH_REPORTS_DIR);
            #[allow(clippy::collapsible_if)] // allow for readability
            if !path.try_exists().unwrap_or(false) {
                if std::fs::create_dir(path.clone()).is_err() { return; }
            }

            let path = path.join(format!("report_{}.txt", current_millis));
            let _ = std::fs::write(path, contents);
        }));
    }

    pub fn reset(&mut self) { self.env_mut().clear(); }

    pub fn clone_env(&self) -> Environment { self.env().clone() }

    fn env(&self) -> &Environment {
        match &self.environment {
            Env::Owned(env) => env,
            Env::Ref(env) => *env,
        }
    }

    fn env_mut(&mut self) -> &mut Environment {
        match &mut self.environment {
            Env::Owned(env) => env,
            Env::Ref(env) => *env,
        }
    }

    pub fn calculate(&mut self, input: &str) -> Result<CalculatorResult> {
        let tokens = tokenize(input)?;
        if matches!(self.verbosity, Verbosity::Tokens | Verbosity::Ast) {
            println!("Tokens:");
            for token in &tokens {
                println!("{} => {:?}", token.text, token.ty);
            }
            println!();
        }

        let color_segments = ColorSegment::all(&tokens);

        // let environment = self.env_mut();
        match parse(&tokens, self.env_mut())? {
            ParserResult::Calculation(ast) => {
                if self.verbosity == Verbosity::Ast {
                    println!("AST:");
                    for node in &ast { println!("{}", node); }
                    println!();
                }

                let result = Engine::evaluate(ast, self.env(), &self.currencies)?;
                self.env_mut().set_ans_variable(Variable(result.result, result.unit.clone()));

                let unit = result.unit.as_ref().map(|unit| {
                    if result.is_long_unit {
                        format_unit(unit, result.result != 1.0)
                    } else {
                        unit.to_string()
                    }
                });
                Ok(CalculatorResult::number(result.result, unit, result.format, color_segments))
            }
            ParserResult::EqualityCheck(lhs, rhs) => {
                if self.verbosity == Verbosity::Ast {
                    println!("Equality check:\nLHS:");
                    for node in &lhs { println!("{}", node); }
                    println!("RHS:");
                    for node in &rhs { println!("{}", node); }
                    println!();
                }

                let lhs = Engine::evaluate(lhs, self.env(), &self.currencies)?;
                let rhs = Engine::evaluate(rhs, self.env(), &self.currencies)?;
                Ok(CalculatorResult::bool(Engine::equals(&lhs, &rhs, &self.currencies), color_segments))
            }
            ParserResult::VariableDefinition(name, ast) => {
                match ast {
                    Some(ast) => {
                        let res = Engine::evaluate(ast, self.env(), &self.currencies)?;
                        self.env_mut().set_variable(&name, Variable(res.result, res.unit)).unwrap();
                        Ok(CalculatorResult::nothing(color_segments))
                    }
                    None => {
                        self.env_mut().remove_variable(&name).unwrap();
                        Ok(CalculatorResult::nothing(color_segments))
                    }
                }
            }
            ParserResult::FunctionDefinition { name, args, ast } => {
                match ast {
                    Some(ast) => {
                        let arg_count = args.len();
                        let function = Function(args, ast);
                        self.env_mut().set_function(&name, function.clone()).unwrap();
                        Ok(CalculatorResult::function(name, arg_count, function, color_segments))
                    }
                    None => {
                        self.env_mut().remove_function(&name).unwrap();
                        Ok(CalculatorResult::nothing(color_segments))
                    }
                }
            }
            ParserResult::Equation {
                lhs,
                rhs,
                is_question_mark_in_lhs,
                output_variable,
            } => {
                if self.verbosity == Verbosity::Ast {
                    println!("Equation:");
                    println!("QuestionMark is in {}\nLHS:", if is_question_mark_in_lhs { "LHS" } else { "RHS" });
                    for node in &lhs { println!("{}", node); }
                    println!("RHS:");
                    for node in &rhs { println!("{}", node); }
                    println!();
                }

                let result = Engine::solve(
                    lhs,
                    rhs,
                    is_question_mark_in_lhs,
                    self.env(),
                    &self.currencies,
                )?;

                self.env_mut().set_ans_variable(Variable(result.result, result.unit.clone()));
                if let Some((name, range)) = output_variable {
                    if let Err(ty) = self.env_mut().set_variable(
                        &name,
                        Variable(result.result, result.unit.clone())) {
                        return Err(ty.with(range));
                    }
                }

                Ok(CalculatorResult::number(result.result, result.unit.map(|u| u.to_string()), result.format, color_segments))
            }
        }
    }

    pub fn get_debug_info(&self, input: &str, verbosity: Verbosity) -> Result<String> {
        let mut output = String::new();

        let tokens = tokenize(input)?;
        if matches!(verbosity, Verbosity::Tokens | Verbosity::Ast) {
            writeln_or_err!(&mut output, "Tokens:");
            for token in &tokens {
                writeln_or_err!(&mut output, "{} => {:?}", token.text, token.ty);
            }
            writeln_or_err!(&mut output);
        }

        let environment = self.env();

        if verbosity == Verbosity::Ast {
            match parse(&tokens, environment) {
                Ok(parser_result) => match parser_result {
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
                    ParserResult::Equation {
                        lhs,
                        rhs,
                        is_question_mark_in_lhs,
                        output_variable
                    } => {
                        writeln_or_err!(&mut output, "Equation:");
                        writeln_or_err!(&mut output, "QuestionMark is in {}, output variable: {:?}\nLHS:", if is_question_mark_in_lhs { "LHS" } else { "RHS" }, output_variable);
                        for node in &lhs { writeln_or_err!(&mut output, "{}", node); }
                        writeln_or_err!(&mut output, "RHS:");
                        for node in &rhs { writeln_or_err!(&mut output, "{}", node); }
                        writeln_or_err!(&mut output);
                    }
                }
                Err(e) => {
                    writeln_or_err!(&mut output, "Error while parsing: {} at {}..{}", e.error, e.start, e.end);
                    return Ok(output);
                }
            }
        }

        Ok(output)
    }
}
