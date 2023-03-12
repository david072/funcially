/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::Write;
use std::str::FromStr;

use astgen::{
    parser::Parser,
    tokenizer::{tokenize, TokenType},
};
pub use color::{Color, ColorSegment};
pub use common::data_dir;
use common::Result;
use engine::Engine;
use environment::{
    currencies::Currencies,
    Variable,
};
pub use environment::{Environment, Function};

use crate::astgen::parser::ParserResult;
use crate::astgen::tokenizer::Token;
pub use crate::engine::Format;
pub use crate::engine::NumberValue;
use crate::engine::Value;
use crate::environment::units::is_unit_with_prefix;
pub use crate::settings::*;

mod astgen;
mod common;
mod engine;
mod environment;
mod color;
mod settings;

const CRASH_REPORTS_DIR: &str = "crash_reports";

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
    Value(Value),
    Boolean(bool),
    /// `name`, `argument count`
    Function {
        name: String,
        arg_count: usize,
        function: Function,
    },
}

pub struct CalculatorResult {
    pub data: Result<ResultData>,
    pub color_segments: Vec<ColorSegment>,
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

#[derive(Clone, Copy)]
pub struct Context<'a> {
    pub env: &'a Environment,
    pub currencies: &'a Currencies,
    pub settings: &'a Settings,
}

pub struct Calculator<'a> {
    environment: Env<'a>,
    pub currencies: std::sync::Arc<Currencies>,
    pub settings: Settings,
    verbosity: Verbosity,
}

impl<'a> Default for Calculator<'a> {
    fn default() -> Self {
        Calculator::set_panic_hook();

        Calculator {
            environment: Env::Owned(Environment::new()),
            currencies: Currencies::new_arc(),
            settings: Settings::default(),
            verbosity: Verbosity::None,
        }
    }
}

impl<'a> Calculator<'a> {
    pub fn update_currencies() { Currencies::update(); }

    pub fn new(verbosity: Verbosity, settings: Settings) -> Calculator<'a> {
        Calculator::set_panic_hook();

        Calculator {
            environment: Env::Owned(Environment::new()),
            currencies: Currencies::new_arc(),
            settings,
            verbosity,
        }
    }

    pub fn with_environment(verbosity: Verbosity, settings: Settings, environment: &'a mut Environment) -> Calculator<'a> {
        Calculator::set_panic_hook();

        Calculator {
            environment: Env::Ref(environment),
            currencies: Currencies::new_arc(),
            verbosity,
            settings,
        }
    }

    /// Sets a panic hook, writing stack trace + PanicInfo to a file
    fn set_panic_hook() {
        // Write stack trace + PanicInfo to a file
        std::panic::set_hook(Box::new(|info| {
            let backtrace = std::backtrace::Backtrace::capture();
            let current_millis = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs();

            let contents = format!("{info}\n\n{backtrace}");

            eprintln!("{contents}");

            let path = data_dir().join(CRASH_REPORTS_DIR);
            #[allow(clippy::collapsible_if)] // allow for readability
            if !path.try_exists().unwrap_or(false) {
                if std::fs::create_dir(path.clone()).is_err() { return; }
            }

            let path = path.join(format!("report_{current_millis}.txt"));
            let _ = std::fs::write(path, contents);
        }));
    }

    pub fn reset(&mut self) { self.env_mut().clear(); }

    pub fn clone_env(&self) -> Environment { self.env().clone() }

    fn env(&self) -> &Environment {
        match &self.environment {
            Env::Owned(env) => env,
            Env::Ref(env) => env,
        }
    }

    fn env_mut(&mut self) -> &mut Environment {
        match &mut self.environment {
            Env::Owned(env) => env,
            Env::Ref(env) => env,
        }
    }

    pub fn context(&self) -> Context {
        Context {
            env: self.env(),
            currencies: &self.currencies,
            settings: &self.settings,
        }
    }

    pub fn calculate(&mut self, input: &str) -> CalculatorResult {
        let tokens = match tokenize(input) {
            Ok(v) => v,
            Err(e) => return CalculatorResult { data: Err(e), color_segments: vec![] },
        };
        if matches!(self.verbosity, Verbosity::Tokens | Verbosity::Ast) {
            println!("Tokens:");
            for token in &tokens {
                println!("{} => {:?}", token.text, token.ty);
            }
            println!();
        }

        let color_segments = ColorSegment::all(&tokens);

        let data = self.calculate_impl(tokens);
        CalculatorResult { data, color_segments }
    }

    fn calculate_impl(&mut self, tokens: Vec<Token>) -> Result<ResultData> {
        match Parser::parse(&tokens, self.context())? {
            ParserResult::Calculation(ast) => {
                if self.verbosity == Verbosity::Ast {
                    println!("AST:");
                    for node in &ast { println!("{node}"); }
                    println!();
                }

                let result = Engine::evaluate(ast, self.context())?;
                self.env_mut().set_ans_variable(Variable(result.clone()));

                Ok(ResultData::Value(result))
            }
            ParserResult::BooleanExpression { lhs, rhs, operator } => {
                if self.verbosity == Verbosity::Ast {
                    println!("Equality check:\nLHS:");
                    for node in &lhs { println!("{node}"); }
                    println!("RHS:");
                    for node in &rhs { println!("{node}"); }
                    println!();
                }

                let lhs = Engine::evaluate(lhs, self.context())?;
                let rhs = Engine::evaluate(rhs, self.context())?;
                Ok(ResultData::Boolean(Engine::check_boolean_operator(&lhs, &rhs, operator, &self.currencies)))
            }
            ParserResult::VariableDefinition(name, ast) => {
                match ast {
                    Some(ast) => {
                        let res = Engine::evaluate(ast, self.context())?;
                        self.env_mut().set_variable(&name, Variable(res.clone())).unwrap();
                        Ok(ResultData::Value(res))
                    }
                    None => {
                        self.env_mut().remove_variable(&name).unwrap();
                        Ok(ResultData::Nothing)
                    }
                }
            }
            ParserResult::FunctionDefinition { name, args, ast } => {
                match ast {
                    Some(ast) => {
                        let arg_count = args.len();
                        let function = Function(args, ast);
                        self.env_mut().set_function(&name, function.clone()).unwrap();
                        Ok(ResultData::Function { name, arg_count, function })
                    }
                    None => {
                        self.env_mut().remove_function(&name).unwrap();
                        Ok(ResultData::Nothing)
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
                    for node in &lhs { println!("{node}"); }
                    println!("RHS:");
                    for node in &rhs { println!("{node}"); }
                    println!();
                }

                let result = Engine::solve(
                    lhs,
                    rhs,
                    is_question_mark_in_lhs,
                    self.context(),
                )?;

                self.env_mut().set_ans_variable(Variable(result.clone()));
                if let Some((name, range)) = output_variable {
                    if let Err(ty) = self.env_mut().set_variable(
                        &name,
                        Variable(result.clone())) {
                        return Err(ty.with(range));
                    }
                }

                Ok(ResultData::Value(result))
            }
        }
    }

    pub fn format(&self, line: &str) -> Result<String> {
        use TokenType::*;

        let tokens = tokenize(line)?;

        let mut is_in_unit = false;
        let mut is_in_object = false;

        let mut new_line = String::new();
        for (i, token) in tokens.iter().enumerate() {
            let text = &token.text;

            if token.ty.is_number() || matches!(token.ty, ExclamationMark
                | PercentSign
                | QuestionMark
            ) {
                let mut text = text.to_owned();
                if token.ty == DecimalLiteral {
                    text = if text.contains('.') {
                        text.trim_matches('0').to_owned()
                    } else if text.len() > 1 {
                        text.trim_start_matches('0').to_owned()
                    } else {
                        text
                    };

                    if text.len() == 1 && text == "." {
                        text = "0.0".to_owned();
                    } else if text.ends_with('.') {
                        text.remove(text.len() - 1);
                    }
                } else if token.ty == HexLiteral || token.ty == BinaryLiteral {
                    text = text[2..].trim_start_matches('0').to_owned();
                    text.insert(0, if token.ty == HexLiteral { 'x' } else { 'b' });
                    text.insert(0, '0');
                    if text.len() == 2 { text.push('0'); }
                    if token.ty == HexLiteral {
                        // Uppercase letters in hex numbers
                        text.replace_range(2.., text[2..].chars()
                            .flat_map(|c| if !c.is_numeric() {
                                c.to_uppercase().collect::<Vec<char>>()
                            } else {
                                vec![c]
                            })
                            .collect::<String>().as_str());
                    }
                }

                if i != 0 && token.ty == Identifier {
                    if let Some(previous) = tokens.get(i - 1) {
                        if previous.ty == Identifier {
                            new_line.push(' ');
                        }
                    }
                }

                if (i != 0 && token.ty.is_literal() && tokens[i - 1].ty == Identifier && !matches!(tokens[i - 1].text.as_str(), "e" | "E")) ||
                    (token.ty == OpenSquareBracket && i != 0 && tokens[i - 1].ty != ObjectArgs && is_in_object) {
                    new_line.push(' ');
                }

                if token.ty == OpenSquareBracket && i != 0 && !tokens[i - 1].ty.is_operator() {
                    is_in_unit = true;
                } else if token.ty == CloseSquareBracket {
                    is_in_unit = false;
                }

                if token.ty == OpenCurlyBracket {
                    is_in_object = true;
                } else if token.ty == CloseCurlyBracket {
                    is_in_object = false;
                }

                new_line += &text;
            } else if token.ty.is_operator() || token.ty.is_boolean_operator() || token.ty.is_format() ||
                token.ty == DefinitionSign {
                if token.ty == Plus || token.ty == Minus {
                    if i == 0 {
                        new_line += text;
                        continue;
                    } else if tokens[i - 1].ty.is_operator() || matches!(tokens[i - 1].ty,
                        OpenBracket
                        | Comma
                        | DefinitionSign
                        | EqualsSign
                        | Identifier
                    ) { // Check if we're a sign
                        // Check if we're a sign after the 'e' of scientific notation
                        if tokens[i - 1].ty != Identifier || (tokens[i - 1].text == "e" || tokens[i - 1].text == "E") {
                            if let Some(next) = tokens.get(i + 1) {
                                if next.ty.is_number() {
                                    new_line += text;
                                    continue;
                                }
                            }
                        }
                    }
                }
                // Format complex units without spaces (e.g. "km/h")
                else if token.ty == Divide || token.ty == Multiply {
                    let is_prev_ident_and_unit = tokens.get(i - 1)
                        .map(|t| t.ty == Identifier && is_unit_with_prefix(&t.text))
                        .unwrap_or(false);
                    let is_next_ident_and_unit = tokens.get(i + 1)
                        .map(|t| t.ty == Identifier && is_unit_with_prefix(&t.text))
                        .unwrap_or(false);
                    if is_prev_ident_and_unit && is_next_ident_and_unit {
                        new_line += text;
                        continue;
                    }
                }

                if !(token.ty.is_format() && tokens.get(i.saturating_sub(1))
                    .map_or(false, |t| t.ty == In)) &&
                    token.ty != Exponentiation && !is_in_unit {
                    new_line.push(' ');
                }
                new_line += text;
                if i != tokens.len() - 1 && token.ty != Exponentiation && !is_in_unit {
                    new_line.push(' ');
                }
            } else if matches!(token.ty, Comma | Semicolon) {
                new_line += text;
                if i != tokens.len() - 1 {
                    new_line.push(' ');
                }
            } else if token.ty == ObjectArgs {
                if tokens.get(i - 1).map(|t| t.ty == Identifier).unwrap_or_default() {
                    new_line.push(' ');
                }
                new_line += text;
            }
        }

        Ok(new_line.to_string())
    }

    pub fn get_debug_info(&self, input: &str, verbosity: Verbosity) -> String {
        let mut output = "Line:\n".to_string();

        let tokens = match tokenize(input) {
            Ok(tokens) => tokens,
            Err(e) => {
                writeln!(&mut output, "Error while tokenizing: {} at", e.error).unwrap();
                for range in e.ranges {
                    writeln!(&mut output, "\t{range:?}").unwrap();
                }

                return output;
            }
        };

        if matches!(verbosity, Verbosity::Tokens | Verbosity::Ast) {
            writeln!(&mut output, "Tokens:").unwrap();
            for token in &tokens {
                writeln!(&mut output, "{} => {:?}", token.text, token.ty).unwrap();
            }
            writeln!(&mut output).unwrap();
        }

        if verbosity == Verbosity::Ast {
            match Parser::parse(&tokens, self.context()) {
                Ok(parser_result) => match parser_result {
                    ParserResult::Calculation(ast) => {
                        writeln!(&mut output, "AST:").unwrap();
                        for node in &ast { writeln!(&mut output, "{}", node).unwrap(); }
                        writeln!(&mut output).unwrap();
                    }
                    ParserResult::BooleanExpression { lhs, rhs, operator } => {
                        writeln!(&mut output, "Boolean expression:\nOperator: {operator:?}\nLHS:").unwrap();
                        for node in &lhs { writeln!(&mut output, "{}", node).unwrap(); }
                        writeln!(&mut output, "RHS:").unwrap();
                        for node in &rhs { writeln!(&mut output, "{}", node).unwrap(); }
                        writeln!(&mut output).unwrap();
                    }
                    ParserResult::VariableDefinition(name, ast) => {
                        if let Some(ast) = ast {
                            writeln!(&mut output, "Variable Definition: {}\nAST:", name).unwrap();
                            for node in &ast { writeln!(&mut output, "{}", node).unwrap(); }
                        } else {
                            writeln!(&mut output, "Variable removal: {}", name).unwrap();
                        }
                    }
                    ParserResult::FunctionDefinition { name, args, ast } => {
                        if let Some(ast) = ast {
                            writeln!(&mut output, "Function Definition: {}", name).unwrap();
                            writeln!(&mut output, "Arguments: {:?}\nAST:", args).unwrap();
                            for node in &ast { writeln!(&mut output, "{}", node).unwrap(); }
                        } else {
                            writeln!(&mut output, "Function removal: {}", name).unwrap();
                        }
                    }
                    ParserResult::Equation {
                        lhs,
                        rhs,
                        is_question_mark_in_lhs,
                        output_variable
                    } => {
                        writeln!(&mut output, "Equation:").unwrap();
                        writeln!(&mut output, "QuestionMark is in {}, output variable: {:?}\nLHS:", if is_question_mark_in_lhs { "LHS" } else { "RHS" }, output_variable).unwrap();
                        for node in &lhs { writeln!(&mut output, "{}", node).unwrap(); }
                        writeln!(&mut output, "RHS:").unwrap();
                        for node in &rhs { writeln!(&mut output, "{}", node).unwrap(); }
                        writeln!(&mut output).unwrap();
                    }
                }
                Err(e) => {
                    writeln!(&mut output, "Error while parsing: {} at", e.error).unwrap();
                    for range in e.ranges {
                        writeln!(&mut output, "\t{range:?}").unwrap();
                    }

                    return output;
                }
            }
        }

        output + &format!("\nEnvironment:\n{}", self.env().get_debug_info())
    }
}
