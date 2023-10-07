/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::cell::RefCell;
use std::fmt::Write;
use std::ops::Range;
use std::rc::Rc;
use std::str::FromStr;
use std::sync::Arc;

use astgen::{
    parser::Parser,
    tokenizer::{tokenize, TokenType},
};
pub use color::{Color, ColorSegment};
pub use common::Result;
pub use common::{data_dir, SourceRange};
use engine::Engine;
pub use environment::{currencies::Currencies, Variable};
pub use environment::{Environment, Function};

use crate::astgen::parser::{ParserResult, ParserResultData};
pub use crate::engine::Format;
pub use crate::engine::NumberValue;
pub use crate::engine::Value;
use crate::environment::units::is_unit_with_prefix;
use crate::environment::FunctionVariantType;
pub use crate::settings::*;

mod astgen;
mod color;
mod common;
mod engine;
mod environment;
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
            _ => Verbosity::None,
        })
    }
}

/// A struct containing information about the calculated result
#[derive(Debug, Clone)]
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
    FunctionRemoval(String),
}

#[derive(Debug)]
pub struct CalculatorResult {
    pub data: Result<(ResultData, Range<usize>)>,
    pub color_segments: Vec<ColorSegment>,
}

pub fn colorize_text(input: &str) -> Option<Vec<ColorSegment>> {
    match tokenize(input) {
        Ok(tokens) => Some(ColorSegment::all(&tokens)),
        Err(_) => None,
    }
}

#[derive(Clone)]
pub struct ContextData {
    pub env: Environment,
    pub currencies: Arc<Currencies>,
    pub settings: Settings,
}

pub type Context = Rc<RefCell<ContextData>>;

pub struct Calculator {
    pub context: Context,
    pub verbosity: Verbosity,
}

impl Default for Calculator {
    fn default() -> Self {
        Calculator::set_panic_hook();

        Calculator {
            context: Rc::new(RefCell::new(ContextData {
                env: Environment::new(),
                currencies: Currencies::new_with_update(),
                settings: Settings::default(),
            })),
            verbosity: Verbosity::None,
        }
    }
}

impl Calculator {
    pub fn update_currencies() {
        Currencies::update();
    }

    pub fn new(verbosity: Verbosity, settings: Settings) -> Calculator {
        Calculator::set_panic_hook();

        Calculator {
            context: Rc::new(RefCell::new(ContextData {
                env: Environment::new(),
                currencies: Currencies::new_with_update(),
                settings,
            })),
            verbosity,
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
                if std::fs::create_dir(path.clone()).is_err() {
                    return;
                }
            }

            let path = path.join(format!("report_{current_millis}.txt"));
            let _ = std::fs::write(path, contents);
        }));
    }

    pub fn reset(&mut self) {
        self.context.borrow_mut().env.clear();
    }

    pub fn clone_env(&self) -> Environment {
        self.context.borrow().env.clone()
    }

    pub fn context(&self) -> Context {
        self.context.clone()
    }

    pub fn calculate(&mut self, input: &str) -> Vec<CalculatorResult> {
        let tokens = match tokenize(input) {
            Ok(v) => v,
            Err(e) => {
                return vec![CalculatorResult {
                    data: Err(e),
                    color_segments: vec![],
                }]
            }
        };

        let mut results = vec![];
        let mut parser = Parser::from_tokens(&tokens, self.context());
        while let Some(parser_result) = parser.next() {
            match parser_result {
                Ok(v) => {
                    let color_segments = ColorSegment::all(&tokens[v.token_range.clone()]);
                    results.push(CalculatorResult {
                        data: self.handle_parser_result(v),
                        color_segments,
                    });
                }
                Err(e) => results.push(CalculatorResult {
                    data: Err(e),
                    color_segments: vec![],
                }),
            }
        }

        results
    }

    fn handle_parser_result(
        &mut self,
        parser_result: ParserResult,
    ) -> Result<(ResultData, Range<usize>)> {
        let result_data = match parser_result.data {
            ParserResultData::Calculation(ast) => {
                if self.verbosity == Verbosity::Ast {
                    println!("AST:");
                    for node in &ast {
                        println!("{node}");
                    }
                    println!();
                }

                let result = Engine::evaluate(ast, self.context())?;
                self.context
                    .borrow_mut()
                    .env
                    .set_ans_variable(Variable(result.clone()));

                ResultData::Value(result)
            }
            ParserResultData::BooleanExpression { lhs, rhs, operator } => {
                if self.verbosity == Verbosity::Ast {
                    println!("Equality check:\nLHS:");
                    for node in &lhs {
                        println!("{node}");
                    }
                    println!("RHS:");
                    for node in &rhs {
                        println!("{node}");
                    }
                    println!();
                }

                let lhs = Engine::evaluate(lhs, self.context())?;
                let rhs = Engine::evaluate(rhs, self.context())?;
                ResultData::Boolean(Engine::check_boolean_operator(
                    &lhs,
                    &rhs,
                    operator,
                    &self.context.borrow().currencies,
                ))
            }
            ParserResultData::VariableDefinition(name, ast) => match ast {
                Some(ast) => {
                    let res = Engine::evaluate(ast, self.context())?;
                    self.context
                        .borrow_mut()
                        .env
                        .set_variable(&name, Variable(res.clone()))
                        .unwrap();
                    ResultData::Value(res)
                }
                None => {
                    self.context
                        .borrow_mut()
                        .env
                        .remove_variable(&name)
                        .unwrap();
                    ResultData::Nothing
                }
            },
            ParserResultData::FunctionDefinition { name, function } => match function {
                Some(function) => {
                    let arg_count = function.arguments.len();
                    self.context
                        .borrow_mut()
                        .env
                        .set_function(&name, function.clone())
                        .unwrap();
                    ResultData::Function {
                        name,
                        arg_count,
                        function,
                    }
                }
                None => {
                    self.context
                        .borrow_mut()
                        .env
                        .remove_function(&name)
                        .unwrap();
                    ResultData::FunctionRemoval(name)
                }
            },
            ParserResultData::Equation {
                lhs,
                rhs,
                is_question_mark_in_lhs,
                output_variable,
            } => {
                if self.verbosity == Verbosity::Ast {
                    println!("Equation:");
                    println!(
                        "QuestionMark is in {}\nLHS:",
                        if is_question_mark_in_lhs {
                            "LHS"
                        } else {
                            "RHS"
                        }
                    );
                    for node in &lhs {
                        println!("{node}");
                    }
                    println!("RHS:");
                    for node in &rhs {
                        println!("{node}");
                    }
                    println!();
                }

                let result = Engine::solve(lhs, rhs, is_question_mark_in_lhs, self.context())?;

                self.context
                    .borrow_mut()
                    .env
                    .set_ans_variable(Variable(result.clone()));
                if let Some((name, range)) = output_variable {
                    if let Err(ty) = self
                        .context
                        .borrow_mut()
                        .env
                        .set_variable(&name, Variable(result.clone()))
                    {
                        return Err(ty.with(range));
                    }
                }

                ResultData::Value(result)
            }
        };

        Ok((result_data, parser_result.line_range))
    }

    pub fn format(&self, line: &str) -> Result<String> {
        use TokenType::*;

        let tokens = tokenize(line)?;

        let mut is_in_unit = false;
        let mut is_in_object = false;

        let mut new_line = String::new();
        for (i, token) in tokens.iter().enumerate() {
            let text = &token.text;

            if token.ty.is_number()
                || matches!(token.ty, ExclamationMark | PercentSign | QuestionMark)
            {
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
                    if text.len() == 2 {
                        text.push('0');
                    }
                    if token.ty == HexLiteral {
                        // Uppercase letters in hex numbers
                        text.replace_range(
                            2..,
                            text[2..]
                                .chars()
                                .flat_map(|c| {
                                    if !c.is_numeric() {
                                        c.to_uppercase().collect::<Vec<char>>()
                                    } else {
                                        vec![c]
                                    }
                                })
                                .collect::<String>()
                                .as_str(),
                        );
                    }
                }

                if i != 0 && token.ty == Identifier {
                    if let Some(previous) = tokens.get(i - 1) {
                        if previous.ty == Identifier {
                            new_line.push(' ');
                        }
                    }
                }

                if (i != 0
                    && token.ty.is_literal()
                    && tokens[i - 1].ty == Identifier
                    && !matches!(tokens[i - 1].text.as_str(), "e" | "E"))
                    || (token.ty == OpenSquareBracket
                        && i != 0
                        && tokens[i - 1].ty != ObjectArgs
                        && is_in_object)
                {
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
            } else if token.ty.is_operator()
                || token.ty.is_boolean_operator()
                || token.ty.is_format()
                || token.ty == DefinitionSign
                || token.ty == PostfixDefinitionSign
            {
                if token.ty == Plus || token.ty == Minus {
                    if i == 0 {
                        new_line += text;
                        continue;
                    } else if tokens[i - 1].ty.is_operator()
                        || matches!(
                            tokens[i - 1].ty,
                            OpenBracket | Comma | DefinitionSign | EqualsSign | Identifier
                        )
                    {
                        // Check if we're a sign
                        // Check if we're a sign after the 'e' of scientific notation
                        if tokens[i - 1].ty != Identifier
                            || (tokens[i - 1].text == "e" || tokens[i - 1].text == "E")
                        {
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
                    let is_prev_ident_and_unit = tokens
                        .get(i - 1)
                        .map(|t| t.ty == Identifier && is_unit_with_prefix(&t.text))
                        .unwrap_or(false);
                    let is_next_ident_and_unit = tokens
                        .get(i + 1)
                        .map(|t| t.ty == Identifier && is_unit_with_prefix(&t.text))
                        .unwrap_or(false);
                    if is_prev_ident_and_unit && is_next_ident_and_unit {
                        new_line += text;
                        continue;
                    }
                }

                if !(token.ty.is_format()
                    && tokens
                        .get(i.saturating_sub(1))
                        .map_or(false, |t| t.ty == In))
                    && token.ty != Exponentiation
                    && !is_in_unit
                {
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
                if tokens
                    .get(i - 1)
                    .map(|t| t.ty == Identifier)
                    .unwrap_or_default()
                {
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
            match Parser::from_tokens(&tokens, self.context()).parse_single() {
                Ok(parser_result) => match parser_result.data {
                    ParserResultData::Calculation(ast) => {
                        writeln!(&mut output, "AST:").unwrap();
                        for node in &ast {
                            writeln!(&mut output, "{}", node).unwrap();
                        }
                        writeln!(&mut output).unwrap();
                    }
                    ParserResultData::BooleanExpression { lhs, rhs, operator } => {
                        writeln!(
                            &mut output,
                            "Boolean expression:\nOperator: {operator:?}\nLHS:"
                        )
                        .unwrap();
                        for node in &lhs {
                            writeln!(&mut output, "{}", node).unwrap();
                        }
                        writeln!(&mut output, "RHS:").unwrap();
                        for node in &rhs {
                            writeln!(&mut output, "{}", node).unwrap();
                        }
                        writeln!(&mut output).unwrap();
                    }
                    ParserResultData::VariableDefinition(name, ast) => {
                        if let Some(ast) = ast {
                            writeln!(&mut output, "Variable Definition: {}\nAST:", name).unwrap();
                            for node in &ast {
                                writeln!(&mut output, "{}", node).unwrap();
                            }
                        } else {
                            writeln!(&mut output, "Variable removal: {}", name).unwrap();
                        }
                    }
                    ParserResultData::FunctionDefinition { name, function } => {
                        if let Some(function) = function {
                            writeln!(&mut output, "Function Definition: {}", name).unwrap();
                            writeln!(&mut output, "Arguments: {:?}\nAST:", function.arguments)
                                .unwrap();
                            for (variant, ast) in &function.variants {
                                match variant {
                                    FunctionVariantType::BooleanVariant { lhs, rhs, operator } => {
                                        write!(&mut output, "Boolean Variant: ").unwrap();
                                        for node in lhs {
                                            write!(&mut output, "{}", node).unwrap();
                                        }
                                        write!(&mut output, " {operator} ").unwrap();
                                        for node in rhs {
                                            write!(&mut output, "{}", node).unwrap();
                                        }
                                        write!(&mut output, ": ").unwrap();
                                    }
                                    FunctionVariantType::Else => {
                                        write!(&mut output, "Else Variant: ").unwrap();
                                    }
                                }

                                for node in ast {
                                    writeln!(&mut output, "{}", node).unwrap();
                                }
                            }
                        } else {
                            writeln!(&mut output, "Function removal: {}", name).unwrap();
                        }
                    }
                    ParserResultData::Equation {
                        lhs,
                        rhs,
                        is_question_mark_in_lhs,
                        output_variable,
                    } => {
                        writeln!(&mut output, "Equation:").unwrap();
                        writeln!(
                            &mut output,
                            "QuestionMark is in {}, output variable: {:?}\nLHS:",
                            if is_question_mark_in_lhs {
                                "LHS"
                            } else {
                                "RHS"
                            },
                            output_variable
                        )
                        .unwrap();
                        for node in &lhs {
                            writeln!(&mut output, "{}", node).unwrap();
                        }
                        writeln!(&mut output, "RHS:").unwrap();
                        for node in &rhs {
                            writeln!(&mut output, "{}", node).unwrap();
                        }
                        writeln!(&mut output).unwrap();
                    }
                },
                Err(e) => {
                    writeln!(&mut output, "Error while parsing: {} at", e.error).unwrap();
                    for range in e.ranges {
                        writeln!(&mut output, "\t{range:?}").unwrap();
                    }

                    return output;
                }
            }
        }

        output
            + &format!(
                "\nEnvironment:\n{}",
                self.context.borrow().env.get_debug_info()
            )
    }
}
