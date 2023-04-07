/*
 * Copyright (c) 2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;

use crate::{Context, error, Format};
use crate::astgen::ast::{AstNode, AstNodeData, AstNodeModifier, BooleanOperator, Operator};
use crate::astgen::objects::{CalculatorObject, ObjectArgument, Vector};
use crate::astgen::tokenizer::{Token, TokenType, TokenType::*};
use crate::common::{Error, ErrorType::*, ErrorType, Result, SourceRange};
use crate::engine::{Engine, Value};
use crate::environment::{ArgCount, FunctionArgument};
use crate::environment::units::{get_prefix_power, is_unit_with_prefix, Unit};

macro_rules! parse_f64_radix {
    ($text:expr, $radix:expr, $range:expr) => {
        {
            match u128::from_str_radix(&$text[2..], $radix) {
                Ok(int) => int as f64,
                Err(e) => error!(InvalidNumber(e.to_string()): $range),
            }
        }
    }
}

macro_rules! operator {
    ($ty:ident) => {
        AstNodeData::Operator(Operator::$ty)
    }
}

fn is(token_type: TokenType) -> impl Fn(&TokenType) -> bool {
    move |other| *other == token_type
}

fn any(types: &[TokenType]) -> impl Fn(&TokenType) -> bool + '_ {
    |other| types.contains(other)
}

fn all_except_newline() -> impl Fn(&TokenType) -> bool {
    |ty| *ty != Newline
}

#[derive(Debug)]
pub struct ParserResult {
    pub data: ParserResultData,
    pub line_range: Range<usize>,
    pub token_range: Range<usize>,
}

#[derive(Debug)]
pub enum ParserResultData {
    Calculation(Vec<AstNode>),
    BooleanExpression {
        lhs: Vec<AstNode>,
        rhs: Vec<AstNode>,
        operator: BooleanOperator,
    },
    VariableDefinition(String, Option<Vec<AstNode>>),
    FunctionDefinition {
        name: String,
        args: Vec<FunctionArgument>,
        ast: Option<Vec<AstNode>>,
    },
    Equation {
        lhs: Vec<AstNode>,
        rhs: Vec<AstNode>,
        is_question_mark_in_lhs: bool,
        output_variable: Option<(String, SourceRange)>,
    },
}

impl std::fmt::Display for ParserResultData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserResultData::Calculation(_) => write!(f, "Calculation"),
            ParserResultData::BooleanExpression { .. } => write!(f, "Boolean Expression"),
            ParserResultData::VariableDefinition(..) => write!(f, "Variable Definition"),
            ParserResultData::FunctionDefinition { .. } => write!(f, "Function Definition"),
            ParserResultData::Equation { .. } => write!(f, "Equation"),
        }
    }
}

macro_rules! result {
    ($name:ident($($arg:expr),+) with tr: $token_range:expr, lr: $line_range:expr) => {
        ParserResult {
            data: ParserResultData::$name($($arg),+),
            line_range: $line_range,
            token_range: $token_range,
        }
    };
    ($name:ident { $($arg_name:ident: $arg_value:expr),+ } with tr: $token_range:expr, lr: $line_range:expr) => {
        #[allow(clippy::redundant_field_names)]
        ParserResult {
            data: ParserResultData::$name { $($arg_name: $arg_value),+ },
            line_range: $line_range,
            token_range: $token_range,
        }
    }
}

#[derive(Debug)]
enum DefinitionInfo {
    Variable(String),
    Function(String, Vec<FunctionArgument>),
}

#[derive(Debug, Clone)]
pub struct QuestionMarkInfo {
    is_in_lhs: bool,
    variable: Option<(String, SourceRange)>,
}

#[derive(Debug, Clone, Copy)]
pub struct BooleanOperatorInfo {
    operator: BooleanOperator,
    ast_index: usize,
    token_index: usize,
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    nesting_level: usize,
    context: Context,
    extra_allowed_variables: Option<Vec<String>>,
    allow_question_mark: bool,
    question_mark: Option<QuestionMarkInfo>,
    boolean_operator: Option<BooleanOperatorInfo>,
    did_find_equals_sign: bool,
    skip_newline_stack: Vec<bool>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(
        tokens: &'a [Token],
        context: Context,
        nesting_level: usize,
        allow_question_mark: bool,
        question_mark: Option<QuestionMarkInfo>,
        boolean_operator: Option<BooleanOperatorInfo>,
        skip_newline_stack: Vec<bool>,
    ) -> Self {
        Self {
            tokens,
            index: 0,
            nesting_level,
            context,
            extra_allowed_variables: None,
            allow_question_mark,
            question_mark,
            did_find_equals_sign: false,
            boolean_operator,
            skip_newline_stack,
        }
    }

    pub(crate) fn from_tokens(tokens: &'a [Token], context: Context) -> Self {
        Self::new(
            tokens,
            context,
            0,
            true,
            None,
            None,
            vec![false],
        )
    }

    fn new_sub(&self, tokens: &'a [Token], allow_question_mark: bool) -> Self {
        let mut skip_newline_stack = self.skip_newline_stack.clone();
        skip_newline_stack.push(true);
        Self {
            tokens,
            index: 0,
            context: self.context.clone(),
            nesting_level: self.nesting_level + 1,
            allow_question_mark,
            did_find_equals_sign: false,
            question_mark: self.question_mark.clone(),
            extra_allowed_variables: self.extra_allowed_variables.clone(),
            skip_newline_stack,
            ..*self
        }
    }

    pub fn set_extra_allowed_variables(&mut self, variables: Vec<String>) {
        self.extra_allowed_variables = Some(variables);
    }

    fn skip_newline(&self) -> bool { *self.skip_newline_stack.last().unwrap() }

    fn set_skip_newline(&mut self, skip: bool) {
        self.skip_newline_stack = vec![skip];
    }

    fn push_skip_newline(&mut self, skip: bool) {
        self.skip_newline_stack.push(skip);
    }

    fn pop_skip_newline(&mut self) {
        if self.skip_newline_stack.len() == 1 {
            panic!("skip_newline_stack cannot be empty!");
        }
        self.skip_newline_stack.pop();
    }

    fn error_range_at_end(&self) -> SourceRange {
        let Some(mut last_range) = self.tokens.last().map(|token| token.range) else {
            return SourceRange::empty();
        };
        last_range.start_char = last_range.end_char - 1;
        last_range
    }

    fn accept<Predicate>(&mut self, predicate: Predicate, error_type: ErrorType) -> Result<&Token>
        where Predicate: Fn(&TokenType) -> bool {
        let token = self.tokens.get(self.index).ok_or_else(|| error_type.clone().with(self.error_range_at_end()))?;
        if predicate(&token.ty) {
            self.index += 1;
            Ok(token)
        } else {
            if self.skip_newline() && token.ty == Newline {
                let token = self.tokens.get(self.index + 1).ok_or_else(|| error_type.clone().with(self.error_range_at_end()))?;
                if predicate(&token.ty) {
                    self.index += 2;
                    return Ok(token);
                }
            }

            Err(error_type.with(token.range))
        }
    }

    fn try_accept<Predicate>(&mut self, predicate: Predicate) -> Option<&Token>
        where Predicate: Fn(&TokenType) -> bool {
        let token = &self.tokens.get(self.index)?;
        if predicate(&token.ty) {
            self.index += 1;
            Some(token)
        } else {
            if self.skip_newline() && token.ty == Newline {
                let token = self.tokens.get(self.index + 1)?;
                if predicate(&token.ty) {
                    self.index += 2;
                    return Some(token);
                }
            }
            None
        }
    }

    /// Get the next token, if it matches `predicate`
    fn peek<Predicate>(&self, predicate: Predicate) -> Option<&Token>
        where Predicate: Fn(&TokenType) -> bool {
        let token = self.tokens.get(self.index)?;
        if predicate(&token.ty) {
            Some(token)
        } else {
            if self.skip_newline() && token.ty == Newline {
                let token = self.tokens.get(self.index + 1)?;
                if predicate(&token.ty) { return Some(token); }
            }
            None
        }
    }

    fn try_accept_variable_definition_head(&mut self, expect_definition_sign: bool) -> Option<Result<String>> {
        let identifier = self.try_accept(is(Identifier))?;
        let identifier_range = identifier.range;
        let name = identifier.text.clone();

        if expect_definition_sign && self.try_accept(is(DefinitionSign)).is_none() {
            self.index = self.index.saturating_sub(1);
            return None;
        }

        if self.context.borrow().env.is_standard_variable(&name) {
            return Some(Err(ReservedVariable(name).with(identifier_range)));
        }

        Some(Ok(name))
    }

    /// Tries to accept a function definition head. To do this, the function tries to parse the
    /// entire definition head while storing the first error it encountered. If it does not find
    /// a definition sign, it returns None, discards the error and resets self.index. If it finds
    /// a definition sign, it releases the stored error if there is one, and otherwise returns the
    /// parsed data.
    fn try_accept_function_definition_head(&mut self, expect_definition_sign: bool) -> Option<Result<(String, Vec<FunctionArgument>)>> {
        let start_index = self.index;

        let mut first_error: Option<Error> = None;
        /// Runs the input, and returns the value if it returns Ok.
        /// If the expression returns an Err, the macro stores the error in first_error, if it
        /// is None and returns a default value.
        macro_rules! try_token {
            ($input:expr, $var:ident) => {
                match $input {
                    Ok(v) => v,
                    Err(e) => {
                        if first_error.is_none() {
                            first_error = Some(e.clone());
                        }
                        &$var
                    }
                }
            };
            ($input:expr) => {
                if let Err(e) = $input {
                    if first_error.is_none() {
                        first_error = Some(e.clone());
                    }
                }
            }
        }

        let _identifier = Token::empty_from_type(Identifier);
        let identifier = try_token!(self.accept(is(Identifier), ExpectedIdentifier), _identifier);
        let identifier_range = identifier.range;
        let name = identifier.text.clone();

        if self.context.borrow().env.is_standard_function(&name) && first_error.is_none() {
            first_error = Some(ReservedFunction(name.clone()).with(identifier_range));
        }

        try_token!(self.accept(is(OpenBracket), ExpectedOpenBracket));
        self.push_skip_newline(true);

        let first_arg = try_token!(self.accept(is(Identifier), ExpectedIdentifier), _identifier);
        let first_arg_text = first_arg.text.clone();
        let first_arg_range = first_arg.range;

        let unit = self.try_accept_unit()
            .and_then(|unit| {
                unit.map_or_else(|e| {
                    first_error = Some(e);
                    None
                }, Some)
            });

        let mut args = vec![(first_arg_text, unit, first_arg_range)];

        let _close_bracket = Token::empty_from_type(CloseBracket);
        loop {
            let next = try_token!(self.accept(any(&[Comma, CloseBracket]), ExpectedComma), _close_bracket);
            match next.ty {
                Comma => {
                    let next_arg = try_token!(self.accept(is(Identifier), ExpectedIdentifier), _identifier);
                    let next_arg_text = next_arg.text.clone();
                    let next_arg_range = next_arg.range;

                    if first_error.is_none() {
                        if let Some((_, _, first_occurrence_range)) = args.iter().find(|(arg, ..)| *arg == next_arg_text) {
                            first_error = Some(DuplicateArgument(next_arg.text.clone()).with_multiple(vec![next_arg.range, *first_occurrence_range]));
                            continue;
                        }
                    }

                    let unit = self.try_accept_unit()
                        .and_then(|unit| {
                            unit.map_or_else(|e| {
                                first_error = Some(e);
                                None
                            }, Some)
                        });
                    args.push((next_arg_text, unit, next_arg_range));
                }
                CloseBracket => {
                    self.pop_skip_newline();
                    break;
                }
                _ => unreachable!(),
            }
        }

        let def_sign_res = self.accept(is(DefinitionSign), UnexpectedElements);
        if def_sign_res.is_err() && expect_definition_sign {
            self.index = start_index;
            None
        } else {
            Some(
                if let Some(e) = first_error {
                    self.index = start_index;
                    Err(e)
                } else {
                    let args = args.into_iter()
                        .map(|(arg, unit, _)| (arg, unit))
                        .collect::<Vec<_>>();
                    Ok((name, args))
                }
            )
        }
    }

    fn accept_definition_info(&mut self, expect_definition_sign: bool) -> Result<Option<DefinitionInfo>> {
        if let Some(name) = self.try_accept_variable_definition_head(expect_definition_sign) {
            Ok(Some(DefinitionInfo::Variable(name?)))
        } else if let Some(result) = self.try_accept_function_definition_head(expect_definition_sign) {
            let (name, args) = result?;
            Ok(Some(DefinitionInfo::Function(name, args)))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn next(&mut self) -> Option<Result<ParserResult>> {
        self.set_skip_newline(false);
        while self.index < self.tokens.len() {
            if self.try_accept(is(Newline)).is_some() { continue; }
            break;
        }
        if self.index >= self.tokens.len() { return None; }

        let new = self.parse_single();
        if new.is_err() {
            // Skip to next line since we can't recover from errors (yet)
            while self.try_accept(all_except_newline()).is_some() {}
        }
        Some(new)
    }

    fn current_tokens_end_line(&self) -> usize {
        self.tokens.get(self.index)
            .map(|t| t.range.end_line)
            .unwrap_or_else(|| self.tokens.last().unwrap().range.end_line)
    }

    fn has_reached_end(&self) -> bool {
        self.index >= self.tokens.len() || (!self.skip_newline() && self.tokens[self.index].ty == Newline)
    }

    // Extract into separate function to allow the use of the error!() macro and easier handling
    // with `return`.
    pub(crate) fn parse_single(&mut self) -> Result<ParserResult> {
        let start_line = self.tokens.get(self.index).map(|t| t.range.start_line).unwrap_or_default();
        let start_token_index = self.index;

        let mut definition_info = self.accept_definition_info(true)?;
        // let mut definition_info = None;

        if self.has_reached_end() {
            let line_range = start_line..self.current_tokens_end_line();
            let token_range = start_token_index..self.index;
            return match definition_info {
                Some(DefinitionInfo::Variable(name)) =>
                    Ok(result!(VariableDefinition(name, None) with tr: token_range, lr: line_range)),
                Some(DefinitionInfo::Function(name, args)) =>
                    Ok(result!(FunctionDefinition { name: name, args: args, ast: None } with tr: token_range, lr: line_range)),
                None => error!(ExpectedElements: self.error_range_at_end()),
            };
        }

        if let Some(DefinitionInfo::Function(_, args)) = &definition_info {
            let args = args.iter().map(|arg| arg.0.clone()).collect::<Vec<_>>();
            self.set_extra_allowed_variables(args);
        }

        #[derive(Default)]
        struct GroupStackEntry {
            ast: Vec<AstNode>,
            modifiers: Vec<AstNodeModifier>,
            start_range: SourceRange,
        }

        impl GroupStackEntry {
            pub fn new(modifiers: Vec<AstNodeModifier>, range: SourceRange) -> Self {
                Self {
                    modifiers,
                    start_range: range,
                    ..Default::default()
                }
            }
        }

        let mut group_stack: Vec<GroupStackEntry> = vec![GroupStackEntry::default()];

        /// Helper to get the current AST
        macro_rules! ast {
            () => { group_stack.last_mut().unwrap().ast }
        }

        // If there is a close bracket at the beginning of the line, it is an error
        if let Some(close_bracket) = self.try_accept(is(CloseBracket)) {
            error!(MissingOpeningBracket: close_bracket.range);
        }

        // Accept opening brackets at the beginning of the line
        while !self.has_reached_end() {
            let start_i = self.index;
            let modifiers = self.accept_prefix_modifiers();

            if let Some(open_bracket) = self.try_accept(is(OpenBracket)) {
                group_stack.push(GroupStackEntry::new(modifiers, open_bracket.range));
                self.set_skip_newline(true);
                self.nesting_level += 1;
            } else {
                self.index = start_i;
                break;
            }
        }

        // Check if we're at the end
        if self.has_reached_end() && group_stack.len() > 1 {
            error!(MissingClosingBracket: group_stack.last().unwrap().start_range);
        }

        // Allow empty brackets, which is handled at the start of the while loop below
        // The accept_expression() function will exit immediately in this case, meaning it will
        // be handled further down.
        // NOTE: () = 1
        if self.peek(is(CloseBracket)).is_none() {
            ast!().push(self.accept_number()?);
        }

        while !self.has_reached_end() {
            self.set_skip_newline(group_stack.len() > 1);
            self.accept_expression(&mut ast!())?;
            if self.has_reached_end() { break; }

            // The accept_expression() function will exit on these tokens:
            let token = self.try_accept(any(&[CloseBracket, PostfixDefinitionSign]));
            match token.map(|t| t.ty) {
                Some(CloseBracket) => {
                    if group_stack.len() == 1 {
                        error!(MissingOpeningBracket: token.unwrap().range);
                    }

                    let GroupStackEntry {
                        ast: group,
                        mut modifiers,
                        start_range: open_bracket_range,
                    } = group_stack.pop().unwrap();
                    let range = open_bracket_range.extend(token.unwrap().range);

                    modifiers.append(&mut self.accept_suffix_modifiers());
                    let unit = self.try_accept_unit().transpose()?;

                    let mut node = if group.is_empty() {
                        AstNode::new(AstNodeData::Literal(1.0), range)
                    } else {
                        AstNode::new(AstNodeData::Group(group), range)
                    };

                    node.modifiers = modifiers;
                    node.unit = unit;

                    ast!().push(node);
                    self.nesting_level -= 1;
                    // if group_stack.len() == 1 && self.try_accept(is(Newline)).is_some() {
                    //     break;
                    // }
                }
                Some(PostfixDefinitionSign) => {
                    if definition_info.is_some() {
                        error!(DisallowedPostfixDefinitionNormalDefinitionWasUsed: token.unwrap().range);
                    }

                    let error_range = SourceRange::empty().extend(self.tokens[self.index - 2].range);
                    if self.boolean_operator.is_some() {
                        error!(ExpectedExpression("Boolean Expression".to_string()): error_range);
                    }
                    if self.question_mark.is_some() {
                        error!(ExpectedExpression("Equation".to_string()): error_range);
                    }

                    definition_info = self.accept_definition_info(false)?;
                    if definition_info.is_none() {
                        error!(ExpectedIdentifier: self.tokens[self.index - 1].range);
                    }
                    if !self.has_reached_end() {
                        error!(UnexpectedElements: self.tokens[self.index].range);
                    }
                }
                _ => {
                    // The function exited because of an open bracket. We can't directly accept()
                    // it though, because there might be prefix modifiers.
                    let modifiers = self.accept_prefix_modifiers();
                    let open_bracket = self.accept(is(OpenBracket), ExpectedElements)?;
                    group_stack.push(GroupStackEntry::new(modifiers, open_bracket.range));
                    self.nesting_level += 1;
                    ast!().push(self.accept_number()?);
                }
            }
        }

        if group_stack.len() > 1 {
            error!(MissingClosingBracket: group_stack.last().unwrap().start_range);
        }

        let GroupStackEntry { ast: result, .. } = group_stack.pop().unwrap();
        let line_range = start_line..self.current_tokens_end_line();
        let token_range = start_token_index..self.index;

        if let Some(BooleanOperatorInfo {
                        operator,
                        ast_index,
                        token_index,
                    }) = self.boolean_operator {
            if definition_info.is_some() {
                error!(DisallowedBooleanOperator: self.tokens[token_index].range);
            }

            let (lhs, rhs) = result.split_at(ast_index);
            if let Some(info) = std::mem::take(&mut self.question_mark) {
                Ok(result!(Equation {
                    lhs: lhs.to_vec(),
                    rhs: rhs.to_vec(),
                    is_question_mark_in_lhs: info.is_in_lhs,
                    output_variable: info.variable
                } with tr: token_range, lr: line_range))
            } else {
                Ok(result!(BooleanExpression {
                    lhs: lhs.to_vec(),
                    rhs: rhs.to_vec(),
                    operator: operator
                } with tr: token_range, lr: line_range))
            }
        } else {
            match definition_info {
                Some(DefinitionInfo::Variable(name)) =>
                    Ok(result!(VariableDefinition(name, Some(result)) with tr: token_range, lr: line_range)),
                Some(DefinitionInfo::Function(name, args)) =>
                    Ok(result!(FunctionDefinition { name: name, args: args, ast: Some(result) } with tr: token_range, lr: line_range)),
                None => {
                    if self.nesting_level == 0 && self.question_mark.is_some() {
                        let mut range = self.tokens.last().unwrap().range;
                        range.end_char = range.end_char.saturating_sub(1);
                        error!(MissingEqualsSign: range);
                    }

                    Ok(result!(Calculation(result) with tr: token_range, lr: line_range))
                }
            }
        }
    }

    fn accept_expression(&mut self, ast: &mut Vec<AstNode>) -> Result<()> {
        while !self.has_reached_end() {
            if self.peek(any(&[CloseBracket, PostfixDefinitionSign])).is_some() {
                break;
            }

            self.try_accept_line_continuation()?;

            match self.accept_operator() {
                Ok(op) => {
                    let AstNodeData::Operator(operator) = op.data else { unreachable!(); };
                    // RHS of `in` (unit / format)
                    if operator == Operator::In {
                        let start = self.tokens.get(self.index).map(|t| t.range);

                        let mut found_rhs = false;
                        if let Some(format) = self.try_accept(|ty| ty.is_format()) {
                            let format = match format.ty {
                                Decimal => Format::Decimal,
                                Binary => Format::Binary,
                                Hex => Format::Hex,
                                Scientific => Format::Scientific,
                                _ => unreachable!(),
                            };
                            ast.last_mut().unwrap().format = format;
                            found_rhs = true;
                        }

                        if let Some(unit) = self.try_accept_unit() {
                            let end = self.tokens[self.index - 1].range;
                            let unit = unit?;
                            ast.push(op);
                            ast.push(AstNode::new(
                                AstNodeData::Unit(unit),
                                start.unwrap().extend(end),
                            ));
                            found_rhs = true;
                        }

                        if found_rhs { continue; }

                        let last = self.tokens.last().unwrap();
                        let range = if last.ty == In {
                            let mut range = last.range;
                            range.end_char = range.end_char.saturating_sub(1);
                            range
                        } else {
                            last.range
                        };
                        error!(ExpectedUnit: range);
                    }

                    ast.push(op);
                }
                Err(error) => {
                    // Try to infer multiplication
                    if self.peek(any(&[OpenBracket, Identifier])).is_some() {
                        ast.push(AstNode::new(AstNodeData::Operator(Operator::Multiply), SourceRange::empty()));
                    } else if let Some((op, range)) = self.try_accept_boolean_operator() {
                        if self.nesting_level != 0 {
                            error!(UnexpectedBooleanOperator: range);
                        } else if self.boolean_operator.is_some() {
                            error!(UnexpectedSecondBooleanOperator: range);
                        }

                        if op == BooleanOperator::Equal {
                            self.did_find_equals_sign = true;
                        }

                        self.boolean_operator = Some(BooleanOperatorInfo {
                            operator: op,
                            ast_index: ast.len(),
                            token_index: self.index - 1,
                        });
                    } else {
                        return Err(error);
                    }
                }
            }

            self.try_accept_line_continuation()?;

            // Check if we need to exit because of an open bracket. To do this, we need to try to
            // accept prefix modifiers and check after them. We then reset our index back to where
            // we were before.
            {
                let start_i = self.index;
                self.accept_prefix_modifiers();
                if self.peek(is(OpenBracket)).is_some() {
                    self.index = start_i;
                    break;
                } else {
                    self.index = start_i
                }
            }

            // Allow empty groups
            // NOTE: () = 1
            if self.peek(is(CloseBracket)).is_some() &&
                self.tokens[self.index - 1].ty == OpenBracket { break; }
            ast.push(self.accept_number()?);
        }

        Ok(())
    }

    fn try_accept_line_continuation(&mut self) -> Result<()> {
        self.push_skip_newline(false);
        if self.try_accept(is(LineContinuation)).is_some() {
            self.accept(is(Newline), ExpectedNewline)?;
        }
        self.pop_skip_newline();
        Ok(())
    }

    fn try_accept_boolean_operator(&mut self) -> Option<(BooleanOperator, SourceRange)> {
        let op = self.try_accept(|ty| ty.is_boolean_operator())?;

        let range = op.range;
        let op = match op.ty {
            EqualsSign => BooleanOperator::Equal,
            NotEqualsSign => BooleanOperator::NotEqual,
            GreaterThan => BooleanOperator::GreaterThan,
            GreaterThanEqual => BooleanOperator::GreaterThanEqual,
            LessThan => BooleanOperator::LessThan,
            LessThanEqual => BooleanOperator::LessThanEqual,
            _ => unreachable!(),
        };

        Some((op, range))
    }

    fn accept_number(&mut self) -> Result<AstNode> {
        let next = self.peek(all_except_newline()).map(|t| t.ty);

        match next {
            Some(OpenCurlyBracket) => self.accept_object(),
            Some(OpenSquareBracket) => self.accept_vector(),
            _ => {
                let mut modifiers = self.accept_prefix_modifiers();

                let next = self.peek(all_except_newline());
                let mut number = match next.map(|token| token.ty) {
                    Some(ty) if ty.is_literal() => self.accept_literal()?,
                    Some(Identifier) => self.accept_identifier()?,
                    Some(QuestionMark) => self.accept_question_mark()?,
                    Some(_) => error!(ExpectedNumber: next.unwrap().range),
                    None => error!(ExpectedNumber: self.error_range_at_end()),
                };

                modifiers.append(&mut self.accept_suffix_modifiers());
                number.modifiers.append(&mut modifiers);

                if let Some(token) = self.peek(is(Identifier)) {
                    if token.text != "e" && token.text != "E" &&
                        (self.context.borrow().env.is_valid_variable(&token.text) ||
                            self.context.borrow().env.is_valid_function(&token.text)) {
                        return Ok(number);
                    }
                }

                if let Some(identifier) = self.peek(is(Identifier)) {
                    if identifier.text == "e" || identifier.text == "E" {
                        self.index += 1;

                        let mut modifiers = vec![];
                        while let Some(sign) = self.try_accept(any(&[Plus, Minus])) {
                            modifiers.push(match sign.ty {
                                Plus => AstNodeModifier::Plus,
                                Minus => AstNodeModifier::Minus,
                                _ => unreachable!(),
                            });
                        }

                        if let Ok(mut exponent) = self.accept_literal() {
                            exponent.modifiers = modifiers;

                            if let Some(unit) = self.try_accept_unit() {
                                number.unit = Some(unit?);
                            } else if let Some(power) = self.try_accept_unit_prefix() {
                                number.modifiers.push(AstNodeModifier::Power(power));
                            }

                            let range = number.range;
                            let group = vec![
                                number,
                                AstNode::new(AstNodeData::Operator(Operator::Multiply), range),
                                AstNode::new(AstNodeData::Literal(10.0), range),
                                AstNode::new(AstNodeData::Operator(Operator::Exponentiation), range),
                                exponent,
                            ];

                            return Ok(AstNode::new(AstNodeData::Group(group), range));
                        } else {
                            // If this isn't a scientific notation (i.e. there is no exponent after the "e"),
                            // revert back to pointing at the "e", so that it can be handled later.
                            self.index -= 1;
                        }
                    }
                }

                if let Some(unit) = self.try_accept_unit() {
                    number.unit = Some(unit?);
                } else if let Some(power) = self.try_accept_unit_prefix() {
                    number.modifiers.push(AstNodeModifier::Power(power));
                }

                Ok(number)
            }
        }
    }

    fn accept_prefix_modifiers(&mut self) -> Vec<AstNodeModifier> {
        let mut result = Vec::new();
        while let Some(token) = self.try_accept(any(&[ExclamationMark, Plus, Minus])) {
            let modifier = match token.ty {
                ExclamationMark => AstNodeModifier::BitwiseNot,
                Plus => AstNodeModifier::Plus,
                Minus => AstNodeModifier::Minus,
                _ => unreachable!(),
            };
            result.push(modifier);
        }
        result
    }

    fn accept_suffix_modifiers(&mut self) -> Vec<AstNodeModifier> {
        let mut result = Vec::new();
        while let Some(modifier) = self.try_accept(any(&[ExclamationMark, PercentSign])) {
            let modifier = match modifier.ty {
                ExclamationMark => AstNodeModifier::Factorial,
                PercentSign => AstNodeModifier::Percent,
                _ => unreachable!(),
            };
            result.push(modifier);
        }
        result
    }

    fn accept_literal(&mut self) -> Result<AstNode> {
        let literal = self.accept(
            |ty| ty.is_literal(),
            ExpectedNumber,
        )?;

        let text = literal.text.chars().filter(|c| *c != '_').collect::<String>();
        let data = match literal.ty {
            DecimalLiteral => {
                let number = match text.parse::<f64>() {
                    Ok(n) => n,
                    Err(e) => error!(InvalidNumber(e.to_string()): literal.range),
                };
                AstNodeData::Literal(number)
            }
            HexLiteral => AstNodeData::Literal(parse_f64_radix!(text, 16, literal.range)),
            BinaryLiteral => AstNodeData::Literal(parse_f64_radix!(text, 2, literal.range)),
            _ => unreachable!(),
        };

        Ok(AstNode::new(data, literal.range))
    }

    fn accept_question_mark(&mut self) -> Result<AstNode> {
        let token = self.accept(is(QuestionMark), ExpectedQuestionMark)?;
        let range = token.range;
        if self.question_mark.is_some() {
            error!(UnexpectedQuestionMark: range);
        } else if !self.allow_question_mark {
            error!(QuestionMarkNotAllowed: range);
        }

        self.question_mark = Some(QuestionMarkInfo {
            variable: None,
            is_in_lhs: !self.did_find_equals_sign,
        });
        Ok(AstNode::new(AstNodeData::QuestionMark, range))
    }

    fn try_accept_unit_prefix(&mut self) -> Option<i32> {
        let Some(prefix) = self.peek(is(Identifier)) else { return None; };
        if prefix.text.len() > 1 { return None; }
        let char = prefix.text.chars().next().unwrap();
        if let Some(power) = get_prefix_power(char) {
            self.index += 1;
            Some(power)
        } else {
            None
        }
    }

    fn try_accept_unit(&mut self) -> Option<Result<Unit>> {
        if self.peek(is(OpenSquareBracket)).is_some() {
            return self.accept_complex_unit().map(|res| res.map(|(unit, ..)| unit));
        }

        let Some(numerator) = self.try_accept_single_unit() else { return None; };
        let numerator = match numerator {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };

        'blk: {
            if self.peek(is(Divide)).is_some() {
                self.index += 1;
                if let Some(identifier) = self.peek(is(Identifier)) {
                    if self.context.borrow().env.is_valid_variable(&identifier.text) {
                        self.index -= 1;
                        break 'blk;
                    }
                }

                if let Some(denominator) = self.try_accept_single_unit() {
                    return match denominator {
                        Ok(denom) => {
                            // e.g. `h/h`, which is equal to 1
                            if numerator == denom { return None; }

                            let mut result = Unit::Fraction(Box::new(numerator), Box::new(denom));
                            if !result.simplify() { return None; }
                            Some(Ok(result))
                        }
                        Err(e) => Some(Err(e)),
                    };
                } else {
                    self.index -= 1;
                }
            }
        }

        Some(Ok(numerator))
    }

    fn accept_complex_unit(&mut self) -> Option<Result<(Unit, SourceRange)>> {
        fn accept_complex_unit_impl(this: &mut Parser) -> Result<(Unit, SourceRange)> {
            let open_bracket = this.accept(is(OpenSquareBracket), ExpectedOpenSquareBracket)?;
            let range_start = open_bracket.range;
            this.push_skip_newline(true);

            let (mut units, _) = this.next_units(0)?;
            if units.is_empty() {
                this.pop_skip_newline();
                error!(ExpectedUnit: this.peek(all_except_newline()).map(|t| t.range).unwrap_or_else(|| this.error_range_at_end()));
            }
            let unit = if units.len() == 1 { units.remove(0) } else { Unit::Product(units) };

            let close_bracket = this.accept(is(CloseSquareBracket), ExpectedCloseSquareBracket)?;
            let close_bracket_range = close_bracket.range;
            this.pop_skip_newline();
            Ok((unit, range_start.extend(close_bracket_range)))
        }

        let res = accept_complex_unit_impl(self);
        if let Err(e) = res { return Some(Err(e)); }
        let (mut unit, range) = res.unwrap();

        if !unit.simplify() { return None; }

        Some(Ok((unit, range)))
    }

    fn next_units(&mut self, nesting_level: usize) -> Result<(Vec<Unit>, SourceRange)> {
        let mut result = vec![];
        let mut result_range = SourceRange::empty();

        let mut numerator: Option<Unit> = None;

        while self.peek(is(CloseSquareBracket)).is_none() {
            let Some(token) = self.peek(all_except_newline()) else {
                error!(ExpectedElements: self.error_range_at_end());
            };
            let token_range = token.range;
            if result_range.start_char == 0 {
                result_range = token_range;
            }

            match token.ty {
                OpenBracket => 'blk: {
                    self.index += 1;
                    let (mut units, range) = self.next_units(nesting_level + 1)?;
                    if units.is_empty() {
                        error!(ExpectedElements: token_range.extend(range));
                    }
                    result_range = result_range.extend(range);

                    let unit = if units.len() == 1 { units.remove(0) } else { Unit::Product(units) };

                    if let Some(numerator) = numerator.take() {
                        result.push(Unit::Fraction(Box::new(numerator), Box::new(unit)));
                        break 'blk;
                    }

                    if self.try_accept(is(Divide)).is_some() {
                        numerator = Some(unit);
                        break 'blk;
                    }

                    result.push(unit);
                }
                CloseBracket => {
                    self.index += 1;
                    if numerator.is_some() {
                        error!(ExpectedUnit: token_range);
                    }

                    return Ok((result, result_range));
                }
                Identifier => 'blk: {
                    let Some(unit) = self.try_accept_single_unit() else {
                        error!(ExpectedUnit: token_range);
                    };
                    let unit = unit?;
                    // try_accept_single_unit always returns a Unit::Unit, so this is safe
                    let Unit::Unit(.., range) = &unit else { unreachable!(); };
                    result_range = result_range.extend(*range);

                    if let Some(numerator) = numerator.take() {
                        result.push(Unit::Fraction(Box::new(numerator), Box::new(unit)));
                        break 'blk;
                    }

                    if self.try_accept(is(Divide)).is_some() {
                        numerator = Some(unit);
                        break 'blk;
                    }

                    result.push(unit);
                }
                _ => error!(ExpectedUnit: token_range),
            }

            self.try_accept(is(Multiply));
        }

        if nesting_level != 0 {
            self.accept(is(CloseBracket), ExpectedCloseBracket)?;
        }

        Ok((result, result_range))
    }

    fn try_accept_single_unit(&mut self) -> Option<Result<Unit>> {
        let Some(unit) = self.peek(is(Identifier)) else { return None; };
        if !is_unit_with_prefix(&unit.text) { return None; }

        let mut unit_range = unit.range;
        let unit = unit.text.clone();
        self.index += 1;

        let mut power = 1.0f64;
        if let Some(exponentiation) = self.peek(is(Exponentiation)) {
            if exponentiation.range.start_char == unit_range.end_char {
                self.index += 1;
                if let Ok(AstNode { data, range, .. }) = self.accept_literal() {
                    let AstNodeData::Literal(number) = data else { unreachable!(); };
                    power = number;
                    unit_range = unit_range.extend(range);
                } else {
                    self.index -= 1;
                }
            }
        }

        Some(Ok(Unit::Unit(unit, power, unit_range)))
    }

    fn accept_identifier(&mut self) -> Result<AstNode> {
        let identifier = self.accept(is(Identifier), ExpectedIdentifier)?;
        let name = identifier.text.clone();
        let range = identifier.range;

        if self.context.borrow().env.is_valid_variable(&name) {
            let ctx = self.context.borrow();
            let v = ctx.env.resolve_variable(&name).unwrap();
            let is_callable_object = matches!(&v.0, Value::Object(object) if object.is_callable());
            drop(ctx);

            let node = AstNode::new(AstNodeData::Identifier(name.clone()), range);

            return if is_callable_object {
                self.maybe_with_call(node, range)
            } else {
                if let Some(question_mark) = self.try_accept_question_mark_after_identifier(&name, range) {
                    return question_mark;
                }
                Ok(node)
            };
        }
        if self.extra_allowed_variables.as_ref().map_or(false, |vars| vars.contains(&name)) {
            let node = AstNode::new(AstNodeData::Identifier(name.clone()), range);
            if let Some(question_mark) = self.try_accept_question_mark_after_identifier(&name, range) {
                return question_mark;
            }
            return Ok(node);
        } else if self.context.borrow().env.is_valid_function(&name) {
            let open_bracket_token = self.peek(is(OpenBracket));
            let open_bracket_range = open_bracket_token.map(|t| t.range).unwrap_or_default();
            let arguments = self.accept_call_arguments(&name)?;
            let close_bracket_range = self.tokens[self.index - 1].range;

            let args_range = SourceRange::new(
                open_bracket_range.start_line,
                open_bracket_range.end_char,
                close_bracket_range.start_line,
                close_bracket_range.start_char,
            );

            return Ok(AstNode::new(AstNodeData::Group(vec![
                AstNode::new(AstNodeData::Identifier(name), range),
                AstNode::new(AstNodeData::Operator(Operator::Call), open_bracket_range),
                AstNode::new(AstNodeData::Arguments(arguments), args_range),
            ]), range.extend(close_bracket_range)));
        }

        if let Some(question_mark) = self.try_accept_question_mark_after_identifier(&name, range) {
            return question_mark;
        }

        error!(UnknownIdentifier(name): range)
    }

    fn try_accept_question_mark_after_identifier(&mut self, identifier: &str, range: SourceRange) -> Option<Result<AstNode>> {
        self.peek(is(QuestionMark))?;
        let question_mark = match self.accept_question_mark() {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        if let Some(info) = self.question_mark.as_mut() {
            info.variable = Some((identifier.to_string(), range));
        }
        Some(Ok(question_mark))
    }

    fn accept_object(&mut self) -> Result<AstNode> {
        let open_bracket = self.accept(is(OpenCurlyBracket), ExpectedOpenCurlyBracket)?;
        let full_range_start = open_bracket.range;
        self.push_skip_newline(true);

        let name = self.accept(is(Identifier), ExpectedObjectName)?;
        if !CalculatorObject::is_valid_object(&name.text) {
            error!(UnknownObject(name.text.clone()): name.range);
        }
        let name = (name.text.clone(), name.range);

        let range_start = self.index;
        let mut args = vec![];
        while !self.has_reached_end() {
            let Some(token) = self.peek(all_except_newline()) else {
                error!(ExpectedCloseCurlyBracket: self.error_range_at_end());
            };
            match token.ty {
                CloseCurlyBracket => break,
                OpenSquareBracket => {
                    let ast = self.accept_object_arguments()?;
                    let range = ast.first().unwrap().range.extend(ast.last().unwrap().range);
                    args.push(ObjectArgument::Ast(ast, range));
                }
                CloseSquareBracket => error!(UnexpectedCloseBracket: token.range),
                ObjectArgs => {
                    let token = self.accept(is(ObjectArgs), Nothing).unwrap();
                    args.push(ObjectArgument::String(token.text.clone(), token.range))
                }
                _ => error!(InvalidToken: token.range),
            }
        }

        let close_bracket = self.accept(is(CloseCurlyBracket), ExpectedCloseCurlyBracket)?;
        let close_bracket_range = close_bracket.range;
        self.pop_skip_newline();

        if self.index - 1 == range_start {
            error!(ExpectedElements: close_bracket_range);
        }

        let object = CalculatorObject::parse(
            name,
            args,
            self.context.clone(),
            full_range_start.extend(close_bracket_range),
        )?;

        let is_callable = object.is_callable();
        let object_node = AstNode::new(AstNodeData::Object(object), full_range_start.extend(close_bracket_range));
        if is_callable { self.maybe_with_call(object_node, full_range_start) } else { Ok(object_node) }
    }

    fn accept_object_arguments(&mut self) -> Result<Vec<AstNode>> {
        let open_bracket = self.accept(is(OpenSquareBracket), ExpectedOpenSquareBracket)?;
        let open_bracket_range = open_bracket.range;

        let mut nesting_level = 1usize;
        let start = self.index;
        while !self.has_reached_end() {
            let Some(token) = self.try_accept(all_except_newline()) else { continue; };
            match token.ty {
                OpenSquareBracket => nesting_level += 1,
                CloseSquareBracket => {
                    nesting_level -= 1;
                    if nesting_level == 0 { break; }
                }
                _ => {}
            }
        }

        if nesting_level != 0 {
            error!(MissingClosingBracket: open_bracket_range);
        }

        let tokens = &self.tokens[start..self.index - 1];
        let mut parser = self.new_sub(tokens, false);
        let ParserResultData::Calculation(ast) = parser.parse_single()?.data else { unreachable!(); };
        self.question_mark = parser.question_mark;
        Ok(ast)
    }

    fn accept_vector(&mut self) -> Result<AstNode> {
        let opening_bracket = self.accept(is(OpenSquareBracket), ExpectedOpenSquareBracket)?;
        let open_bracket_range = opening_bracket.range;
        self.push_skip_newline(true);

        let tokens = self.accept_separated(open_bracket_range, Semicolon, CloseSquareBracket)?;
        self.pop_skip_newline();

        let full_range = open_bracket_range.extend(self.tokens[self.index - 1].range);

        let mut numbers = vec![];
        for tokens in tokens {
            // parse
            let mut parser = self.new_sub(tokens, false);
            let ParserResultData::Calculation(ast) = parser.parse_single()?.data else { unreachable!(); };
            self.question_mark = parser.question_mark;

            // evaluate
            let _full_range = crate::engine::full_range(&ast);
            let Ok(crate::engine::NumberValue { number, .. }) =
                Engine::evaluate_to_number(ast, self.context.clone())
                else { error!(ExpectedNumber: _full_range); };
            numbers.push(number);
        }

        let vector = AstNode::new(AstNodeData::Object(CalculatorObject::Vector(Vector { numbers })), full_range);
        self.maybe_with_call(vector, full_range)
    }

    fn accept_call_arguments(&mut self, function_name: &str) -> Result<Vec<Vec<AstNode>>> {
        let open_bracket_token = self.accept(is(OpenBracket), MissingOpeningBracket)?;
        let open_bracket_range = open_bracket_token.range;
        self.push_skip_newline(true);

        let arguments = self.accept_separated(open_bracket_range, Comma, CloseBracket)?;
        self.pop_skip_newline();

        let range_end = self.tokens[self.index - 1].range;
        let full_range = open_bracket_range.extend(range_end);

        let function_args_count = self.context.borrow().env.function_argument_count(function_name).unwrap();
        if !function_args_count.is_valid_count(arguments.len()) {
            match function_args_count {
                ArgCount::Single(count) => error!(WrongNumberOfArguments(count): full_range),
                ArgCount::Multiple(options) => error!(WrongNumberOfArgumentsMultiple(options): full_range),
            }
        }

        let allow_question_mark = !self.context.borrow().env.is_standard_function(function_name);
        self.parse_arguments(arguments, allow_question_mark)
    }

    fn maybe_with_call(&mut self, node: AstNode, range_start: SourceRange) -> Result<AstNode> {
        if let Some(open_bracket) = self.try_accept(is(OpenBracket)) {
            let open_bracket_range = open_bracket.range;
            self.push_skip_newline(true);
            let tokens = self.accept_separated(open_bracket_range, Comma, CloseBracket)?;
            self.pop_skip_newline();
            let args = self.parse_arguments(tokens, false)?;

            let close_bracket_range = self.tokens[self.index - 1].range;
            let args_range = SourceRange::new(
                open_bracket_range.start_line,
                open_bracket_range.end_char,
                close_bracket_range.start_line,
                close_bracket_range.start_char,
            );
            let group_range = SourceRange::new(
                range_start.start_line,
                range_start.start_char,
                close_bracket_range.start_line,
                close_bracket_range.start_char,
            );

            Ok(AstNode::new(AstNodeData::Group(vec![
                node,
                AstNode::new(AstNodeData::Operator(Operator::Call), open_bracket_range),
                AstNode::new(AstNodeData::Arguments(args), args_range),
            ]), group_range))
        } else {
            Ok(node)
        }
    }

    fn parse_arguments(&mut self, arguments: Vec<&'a [Token]>, allow_question_mark: bool) -> Result<Vec<Vec<AstNode>>> {
        let mut result = Vec::new();
        for tokens in arguments {
            let mut parser = self.new_sub(tokens, allow_question_mark);
            let ParserResultData::Calculation(ast) = parser.parse_single()?.data else { unreachable!(); };
            self.question_mark = parser.question_mark;
            result.push(ast);
        }
        Ok(result)
    }

    fn accept_separated<'b>(&mut self, open_bracket_range: SourceRange, separator: TokenType, end: TokenType) -> Result<Vec<&'b [Token]>>
        where 'a: 'b {
        let mut tokens: Vec<&'b [Token]> = vec![];

        let mut nesting_level = 1usize;
        let mut argument_start = self.index;
        while !self.has_reached_end() {
            let token = self.accept(all_except_newline(), ExpectedElements)?;
            let ty = token.ty;
            if ty == OpenBracket {
                nesting_level += 1
            } else if ty == CloseBracket {
                nesting_level -= 1;
                // Ignore brackets that aren't on the base level
                if nesting_level != 0 { continue; }
            } else if ty == separator && nesting_level == 1 {
                if argument_start == self.index - 1 {
                    let range = self.tokens[argument_start].range.extend(self.tokens[self.index - 1].range);
                    error!(ExpectedElements: range);
                }
                let argument = &self.tokens[argument_start..self.index - 1];
                tokens.push(argument);
                argument_start = self.index;
            }

            if ty == end {
                if end != CloseBracket {
                    nesting_level -= 1;
                }

                if argument_start == self.index - 1 {
                    let range = self.tokens[argument_start].range.extend(self.tokens[self.index - 1].range);
                    error!(ExpectedElements: range);
                }
                let argument = &self.tokens[argument_start..self.index - 1];
                tokens.push(argument);

                if nesting_level == 0 { break; }
            }
        }

        if nesting_level != 0 {
            error!(MissingClosingBracket: open_bracket_range);
        }

        Ok(tokens)
    }

    fn accept_operator(&mut self) -> Result<AstNode> {
        let operator = self.accept(
            |ty| ty.is_operator(),
            ExpectedOperator,
        )?;

        let data = match operator.ty {
            Plus => operator!(Plus),
            Minus => operator!(Minus),
            Multiply => operator!(Multiply),
            Divide => operator!(Divide),
            Exponentiation => operator!(Exponentiation),
            BitwiseAnd => operator!(BitwiseAnd),
            BitwiseOr => operator!(BitwiseOr),
            Xor => operator!(Xor),
            BitShiftLeft => operator!(BitShiftLeft),
            BitShiftRight => operator!(BitShiftRight),
            Of => operator!(Of),
            In => operator!(In),
            Modulo => operator!(Modulo),
            _ => unreachable!(),
        };

        Ok(AstNode::new(data, operator.range))
    }
}

#[cfg(test)]
mod tests {
    use crate::{Currencies, Environment, NumberValue, range, Settings};
    use crate::astgen::tokenizer::tokenize;
    use crate::engine::Value;
    use crate::environment::Variable;

    use super::*;

    macro_rules! parse {
        ($input:expr) => {
            Parser::from_tokens(&tokenize($input)?, Context {
                env: &Environment::new(),
                currencies: &Currencies::none(),
                settings: &Settings::default(),
            }).parse_single()
        };
        ($input:expr, $context:expr) => {
            Parser::from_tokens(&tokenize($input)?, $context).parse_single()
        }
    }

    macro_rules! assert_error_type {
        ($result:expr, $variant:pat) => {
            assert!(matches!($result.err().unwrap().error, $variant))
        }
    }

    macro_rules! calculation {
        ($input:expr) => {
            if let ParserResultData::Calculation(ast) = parse!($input)?.data {
                ast
            } else {
                panic!("Expected ParserResult::Calculation");
            }
        };
        ($input:expr, $context:expr) => {
            if let ParserResultData::Calculation(ast) = parse!($input, $context)?.data {
                ast
            } else {
                panic!("Expected ParserResult::Calculation");
            }
        }
    }

    macro_rules! boolean_expression {
        ($input:expr) => {
            if let ParserResultData::BooleanExpression { lhs, rhs, operator } = parse!($input)?.data {
                (lhs, rhs, operator)
            } else {
                panic!("Expected ParserResult::BooleanExpression");
            }
        }
    }

    macro_rules! var_definition {
        ($input:expr) => {
            if let ParserResultData::VariableDefinition(name, ast) = parse!($input)?.data {
                (name, ast)
            }
            else {
                panic!("Expected ParserResult::FunctionDefinition");
            }
        }
    }

    macro_rules! func_definition {
        ($input:expr) => {
            if let ParserResultData::FunctionDefinition { name, args, ast } = parse!($input)?.data {
                (name, args, ast)
            }
            else {
                panic!("Expected ParserResult::FunctionDefinition");
            }
        }
    }

    #[test]
    fn basic() -> Result<()> {
        let ast = calculation!("1 - 3 + 4 * 5 / 6");
        assert_eq!(ast.iter().map(|n| n.data.clone()).collect::<Vec<_>>(), vec![
            AstNodeData::Literal(1.0),
            AstNodeData::Operator(Operator::Minus),
            AstNodeData::Literal(3.0),
            AstNodeData::Operator(Operator::Plus),
            AstNodeData::Literal(4.0),
            AstNodeData::Operator(Operator::Multiply),
            AstNodeData::Literal(5.0),
            AstNodeData::Operator(Operator::Divide),
            AstNodeData::Literal(6.0),
        ]);
        Ok(())
    }

    #[test]
    fn modifiers() -> Result<()> {
        let ast = calculation!("2! + 3% + !4 + 3!%");
        assert_eq!(ast.iter()
                       .filter(|n| matches!(n.data, AstNodeData::Literal(_)))
                       .map(|n| &n.modifiers)
                       .collect::<Vec<_>>(), vec![
            &vec![AstNodeModifier::Factorial],
            &vec![AstNodeModifier::Percent],
            &vec![AstNodeModifier::BitwiseNot],
            &vec![AstNodeModifier::Factorial, AstNodeModifier::Percent],
        ] as Vec<&Vec<AstNodeModifier>>);
        Ok(())
    }

    #[test]
    fn extended_operators() -> Result<()> {
        let ast = calculation!("20% of 3 ^ 2 & 1 | 4");
        assert_eq!(ast.iter()
                       .filter(|n| matches!(n.data, AstNodeData::Operator(_)))
                       .map(|n| n.data.clone())
                       .collect::<Vec<_>>(), vec![
            AstNodeData::Operator(Operator::Of),
            AstNodeData::Operator(Operator::Exponentiation),
            AstNodeData::Operator(Operator::BitwiseAnd),
            AstNodeData::Operator(Operator::BitwiseOr),
        ]);
        Ok(())
    }

    #[test]
    fn groups() -> Result<()> {
        let ast = calculation!("(1 + (1 + 1)) * 2");
        assert_eq!(ast.len(), 3);
        let group = match ast[0].data {
            AstNodeData::Group(ref group) => group,
            _ => unreachable!(),
        };
        assert_eq!(group.len(), 3);
        assert!(matches!(group.last().unwrap().data, AstNodeData::Group(_)));
        Ok(())
    }

    #[test]
    fn group_with_unit() -> Result<()> {
        let ast = calculation!("(3)km");
        assert_eq!(ast.len(), 1);
        assert!(ast[0].unit.is_some());
        assert_eq!(*ast[0].unit.as_ref().unwrap(), Unit::new("km", 1.0, range!(line 0 => 3..5)));

        Ok(())
    }

    #[test]
    fn boolean_expression() -> Result<()> {
        let (lhs, rhs, operator) = boolean_expression!("3 = 3");
        assert!(matches!(lhs[0].data, AstNodeData::Literal(_)));
        assert!(matches!(rhs[0].data, AstNodeData::Literal(_)));
        assert_eq!(operator, BooleanOperator::Equal);

        let (.., operator) = boolean_expression!("3 != 3");
        assert_eq!(operator, BooleanOperator::NotEqual);
        let (.., operator) = boolean_expression!("3 > 3");
        assert_eq!(operator, BooleanOperator::GreaterThan);
        let (.., operator) = boolean_expression!("3 >= 3");
        assert_eq!(operator, BooleanOperator::GreaterThanEqual);
        let (.., operator) = boolean_expression!("3 < 3");
        assert_eq!(operator, BooleanOperator::LessThan);
        let (.., operator) = boolean_expression!("3 <= 3");
        assert_eq!(operator, BooleanOperator::LessThanEqual);
        Ok(())
    }

    #[test]
    fn variables() -> Result<()> {
        let ast = calculation!("pi");
        match ast[0].data {
            AstNodeData::Identifier(ref s) => {
                assert_eq!("pi".to_string(), *s);
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    #[test]
    fn functions() -> Result<()> {
        let ast = calculation!("sin(30)");
        assert_eq!(ast.len(), 1);
        let AstNodeData::Group(ast) = &ast.first().unwrap().data else { unreachable!(); };
        assert_eq!(ast.len(), 3);
        assert!(matches!(ast[0].data, AstNodeData::Identifier(_)));
        assert!(matches!(ast[1].data, AstNodeData::Operator(Operator::Call)));
        assert!(matches!(ast[2].data, AstNodeData::Arguments(_)));
        Ok(())
    }

    #[test]
    fn inferred_multiplication() -> Result<()> {
        let ast = calculation!("2(1)");
        assert!(ast.len() == 3 && matches!(ast[1].data, AstNodeData::Operator(Operator::Multiply)));
        let ast = calculation!("2pi");
        assert!(ast.len() == 3 && matches!(ast[1].data, AstNodeData::Operator(Operator::Multiply)));
        let ast = calculation!("2sin(30)");
        assert!(ast.len() == 3 && matches!(ast[1].data, AstNodeData::Operator(Operator::Multiply)));
        Ok(())
    }

    #[test]
    fn power_modifier() -> Result<()> {
        let ast = calculation!("3k");
        assert_eq!(ast.len(), 1);
        assert_eq!(ast[0].modifiers.len(), 1);
        assert!(matches!(ast[0].modifiers[0], AstNodeModifier::Power(3)));
        Ok(())
    }

    #[test]
    fn units() -> Result<()> {
        let ast = calculation!("3m");
        assert_eq!(ast.len(), 1);
        assert_eq!(ast[0].unit.as_ref().unwrap().to_string(), "m");
        let ast = calculation!("3km");
        assert_eq!(ast.len(), 1);
        assert_eq!(ast[0].unit.as_ref().unwrap().to_string(), "km");
        let ast = calculation!("3EUR");
        assert_eq!(ast.len(), 1);
        assert_eq!(ast[0].unit.as_ref().unwrap().to_string(), "EUR");
        Ok(())
    }

    #[test]
    fn unit_with_exponentiation() -> Result<()> {
        let ast = calculation!("3m^3");
        assert_eq!(ast.len(), 1);
        assert_eq!(*ast[0].unit.as_ref().unwrap(), Unit::new("m", 3.0, range!(line 0 => 1..4)));
        Ok(())
    }

    #[test]
    fn unit_without_exponentiation() -> Result<()> {
        let ast = calculation!("3m ^3");
        assert_eq!(ast.len(), 3);
        assert!(matches!(ast[1].data, AstNodeData::Operator(_)));
        Ok(())
    }

    #[test]
    fn complex_units() -> Result<()> {
        let ast = calculation!("3km/h");
        assert_eq!(ast.len(), 1);
        assert_eq!(
            *ast[0].unit.as_ref().unwrap(),
            Unit::Fraction(Box::new(Unit::new("km", 1.0, range!(line 0 => 1..3))),
                           Box::new(Unit::new("h", 1.0, range!(line 0 => 4..5))))
        );
        Ok(())
    }

    #[test]
    fn unit_conversions() -> Result<()> {
        let ast = calculation!("3m in km");
        assert_eq!(ast.len(), 3);
        assert_eq!(ast[0].unit.as_ref().unwrap().to_string(), "m");
        assert!(matches!(ast[1].data, AstNodeData::Operator(Operator::In)));
        assert!(matches!(ast[2].data, AstNodeData::Unit(_)));
        Ok(())
    }

    #[test]
    fn variable_definitions() -> Result<()> {
        let (name, ast) = var_definition!("x := 3");
        assert_eq!(name, "x");
        assert!(ast.is_some());
        assert_eq!(ast.as_ref().unwrap().len(), 1);
        assert_eq!(ast.unwrap()[0].data, AstNodeData::Literal(3.0));

        let (name, ast) = var_definition!("x :=");
        assert_eq!(name, "x");
        assert!(ast.is_none());
        Ok(())
    }

    #[test]
    fn function_definitions() -> Result<()> {
        let (name, args, ast) = func_definition!("f(x) := x");
        assert_eq!(name, "f");
        assert_eq!(args, vec![("x".into(), None)]);
        assert!(ast.is_some());
        assert_eq!(ast.as_ref().unwrap().len(), 1);
        assert_eq!(ast.unwrap()[0].data, AstNodeData::Identifier("x".to_owned()));

        let (name, args, ast) = func_definition!("f(x, y) := x");
        assert_eq!(name, "f");
        assert_eq!(args, vec![("x".into(), None), ("y".into(), None)]);
        assert!(ast.is_some());

        let (name, args, ast) = func_definition!("f(x, y) :=");
        assert_eq!(name, "f");
        assert_eq!(args, vec![("x".into(), None), ("y".into(), None)]);
        assert!(ast.is_none());
        Ok(())
    }

    #[test]
    fn scientific_notation() -> Result<()> {
        fn test(ast: Vec<AstNode>) -> Result<()> {
            assert_eq!(ast.len(), 1);
            assert!(matches!(ast[0].data, AstNodeData::Group(_)));
            let AstNodeData::Group(ast) = &ast[0].data else { unreachable!(); };
            assert_eq!(ast.len(), 5); // 1 * 10 ^ 2
            assert!(matches!(ast[0].data, AstNodeData::Literal(_)));
            assert!(matches!(ast[1].data, AstNodeData::Operator(Operator::Multiply)));
            assert!(matches!(ast[2].data, AstNodeData::Literal(_)));
            assert!(matches!(ast[3].data, AstNodeData::Operator(Operator::Exponentiation)));
            assert!(matches!(ast[4].data, AstNodeData::Literal(_)));
            Ok(())
        }

        test(calculation!("1e2"))?;
        test(calculation!("1e-2"))?;
        test(calculation!("1e--2"))?;

        let ast = calculation!("1e");
        assert_eq!(ast.len(), 3);
        assert!(matches!(ast[2].data, AstNodeData::Identifier(_)));
        Ok(())
    }

    #[test]
    fn expected_operand() -> Result<()> {
        let ast = parse!("2 3 + 4");
        assert_error_type!(ast, ExpectedOperator);
        Ok(())
    }

    #[test]
    fn expected_number() -> Result<()> {
        let ast = parse!("2 +");
        assert_error_type!(ast, ExpectedNumber);
        Ok(())
    }

    #[test]
    fn percent_expected_number() -> Result<()> {
        let ast = parse!("3 + %");
        assert_error_type!(ast, ExpectedNumber);
        let ast = parse!("%");
        assert_error_type!(ast, ExpectedNumber);
        Ok(())
    }

    #[test]
    fn missing_closing_bracket() -> Result<()> {
        let ast = parse!("(");
        assert_error_type!(ast, MissingClosingBracket);
        let ast = parse!("sin(");
        assert_error_type!(ast, MissingClosingBracket);
        Ok(())
    }

    #[test]
    fn missing_opening_bracket() -> Result<()> {
        let ast = parse!(")");
        assert_error_type!(ast, MissingOpeningBracket);
        let ast = parse!("sin)");
        assert_error_type!(ast, MissingOpeningBracket);
        Ok(())
    }

    #[test]
    fn unknown_identifier() -> Result<()> {
        let ast = parse!("something");
        assert_error_type!(ast, UnknownIdentifier(_));
        Ok(())
    }

    #[test]
    fn reserved_variable() -> Result<()> {
        let err = parse!("pi :=");
        assert_error_type!(err, ReservedVariable(_));
        Ok(())
    }

    #[test]
    fn reserved_function() -> Result<()> {
        let err = parse!("sin(x) :=");
        assert_error_type!(err, ReservedFunction(_));
        Ok(())
    }

    #[test]
    fn duplicate_argument() -> Result<()> {
        let err = parse!("f(x, x) :=");
        assert_error_type!(err, DuplicateArgument(_));
        Ok(())
    }

    #[test]
    fn expected_unit() -> Result<()> {
        let err = parse!("3km in k");
        assert_error_type!(err, ExpectedUnit);
        Ok(())
    }

    #[test]
    fn date_object() -> Result<()> {
        let result = calculation!("{date now}");
        assert_eq!(result.len(), 1);
        assert!(matches!(result[0].data, AstNodeData::Object(CalculatorObject::Date(_))));
        let result = calculation!("{date 01.01.2023}");
        assert_eq!(result.len(), 1);
        assert!(matches!(result[0].data, AstNodeData::Object(CalculatorObject::Date(_))));
        Ok(())
    }

    #[test]
    fn unknown_object() -> Result<()> {
        let err = parse!("{asdf}");
        assert_error_type!(err, UnknownObject(_));
        Ok(())
    }

    #[test]
    fn prefers_variables_and_functions() -> Result<()> {
        let mut env = Environment::new();
        env.set_variable("a", Variable(Value::Number(NumberValue::new(3.0)))).unwrap();

        let result = calculation!("4a", Context {
            env: &env,
            currencies: &Currencies::none(),
            settings: &Settings::default(),
        });
        assert_eq!(result.len(), 3);
        Ok(())
    }

    #[test]
    fn group_with_modifiers() -> Result<()> {
        let result = calculation!("!(4)!%");
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].modifiers.len(), 3);

        let result = calculation!("3 + !(1)%");
        assert_eq!(result.len(), 3);
        assert_eq!(result[2].modifiers.len(), 2);
        Ok(())
    }

    #[test]
    fn empty_group() -> Result<()> {
        let result = calculation!("()");
        assert_eq!(result.len(), 1);
        let AstNodeData::Literal(n) = result[0].data else { unreachable!(); };
        assert_eq!(n, 1.0);
        Ok(())
    }
}
