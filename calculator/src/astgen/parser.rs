/*
 * Copyright (c) 2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;

use crate::{Context, error, Format};
use crate::astgen::ast::{AstNode, AstNodeData, AstNodeModifier, BooleanOperator, Operator};
use crate::astgen::objects::{CalculatorObject, ObjectArgument};
use crate::astgen::tokenizer::{Token, TokenType, TokenType::*};
use crate::common::{Error, ErrorType::*, ErrorType, Result};
use crate::environment::ArgCount;
use crate::environment::units::{get_prefix_power, is_unit_with_prefix, Unit};

macro_rules! parse_f64_radix {
    ($text:expr, $radix:expr, $range:expr) => {
        {
            match i64::from_str_radix(&$text[2..], $radix) {
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

fn all() -> impl Fn(&TokenType) -> bool {
    |_| true
}

pub enum ParserResult {
    Calculation(Vec<AstNode>),
    BooleanExpression {
        lhs: Vec<AstNode>,
        rhs: Vec<AstNode>,
        operator: BooleanOperator,
    },
    VariableDefinition(String, Option<Vec<AstNode>>),
    FunctionDefinition {
        name: String,
        args: Vec<String>,
        ast: Option<Vec<AstNode>>,
    },
    Equation {
        lhs: Vec<AstNode>,
        rhs: Vec<AstNode>,
        is_question_mark_in_lhs: bool,
        output_variable: Option<(String, Range<usize>)>,
    },
}

impl std::fmt::Display for ParserResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserResult::Calculation(_) => write!(f, "Calculation"),
            ParserResult::BooleanExpression { .. } => write!(f, "Boolean Expression"),
            ParserResult::VariableDefinition(..) => write!(f, "Variable Definition"),
            ParserResult::FunctionDefinition { .. } => write!(f, "Function Definition"),
            ParserResult::Equation { .. } => write!(f, "Equation"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct QuestionMarkInfo {
    is_in_lhs: bool,
    variable: Option<(String, Range<usize>)>,
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    nesting_level: usize,
    context: Context<'a>,
    extra_allowed_variables: Option<&'a [String]>,
    allow_question_mark: bool,
    question_mark: Option<QuestionMarkInfo>,
    did_find_equals_sign: bool,
}

impl<'a> Parser<'a> {
    pub(crate) fn parse(tokens: &'a [Token], context: Context) -> Result<ParserResult> {
        let mut parser = Parser::new(
            tokens,
            context,
            0,
            true,
            None,
        );
        if let Some(name) = parser.try_accept_variable_definition_head() {
            let name = name?;
            let ast = parser.accept_definition_expression()?;
            Ok(ParserResult::VariableDefinition(name, ast))
        } else if let Some(result) = parser.try_accept_function_definition_head() {
            let (name, args) = result?;

            parser.set_extra_allowed_variables(&args);
            let ast = parser.accept_definition_expression()?;

            Ok(ParserResult::FunctionDefinition { name, args, ast })
        } else {
            parser.accept_expression()
        }
    }

    pub(crate) fn new(
        tokens: &'a [Token],
        context: Context<'a>,
        nesting_level: usize,
        allow_question_mark: bool,
        question_mark: Option<QuestionMarkInfo>,
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
        }
    }

    pub fn set_extra_allowed_variables(&mut self, variables: &'a [String]) {
        self.extra_allowed_variables = Some(variables);
    }

    fn error_range_at_end(&self) -> Range<usize> {
        let Some(last_range) = self.tokens.last().map(|token| token.range()) else {
            return 0..1;
        };
        last_range.end - 1..last_range.end
    }

    fn accept<Predicate>(&mut self, predicate: Predicate, error_type: ErrorType) -> Result<&Token>
        where Predicate: Fn(&TokenType) -> bool {
        if self.index >= self.tokens.len() {
            return Err(error_type.with(self.error_range_at_end()));
        }

        let token = &self.tokens[self.index];
        if predicate(&token.ty) {
            self.index += 1;
            Ok(token)
        } else {
            Err(error_type.with(token.range()))
        }
    }

    fn try_accept<Predicate>(&mut self, predicate: Predicate) -> Option<&Token>
        where Predicate: Fn(&TokenType) -> bool {
        if self.index >= self.tokens.len() {
            return None;
        }

        let token = &self.tokens[self.index];
        if predicate(&token.ty) {
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }

    /// Get the next token, if it matches `predicate`
    fn peek<Predicate>(&self, predicate: Predicate) -> Option<&Token>
        where Predicate: Fn(&TokenType) -> bool {
        if self.index >= self.tokens.len() {
            return None;
        }

        let token = &self.tokens[self.index];
        if predicate(&token.ty) {
            Some(token)
        } else {
            None
        }
    }

    fn try_accept_variable_definition_head(&mut self) -> Option<Result<String>> {
        let identifier = self.try_accept(is(Identifier))?;
        let identifier_range = identifier.range();
        let name = identifier.text.clone();

        if self.try_accept(is(DefinitionSign)).is_none() {
            self.index = self.index.saturating_sub(2);
            return None;
        }

        if self.context.env.is_standard_variable(&name) {
            return Some(Err(ReservedVariable(name).with(identifier_range)));
        }

        Some(Ok(name))
    }

    /// Tries to accept a function definition head. To do this, the function tries to parse the
    /// entire definition head while storing the first error it encountered. If it does not find
    /// a definition sign, it returns None, discards the error and resets self.index. If it finds
    /// a definition sign, it releases the stored error if there is one, and otherwise returns the
    /// parsed data.
    fn try_accept_function_definition_head(&mut self) -> Option<Result<(String, Vec<String>)>> {
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
        let identifier_range = identifier.range();
        let name = identifier.text.clone();

        if self.context.env.is_standard_function(&name) && first_error.is_none() {
            first_error = Some(ReservedFunction(name.clone()).with(identifier_range));
        }

        try_token!(self.accept(is(OpenBracket), ExpectedOpenBracket));

        let first_arg = try_token!(self.accept(is(Identifier), ExpectedIdentifier), _identifier).text.clone();
        let mut args = vec![first_arg];

        let _close_bracket = Token::empty_from_type(CloseBracket);
        loop {
            let next = try_token!(self.accept(any(&[Comma, CloseBracket]), ExpectedComma), _close_bracket);
            match next.ty {
                Comma => {
                    let arg = try_token!(self.accept(is(Identifier), ExpectedIdentifier), _identifier);
                    if args.contains(&arg.text) && first_error.is_none() {
                        first_error = Some(DuplicateArgument(arg.text.clone()).with(arg.range()));
                    }
                    args.push(arg.text.clone());
                }
                CloseBracket => break,
                _ => unreachable!(),
            }
        }

        let def_sign_res = self.accept(is(DefinitionSign), UnexpectedElements);
        if def_sign_res.is_err() {
            self.index = start_index;
            None
        } else {
            Some(
                if let Some(e) = first_error {
                    Err(e)
                } else {
                    Ok((name, args))
                }
            )
        }
    }

    fn accept_definition_expression(&mut self) -> Result<Option<Vec<AstNode>>> {
        let expr_start = self.index;
        if expr_start >= self.tokens.len() {
            Ok(None)
        } else {
            match self.accept_expression()? {
                ParserResult::Calculation(ast) => Ok(Some(ast)),
                res => {
                    let range = self.tokens[expr_start].range().start..self.tokens.last().unwrap().range().end;
                    error!(ExpectedExpression(res.to_string()): range)
                }
            }
        }
    }

    fn accept_expression(&mut self) -> Result<ParserResult> {
        let mut group_stack: Vec<(Vec<AstNode>, Range<usize>)> = vec![(vec![], Range::default())];
        let mut boolean_operator: Option<(BooleanOperator, usize)> = None;

        /// Helper to get the current AST
        macro_rules! ast {
            () => { group_stack.last_mut().unwrap().0 }
        }

        // If there is a close bracket at the beginning of the line, it is an error
        if let Some(close_bracket) = self.try_accept(is(CloseBracket)) {
            error!(MissingOpeningBracket: close_bracket.range());
        }

        // Accept opening brackets at the beginning of the line
        while self.index < self.tokens.len() {
            if let Some(open_bracket) = self.try_accept(is(OpenBracket)) {
                group_stack.push((vec![], open_bracket.range()));
                self.nesting_level += 1;
            } else {
                break;
            }
        }

        if self.index >= self.tokens.len() && group_stack.len() > 1 {
            error!(MissingClosingBracket: group_stack.last().unwrap().1.clone());
        }

        // Allow empty brackets, which is handled at the start of the while loop below
        // NOTE: () = 1
        if self.peek(is(CloseBracket)).is_none() {
            ast!().push(self.accept_number()?);
        }

        while self.index < self.tokens.len() {
            while self.index < self.tokens.len() {
                if let Some(close_bracket) = self.try_accept(is(CloseBracket)) {
                    if group_stack.len() == 1 {
                        error!(MissingOpeningBracket: close_bracket.range());
                    }

                    let (group, open_bracket_range) = group_stack.pop().unwrap();
                    let range = open_bracket_range.start..close_bracket.range().end;

                    let unit = self.try_accept_unit().transpose()?;

                    let mut node = if group.is_empty() {
                        AstNode::new(AstNodeData::Literal(1.0), range)
                    } else {
                        AstNode::new(AstNodeData::Group(group), range)
                    };

                    if let Some((unit, unit_range)) = unit {
                        node.unit = Some(unit);
                        node.range.end = unit_range.end;
                    }

                    ast!().push(node);
                    self.nesting_level -= 1;
                } else {
                    break;
                }
            }

            if self.index >= self.tokens.len() { break; }

            match self.accept_operator() {
                Ok(op) => {
                    let AstNodeData::Operator(operator) = op.data else { unreachable!(); };
                    // RHS of `in` (unit / format)
                    if operator == Operator::In {
                        if let Some(unit) = self.try_accept_unit() {
                            let (unit, range) = unit?;
                            ast!().push(op);
                            ast!().push(AstNode::new(AstNodeData::Unit(unit), range));
                            continue;
                        } else if let Some(format) = self.try_accept(|ty| ty.is_format()) {
                            let format = match format.ty {
                                Decimal => Format::Decimal,
                                Binary => Format::Binary,
                                Hex => Format::Hex,
                                Scientific => Format::Scientific,
                                _ => unreachable!(),
                            };
                            ast!().last_mut().unwrap().format = format;
                            continue;
                        } else {
                            let last = self.tokens.last().unwrap();
                            let range = if last.ty == In {
                                let range = last.range();
                                range.end - 1..range.end
                            } else {
                                last.range()
                            };
                            error!(ExpectedUnit: range);
                        };
                    }

                    ast!().push(op);
                }
                Err(error) => {
                    // Try to infer multiplication
                    if self.peek(any(&[OpenBracket, Identifier])).is_some() {
                        ast!().push(AstNode::new(AstNodeData::Operator(Operator::Multiply), 0..1));
                    } else if let Some((op, range)) = self.try_accept_boolean_operator() {
                        if self.nesting_level != 0 {
                            error!(UnexpectedBooleanOperator: range);
                        } else if boolean_operator.is_some() {
                            error!(UnexpectedSecondBooleanOperator: range);
                        }

                        if op == BooleanOperator::Equal {
                            self.did_find_equals_sign = true;
                        }

                        boolean_operator = Some((op, ast!().len()));
                    } else {
                        return Err(error);
                    }
                }
            }

            while self.index < self.tokens.len() {
                if let Some(open_bracket) = self.try_accept(is(OpenBracket)) {
                    group_stack.push((vec![], open_bracket.range()));
                    self.nesting_level += 1;
                } else {
                    break;
                }
            }

            // Allow empty groups. This will be handled at the start of this loop.
            // NOTE: () = 1
            if self.peek(is(CloseBracket)).is_some() &&
                self.tokens[self.index - 1].ty == OpenBracket { continue; }
            ast!().push(self.accept_number()?);
        }

        if group_stack.len() > 1 {
            error!(MissingClosingBracket: group_stack.last().unwrap().1.clone());
        }

        let (result, _) = group_stack.pop().unwrap();

        if let Some((op, index)) = boolean_operator {
            let (lhs, rhs) = result.split_at(index);
            if let Some(info) = std::mem::take(&mut self.question_mark) {
                Ok(ParserResult::Equation {
                    lhs: lhs.to_vec(),
                    rhs: rhs.to_vec(),
                    is_question_mark_in_lhs: info.is_in_lhs,
                    output_variable: info.variable,
                })
            } else {
                Ok(ParserResult::BooleanExpression {
                    lhs: lhs.to_vec(),
                    rhs: rhs.to_vec(),
                    operator: op,
                })
            }
        } else {
            if self.nesting_level == 0 && self.question_mark.is_some() {
                let last = self.tokens.last().unwrap().range();
                let range = last.end - 1..last.end;
                error!(MissingEqualsSign: range);
            }

            Ok(ParserResult::Calculation(result))
        }
    }

    fn try_accept_boolean_operator(&mut self) -> Option<(BooleanOperator, Range<usize>)> {
        let op = self.try_accept(|ty| ty.is_boolean_operator())?;

        let range = op.range();
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
        if self.peek(is(OpenCurlyBracket)).is_some() {
            self.accept_object()
        } else {
            let mut modifiers = self.accept_prefix_modifiers();

            let next = self.peek(all());
            let mut number = match next.map(|token| token.ty) {
                Some(ty) if ty.is_literal() => self.accept_literal()?,
                Some(Identifier) => self.accept_identifier()?,
                Some(QuestionMark) => self.accept_question_mark()?,
                Some(_) => error!(ExpectedNumber: next.unwrap().range()),
                None => error!(ExpectedNumber: self.error_range_at_end()),
            };

            modifiers.append(&mut self.accept_suffix_modifiers());
            number.modifiers.append(&mut modifiers);

            if let Some(unit) = self.try_accept_unit() {
                let (unit, unit_range) = unit?;
                number.unit = Some(unit);
                number.range.end = unit_range.end;
            } else if let Some(power) = self.try_accept_unit_prefix() {
                number.modifiers.push(AstNodeModifier::Power(power));
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

                        let range = number.range.clone();
                        let group = vec![
                            number,
                            AstNode::new(AstNodeData::Operator(Operator::Multiply), range.clone()),
                            AstNode::new(AstNodeData::Literal(10.0), range.clone()),
                            AstNode::new(AstNodeData::Operator(Operator::Exponentiation), range.clone()),
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

            Ok(number)
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
                    Err(e) => error!(InvalidNumber(e.to_string()): literal.range()),
                };
                AstNodeData::Literal(number)
            }
            HexLiteral => AstNodeData::Literal(parse_f64_radix!(text, 16, literal.range())),
            BinaryLiteral => AstNodeData::Literal(parse_f64_radix!(text, 2, literal.range())),
            _ => unreachable!(),
        };

        Ok(AstNode::new(data, literal.range()))
    }

    fn accept_question_mark(&mut self) -> Result<AstNode> {
        let token = self.accept(is(QuestionMark), ExpectedQuestionMark)?;
        let range = token.range();
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

    fn try_accept_unit(&mut self) -> Option<Result<(Unit, Range<usize>)>> {
        if self.peek(is(OpenSquareBracket)).is_some() {
            return Some(self.accept_complex_unit());
        }

        let Some(numerator) = self.try_accept_single_unit() else { return None; };
        let (numerator, numerator_range) = match numerator {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };

        if self.peek(is(Divide)).is_some() {
            self.index += 1;
            if let Some(denominator) = self.try_accept_single_unit() {
                return match denominator {
                    Ok((denom, denominator_range)) => {
                        Some(Ok((
                            Unit::Fraction(Box::new(Unit::Unit(numerator)), Box::new(Unit::Unit(denom))),
                            numerator_range.start..denominator_range.end
                        )))
                    }
                    Err(e) => Some(Err(e)),
                };
            } else {
                self.index -= 1;
            }
        }

        Some(Ok((Unit::Unit(numerator), numerator_range)))
    }

    fn accept_complex_unit(&mut self) -> Result<(Unit, Range<usize>)> {
        let open_bracket = self.accept(is(OpenSquareBracket), ExpectedOpenSquareBracket)?;
        let range_start = open_bracket.range().start;

        let (mut units, _) = self.next_units(0)?;
        if units.is_empty() {
            error!(ExpectedUnit: self.peek(all()).map(|t| t.range()).unwrap_or_else(|| self.error_range_at_end()));
        }
        let mut unit = if units.len() == 1 { units.remove(0) } else { Unit::Product(units) };
        unit.simplify();

        let close_bracket = self.accept(is(CloseSquareBracket), ExpectedCloseSquareBracket)?;
        Ok((unit, range_start..close_bracket.range().end))
    }

    fn next_units(&mut self, nesting_level: usize) -> Result<(Vec<Unit>, Range<usize>)> {
        let mut result = vec![];
        let mut result_range = 0..1;

        let mut numerator: Option<Unit> = None;

        while self.peek(is(CloseSquareBracket)).is_none() {
            let Some(token) = self.peek(all()) else {
                error!(ExpectedElements: self.error_range_at_end());
            };
            let token_range = token.range();
            if result_range.start == 0 {
                result_range = token.range();
            }

            match token.ty {
                OpenBracket => 'blk: {
                    self.index += 1;
                    let (mut units, range) = self.next_units(nesting_level + 1)?;
                    if units.is_empty() {
                        error!(ExpectedElements: token_range.start..range.end);
                    }
                    result_range.end = range.end;

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
                    let (unit_str, unit_range) = unit?;
                    result_range.end = unit_range.end;

                    let unit = Unit::Unit(unit_str);

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

    fn try_accept_single_unit(&mut self) -> Option<Result<(String, Range<usize>)>> {
        let Some(unit) = self.peek(is(Identifier)) else { return None; };
        if !is_unit_with_prefix(&unit.text) { return None; }

        let unit_range = unit.range();
        let mut unit = unit.text.clone();
        self.index += 1;

        if let Some(exponentiation) = self.peek(is(Exponentiation)) {
            if exponentiation.range().start == unit_range.end {
                self.index += 1;
                if let Ok(AstNode { data, range, .. }) = self.accept_literal() {
                    unit += "^";
                    let AstNodeData::Literal(number) = data else { unreachable!(); };
                    unit += number.to_string().as_str();

                    if !is_unit_with_prefix(&unit) {
                        return Some(Err(UnknownIdentifier(unit).with(unit_range.start..range.end)));
                    }
                } else {
                    self.index -= 1;
                }
            }
        }

        Some(Ok((unit, unit_range)))
    }

    fn accept_identifier(&mut self) -> Result<AstNode> {
        let identifier = self.accept(is(Identifier), ExpectedIdentifier)?;
        let name = identifier.text.clone();
        let range = identifier.range();

        if self.context.env.is_valid_variable(&name) ||
            self.extra_allowed_variables.map_or(false, |vars| vars.contains(&name)) {
            if let Some(question_mark) = self.try_accept_question_mark_after_identifier(&name, &range) {
                return question_mark;
            }
            return Ok(AstNode::new(AstNodeData::VariableReference(name), range));
        } else if self.context.env.is_valid_function(&name) {
            let arguments = self.accept_function_arguments(&name)?;
            let close_bracket_range = self.tokens[self.index - 1].range();

            return Ok(AstNode::new(
                AstNodeData::FunctionInvocation(name, arguments),
                range.start..close_bracket_range.end),
            );
        }

        if let Some(question_mark) = self.try_accept_question_mark_after_identifier(&name, &range) {
            return question_mark;
        }

        error!(UnknownIdentifier(name): range)
    }

    fn try_accept_question_mark_after_identifier(&mut self, identifier: &str, range: &Range<usize>) -> Option<Result<AstNode>> {
        self.peek(is(QuestionMark))?;
        let question_mark = match self.accept_question_mark() {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        if let Some(info) = self.question_mark.as_mut() {
            info.variable = Some((identifier.to_string(), range.clone()));
        }
        Some(Ok(question_mark))
    }

    fn accept_object(&mut self) -> Result<AstNode> {
        let open_bracket = self.accept(is(OpenCurlyBracket), ExpectedOpenCurlyBracket)?;
        let full_range_start = open_bracket.range().start;
        let name = self.accept(is(Identifier), ExpectedObjectName)?;
        if !CalculatorObject::is_valid_object(&name.text) {
            error!(UnknownObject(name.text.clone()): name.range());
        }
        let name = (name.text.clone(), name.range());

        let range_start = self.index;
        let mut args = vec![];
        while self.index < self.tokens.len() {
            let Some(token) = self.peek(all()) else {
                error!(ExpectedCloseCurlyBracket: self.error_range_at_end());
            };
            match token.ty {
                CloseCurlyBracket => break,
                OpenSquareBracket => {
                    let ast = self.accept_object_arguments()?;
                    let range = ast.first().unwrap().range.start..ast.last().unwrap().range.end;
                    args.push(ObjectArgument::Ast(ast, range));
                }
                CloseSquareBracket => error!(UnexpectedCloseBracket: token.range()),
                ObjectArgs => {
                    let token = self.accept(is(ObjectArgs), Nothing).unwrap();
                    args.push(ObjectArgument::String(token.text.clone(), token.range()))
                }
                _ => error!(InvalidToken: token.range()),
            }
        }

        let close_bracket = self.accept(is(CloseCurlyBracket), ExpectedCloseCurlyBracket)?;
        let close_bracket_range = close_bracket.range();

        if self.index - 1 == range_start {
            error!(ExpectedElements: close_bracket_range);
        }

        let object = CalculatorObject::parse(
            name,
            args,
            self.context,
            full_range_start..close_bracket_range.end,
        )?;
        Ok(AstNode::new(AstNodeData::Object(object), full_range_start..close_bracket_range.end))
    }

    fn accept_object_arguments(&mut self) -> Result<Vec<AstNode>> {
        let open_bracket = self.accept(is(OpenSquareBracket), ExpectedOpenSquareBracket)?;
        let open_bracket_range = open_bracket.range();

        let mut nesting_level = 1usize;
        let start = self.index;
        while self.index < self.tokens.len() {
            let Some(token) = self.try_accept(all()) else { continue; };
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
        let mut parser = Self::new(
            tokens,
            self.context,
            self.nesting_level + 1,
            false,
            self.question_mark.clone(),
        );
        if let Some(vars) = self.extra_allowed_variables {
            parser.set_extra_allowed_variables(vars);
        }
        let ParserResult::Calculation(ast) = parser.accept_expression()? else { unreachable!(); };
        Ok(ast)
    }

    fn accept_function_arguments(&mut self, function_name: &str) -> Result<Vec<Vec<AstNode>>> {
        let open_bracket_token = self.accept(is(OpenBracket), MissingOpeningBracket)?;
        let open_bracket_range = open_bracket_token.range();

        let mut arguments: Vec<&[Token]> = vec![];

        let mut range_end = 0usize;

        let mut nesting_level = 1usize;
        let mut argument_start = self.index;
        while self.index < self.tokens.len() {
            let Some(token) = self.try_accept(all()) else { continue; };
            match token.ty {
                OpenBracket => nesting_level += 1,
                CloseBracket => {
                    nesting_level -= 1;
                    // Ignore brackets that aren't on the base level
                    if nesting_level != 0 { continue; }

                    // Set range_end here, because otherwise the borrow checker complains because
                    // we immutably borrowed `token`, and are now trying to mutably borrow
                    // `self.index` and `self.tokens`.
                    range_end = token.range().end;

                    if argument_start == self.index - 1 {
                        let range = self.tokens[argument_start].range().start..self.tokens[self.index - 1].range().end;
                        error!(ExpectedElements: range);
                    }
                    let argument = &self.tokens[argument_start..self.index - 1];
                    arguments.push(argument);

                    if nesting_level == 0 { break; }
                }
                Comma => {
                    if nesting_level == 1 {
                        if argument_start == self.index - 1 {
                            let range = self.tokens[argument_start].range().start..self.tokens[self.index - 1].range().end;
                            error!(ExpectedElements: range);
                        }
                        let argument = &self.tokens[argument_start..self.index - 1];
                        arguments.push(argument);
                        argument_start = self.index;
                    }
                }
                _ => {}
            }
        }

        if nesting_level != 0 {
            error!(MissingClosingBracket: open_bracket_range);
        }

        let full_range = open_bracket_range.start..range_end;

        let function_args_count = self.context.env.function_argument_count(function_name).unwrap();
        if !function_args_count.is_valid_count(arguments.len()) {
            match function_args_count {
                ArgCount::Single(count) => error!(WrongNumberOfArguments(count): full_range),
                ArgCount::Multiple(options) => error!(WrongNumberOfArgumentsMultiple(options): full_range),
            }
        }

        let allow_question_mark = !self.context.env.is_standard_function(function_name);

        let mut result = Vec::new();
        for tokens in arguments {
            let mut parser = Self::new(
                tokens,
                self.context,
                self.nesting_level + 1,
                allow_question_mark,
                self.question_mark.clone(),
            );
            if let Some(vars) = self.extra_allowed_variables {
                parser.set_extra_allowed_variables(vars);
            }
            let ParserResult::Calculation(ast) = parser.accept_expression()? else { unreachable!(); };
            result.push(ast);
        }

        Ok(result)
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

        Ok(AstNode::new(data, operator.range()))
    }
}

#[cfg(test)]
mod tests {
    use crate::{Currencies, Environment, Settings};
    use crate::astgen::tokenizer::tokenize;

    use super::*;

    macro_rules! parse {
        ($input:expr) => {
            Parser::parse(&tokenize($input)?, Context {
                env: &Environment::new(),
                currencies: &Currencies::none(),
                settings: &Settings::default(),
            })
        };
    }

    macro_rules! assert_error_type {
        ($result:expr, $variant:pat) => {
            assert!(matches!($result.err().unwrap().error, $variant))
        }
    }

    macro_rules! calculation {
        ($input:expr) => {
            if let ParserResult::Calculation(ast) = parse!($input)? {
                ast
            } else {
                panic!("Expected ParserResult::Calculation");
            }
        }
    }

    macro_rules! boolean_expression {
        ($input:expr) => {
            if let ParserResult::BooleanExpression { lhs, rhs, operator } = parse!($input)? {
                (lhs, rhs, operator)
            } else {
                panic!("Expected ParserResult::BooleanExpression");
            }
        }
    }

    macro_rules! var_definition {
        ($input:expr) => {
            if let ParserResult::VariableDefinition(name, ast) = parse!($input)? {
                (name, ast)
            }
            else {
                panic!("Expected ParserResult::FunctionDefinition");
            }
        }
    }

    macro_rules! func_definition {
        ($input:expr) => {
            if let ParserResult::FunctionDefinition { name, args, ast } = parse!($input)? {
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
        assert_eq!(ast[0].unit.as_ref().unwrap().0, "km");
        assert!(ast[0].unit.as_ref().unwrap().1.is_none());

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
            AstNodeData::VariableReference(ref s) => {
                assert_eq!("pi".to_string(), *s);
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    #[test]
    fn functions() -> Result<()> {
        let ast = calculation!("sin(30)");
        match ast[0].data {
            AstNodeData::FunctionInvocation(ref name, ref args) => {
                assert_eq!("sin".to_string(), *name);
                assert!(args.len() == 1 && args[0].len() == 1);
                assert!(matches!(args[0][0].data, AstNodeData::Literal(_)));
            }
            _ => unreachable!(),
        }
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
        assert_eq!(ast[0].unit.as_ref().unwrap().0, "m^3");
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
        assert_eq!(ast[0].unit.as_ref().unwrap().0, "km");
        assert_eq!(ast[0].unit.as_ref().unwrap().1.as_ref().unwrap(), "h");
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
        assert_eq!(args, vec!["x"]);
        assert!(ast.is_some());
        assert_eq!(ast.as_ref().unwrap().len(), 1);
        assert_eq!(ast.unwrap()[0].data, AstNodeData::VariableReference("x".to_owned()));

        let (name, args, ast) = func_definition!("f(x, y) := x");
        assert_eq!(name, "f");
        assert_eq!(args, vec!["x", "y"]);
        assert!(ast.is_some());

        let (name, args, ast) = func_definition!("f(x, y) :=");
        assert_eq!(name, "f");
        assert_eq!(args, vec!["x", "y"]);
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
        assert!(matches!(ast[2].data, AstNodeData::VariableReference(_)));
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
}
