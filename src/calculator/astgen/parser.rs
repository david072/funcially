/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use strum::IntoEnumIterator;
use std::mem;
use crate::{
    Format,
    astgen::ast::{AstNode, Operator, AstNodeData, AstNodeModifier},
    astgen::tokenizer::{Token, TokenType},
    common::*,
    environment::{
        Environment,
        units::{get_prefix_power, is_prefix, is_unit, Unit},
    },
};

macro_rules! error {
    ($variant:ident($range:expr)) => {
        return Err(ErrorType::$variant.with($range.clone()))
    }
}

macro_rules! error_with_args {
    ($variant:ident($range:expr) $($arg:expr),*) => {
        return Err(ErrorType::$variant($($arg),*).with($range.clone()))
    }
}

macro_rules! remove_elems {
    ($vec:ident, $clojure:tt) => {
        while let Some(i) = $vec.iter().position($clojure) {
            $vec.remove(i);
        }
    }
}

macro_rules! parse_f64_radix {
    ($token:expr, $radix:expr) => {
        (match i64::from_str_radix(&$token.text[2..], $radix) {
            Ok(number) => number,
            Err(_) => return Err(ErrorType::InvalidNumber.with($token.range.clone())),
        }) as f64
    }
}

macro_rules! ok_operator {
    ($variant:ident) => {
        Ok(AstNodeData::Operator(Operator::$variant))
    }
}

pub enum ParserResult {
    Calculation(Vec<AstNode>),
    EqualityCheck(Vec<AstNode>, Vec<AstNode>),
    VariableDefinition(String, Option<Vec<AstNode>>),
    FunctionDefinition {
        name: String,
        args: Vec<String>,
        ast: Option<Vec<AstNode>>,
    },
}

impl std::fmt::Display for ParserResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserResult::Calculation(_) => write!(f, "Calculation"),
            ParserResult::EqualityCheck(_, _) => write!(f, "Equality Check"),
            ParserResult::VariableDefinition(_, _) => write!(f, "Variable Definition"),
            ParserResult::FunctionDefinition { name: _, args: _, ast: _ } => write!(f, "Function Definition"),
        }
    }
}

pub fn parse(tokens: &[Token], env: &Environment) -> Result<ParserResult> {
    let definition_signs = tokens.iter()
        .enumerate()
        .filter(|t| t.1.ty == TokenType::DefinitionSign)
        .collect::<Vec<_>>();
    if !definition_signs.is_empty() {
        if definition_signs.len() > 1 {
            return Err(ErrorType::UnexpectedDefinition.with(definition_signs[1].1.range.clone()));
        }

        let definition_sign_index = definition_signs[0].0;

        let lhs = &tokens[0..definition_sign_index];
        let rhs = &tokens[definition_sign_index + 1..];

        return match parse_definition_identifier(lhs, env)? {
            DefinitionIdentifier::Variable(name) => {
                if definition_sign_index + 1 == tokens.len() {
                    return Ok(ParserResult::VariableDefinition(name, None));
                }

                let variable_ast = match Parser::new(rhs, 1, env).parse()? {
                    ParserResult::Calculation(ast) => ast,
                    res =>
                        error_with_args!(ExpectedExpression(lhs[0].range.start..lhs.last().unwrap().range.end) res.to_string()),
                };

                Ok(ParserResult::VariableDefinition(name, Some(variable_ast)))
            }
            DefinitionIdentifier::Function(name, args) => {
                if definition_sign_index + 1 == tokens.len() {
                    return Ok(ParserResult::FunctionDefinition {
                        name,
                        args,
                        ast: None,
                    });
                }

                let mut parser = Parser::new(rhs, 1, env);
                parser.extra_allowed_variables = Some(&args);
                let function_ast = match parser.parse()? {
                    ParserResult::Calculation(ast) => ast,
                    res =>
                        error_with_args!(ExpectedExpression(lhs[0].range.start..lhs.last().unwrap().range.end) res.to_string()),
                };
                Ok(ParserResult::FunctionDefinition {
                    name,
                    args,
                    ast: Some(function_ast),
                })
            }
        };
    }

    let mut parser = Parser::new(tokens, 0, env);
    let result = parser.parse()?;
    Ok(result)
}

enum DefinitionIdentifier {
    Variable(String),
    Function(String, Vec<String>),
}

fn parse_definition_identifier(tokens: &[Token], env: &Environment) -> Result<DefinitionIdentifier> {
    if tokens.is_empty() {
        error!(ExpectedElements(0..1));
    } else if tokens[0].ty != TokenType::Identifier {
        error!(ExpectedIdentifier(tokens[0].range));
    }

    let name = tokens[0].text.clone();
    if env.is_standard_variable(&name) {
        error!(ReservedVariable(tokens[0].range));
    } else if env.is_standard_function(&name) {
        error!(ReservedFunction(tokens[0].range));
    }

    if tokens.len() == 1 {
        return Ok(DefinitionIdentifier::Variable(name));
    }

    // We're parsing a function!
    if tokens[1].ty != TokenType::OpenBracket {
        error!(ExpectedOpenBracket(tokens[1].range));
    }

    let mut args = Vec::new();
    let mut last_token_ty: Option<TokenType> = None;

    let mut i = 2usize;
    for token in &tokens[2..] {
        if let Some(ty) = last_token_ty {
            match ty {
                TokenType::Identifier => {
                    if token.ty == TokenType::CloseBracket {
                        break;
                    } else if token.ty != TokenType::Comma {
                        error!(ExpectedComma(token.range));
                    }
                }
                TokenType::Comma => {
                    if token.ty != TokenType::Identifier { error!(ExpectedIdentifier(token.range)); }
                    if args.contains(&token.text) { error!(DuplicateArgument(token.range)); }
                    args.push(token.text.clone());
                }
                _ => unreachable!(),
            }
        } else {
            if token.ty != TokenType::Identifier {
                error!(ExpectedIdentifier(token.range));
            }

            args.push(token.text.clone());
        }

        last_token_ty = Some(token.ty);
        i += 1;
    }

    if i == tokens.len() || tokens[i].ty != TokenType::CloseBracket {
        error!(MissingClosingBracket(tokens[1].range));
    } else if i + 1 != tokens.len() {
        error!(UnexpectedElements(tokens[i + 1].range.start..tokens.last().unwrap().range.end));
    }

    Ok(DefinitionIdentifier::Function(name, args))
}

struct Parser<'a> {
    env: &'a Environment,
    extra_allowed_variables: Option<&'a [String]>,

    tokens: &'a [Token],
    nesting_level: usize,

    index: usize,
    all_tokens_tys: Vec<TokenType>,
    last_token_ty: Option<TokenType>,
    next_token_modifiers: Vec<AstNodeModifier>,
    equals_sign_index: Option<usize>,
    result: Vec<AstNode>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], nesting_level: usize, env: &'a Environment) -> Parser<'a> {
        Parser {
            env,
            extra_allowed_variables: None,
            tokens,
            nesting_level,
            index: 0,
            all_tokens_tys: TokenType::iter().collect(),
            last_token_ty: None,
            next_token_modifiers: Vec::new(),
            equals_sign_index: None,
            result: Vec::new(),
        }
    }

    pub fn from(other: &Parser<'a>, tokens: &'a [Token]) -> Parser<'a> {
        Parser {
            extra_allowed_variables: other.extra_allowed_variables,
            ..Parser::new(tokens, other.nesting_level + 1, other.env)
        }
    }

    pub fn parse(&mut self) -> Result<ParserResult> {
        while self.next()? {}

        if !self.next_token_modifiers.is_empty() {
            return Err(ErrorType::ExpectedNumber.with(self.tokens.last().unwrap().range.clone()));
        }

        let result = mem::take(&mut self.result);
        if let Some(index) = self.equals_sign_index {
            let (lhs, rhs) = result.split_at(index);
            Ok(ParserResult::EqualityCheck(lhs.to_vec(), rhs.to_vec()))
        } else {
            Ok(ParserResult::Calculation(result))
        }
    }

    pub fn next(&mut self) -> Result<bool> {
        if self.index >= self.tokens.len() {
            return Ok(false);
        }

        let token = &self.tokens[self.index];
        self.index += 1;

        self.verify_valid_token(token)?;

        // Handle formats
        if token.ty.is_format() {
            self.result.remove(self.result.len() - 1);
            match token.ty {
                TokenType::Decimal => self.result.last_mut().unwrap().format = Format::Decimal,
                TokenType::Hex => self.result.last_mut().unwrap().format = Format::Hex,
                TokenType::Binary => self.result.last_mut().unwrap().format = Format::Binary,
                _ => unreachable!(),
            }
            return Ok(true);
        }

        // Handle special tokens
        match token.ty {
            TokenType::ExclamationMark => {
                return if let Some(last_ty) = self.last_token_ty {
                    if !last_ty.is_operator() {
                        // Exclamation mark is factorial
                        let last_node = self.result.last_mut().unwrap();
                        last_node.modifiers.push(AstNodeModifier::Factorial);
                        Ok(true)
                    } else {
                        self.next_token_modifiers.push(AstNodeModifier::BitwiseNot);
                        Ok(true)
                    }
                } else {
                    self.next_token_modifiers.push(AstNodeModifier::BitwiseNot);
                    Ok(true)
                };
            }
            TokenType::PercentSign => {
                let last_node = self.result.last_mut().unwrap();
                last_node.modifiers.push(AstNodeModifier::Percent);
                return Ok(true);
            }
            TokenType::Minus => {
                if let Some(last_ty) = self.last_token_ty {
                    if last_ty.is_operator() {
                        self.next_token_modifiers.push(AstNodeModifier::Minus);
                        return Ok(true);
                    }
                } else {
                    self.next_token_modifiers.push(AstNodeModifier::Minus);
                    return Ok(true);
                }
            }
            TokenType::Plus => {
                if let Some(last_ty) = self.last_token_ty {
                    if last_ty.is_operator() {
                        self.next_token_modifiers.push(AstNodeModifier::Plus);
                        return Ok(true);
                    }
                } else {
                    self.next_token_modifiers.push(AstNodeModifier::Plus);
                    return Ok(true);
                }
            }
            TokenType::OpenBracket => {
                self.next_group(token)?;
                self.last_token_ty = Some(token.ty);
                return Ok(true);
            }
            TokenType::Identifier => {
                self.next_identifier(token)?;
                self.last_token_ty = Some(token.ty);
                return Ok(true);
            }
            TokenType::EqualsSign => {
                if self.nesting_level != 0 {
                    error!(UnexpectedEqualsSign(token.range));
                } else if self.equals_sign_index.is_some() {
                    error!(UnexpectedSecondEqualsSign(token.range));
                }

                self.equals_sign_index = Some(self.result.len());
                self.last_token_ty = Some(token.ty);
                return Ok(true);
            }
            _ => {}
        }

        self.last_token_ty = Some(token.ty);

        let data = match token.ty {
            TokenType::DecimalLiteral => {
                let number = match token.text.parse() {
                    Ok(number) => number,
                    Err(_) => error!(InvalidNumber(token.range)),
                };
                Ok(AstNodeData::Literal(number))
            }
            TokenType::HexLiteral => Ok(AstNodeData::Literal(parse_f64_radix!(token, 16))),
            TokenType::BinaryLiteral => Ok(AstNodeData::Literal(parse_f64_radix!(token, 2))),
            TokenType::Plus => ok_operator!(Plus),
            TokenType::Minus => ok_operator!(Minus),
            TokenType::Multiply => ok_operator!(Multiply),
            TokenType::Divide => ok_operator!(Divide),
            TokenType::Exponentiation => ok_operator!(Exponentiation),
            TokenType::BitwiseAnd => ok_operator!(BitwiseAnd),
            TokenType::BitwiseOr => ok_operator!(BitwiseOr),
            TokenType::Of => ok_operator!(Of),
            TokenType::In => ok_operator!(In),
            TokenType::DefinitionSign => error!(UnexpectedDefinition(token.range)),
            _ => unreachable!(),
        }?;

        let mut new_node = AstNode::new(data, token.range.clone());
        new_node.modifiers = mem::take(&mut self.next_token_modifiers);
        self.result.push(new_node);
        Ok(true)
    }

    fn next_group(&mut self, open_bracket_token: &Token) -> Result<()> {
        let mut nesting_level = 1usize;

        let group_start = self.index;
        let mut group_end: usize = 0;
        while let Some(token) = self.tokens.get(self.index) {
            self.index += 1;
            match token.ty {
                TokenType::OpenBracket => nesting_level += 1,
                TokenType::CloseBracket => {
                    nesting_level -= 1;
                    if nesting_level == 0 {
                        group_end = self.index;
                        break;
                    }
                }
                _ => {}
            }
        }

        if nesting_level != 0 {
            error!(MissingClosingBracket(open_bracket_token.range));
        }

        self.infer_multiplication(group_start..group_start + 1);

        let group_range = group_start - 1..group_end;
        if group_end - group_start <= 1 {
            self.result.push(AstNode::new(AstNodeData::Literal(0.0), group_range));
            return Ok(());
        }

        let group_tokens = &self.tokens[group_start..group_end - 1];
        let group_ast = match Parser::from(self, group_tokens).parse()? {
            ParserResult::Calculation(ast) => ast,
            _ => unreachable!(),
        };

        self.push_new_node(AstNodeData::Group(group_ast), group_range);
        Ok(())
    }

    fn next_identifier(&mut self, identifier: &Token) -> Result<()> {
        match identifier.ty {
            TokenType::Identifier => {
                let multiplication_range = identifier.range.start..identifier.range.start + 1;

                if self.env.is_valid_variable(&identifier.text) ||
                    self.extra_allowed_variables.map_or(false, |vars| vars.contains(&identifier.text)) {
                    self.infer_multiplication(multiplication_range);
                    self.push_new_node(
                        AstNodeData::VariableReference(identifier.text.to_lowercase()),
                        identifier.range.clone(),
                    );
                    Ok(())
                } else if self.env.is_valid_function(&identifier.text) {
                    if self.index >= self.tokens.len() || !matches!(self.tokens[self.index].ty, TokenType::OpenBracket) {
                        error!(MissingOpeningBracket(identifier.range.end - 1..identifier.range.end));
                    }
                    self.infer_multiplication(multiplication_range);
                    self.next_function(identifier)?;
                    Ok(())
                } else {
                    let unit = match self.parse_unit(identifier)? {
                        Some(u) => u,
                        None => {
                            if matches!(self.last_token_ty, Some(TokenType::In)) {
                                error!(ExpectedUnit(identifier.range));
                            }
                            return Ok(());
                        }
                    };

                    let mut unit = Unit(unit, None);
                    // Handle unit denominator (e.g. for "km/h")
                    if self.index <= self.tokens.len() - 2 {
                        let next = &self.tokens[self.index];

                        if matches!(next.ty, TokenType::Divide) &&
                            next.range.start == identifier.range.end {
                            let second_unit_token = &self.tokens[self.index + 1];

                            if second_unit_token.ty == TokenType::Identifier {
                                match self.parse_unit(second_unit_token)? {
                                    Some(second_unit) => {
                                        unit.1 = Some(second_unit);
                                        self.index += 2;
                                    }
                                    None => {}
                                }
                            }
                        }
                    }

                    if matches!(self.last_token_ty, Some(TokenType::In)) {
                        self.push_new_node(AstNodeData::Unit(unit), identifier.range.clone());
                        return Ok(());
                    }

                    if !matches!(self.last_token_ty, Some(_)) || !self.last_token_ty.unwrap().is_number() {
                        error!(ExpectedNumber(identifier.range));
                    }

                    let last = self.result.last_mut().unwrap();
                    if last.unit.is_some() {
                        error!(UnexpectedUnit(identifier.range));
                    }

                    last.unit = Some(unit);
                    Ok(())
                }
            }
            _ => panic!("Must pass TokenType::Identifier to Parser::next_identifier()!"),
        }
    }

    fn parse_unit(&mut self, identifier: &Token) -> Result<Option<String>> {
        let last = self.result.last_mut();

        let first = identifier.text.chars().next().unwrap();
        let (mut unit, has_prefix) = if identifier.text.len() == 1 {
            if is_unit(&identifier.text) {
                (identifier.text.clone(), false)
            } else if is_prefix(first) {
                let power = get_prefix_power(first).unwrap();
                if let Some(last) = last {
                    last.modifiers.push(AstNodeModifier::Power(power));
                } else {
                    error!(ExpectedNumber(identifier.range));
                }
                return Ok(None);
            } else {
                error!(UnknownIdentifier(identifier.range));
            }
        } else if is_unit(&identifier.text) {
            (identifier.text.clone(), false)
        } else if is_prefix(first) {
            if !is_unit(&identifier.text[1..]) {
                error!(UnknownIdentifier(identifier.range));
            }
            (identifier.text.clone(), true)
        } else {
            error!(UnknownIdentifier(identifier.range));
        };

        // Handle units with exponentiation (e.g. m^2 (square meters))
        if self.index <= self.tokens.len() - 2 {
            let next = &self.tokens[self.index];

            if matches!(next.ty, TokenType::Exponentiation) &&
                next.range.start == identifier.range.end {
                let literal = &self.tokens[self.index + 1];
                if !literal.ty.is_literal() {
                    error!(UnknownIdentifier(identifier.range.start..next.range.end));
                }

                unit += &next.text;
                unit += &literal.text;
                if !is_unit(if has_prefix { &unit[1..] } else { &unit }) {
                    error!(UnknownIdentifier(identifier.range.start..literal.range.end));
                }

                self.index += 2;
            }
        }

        Ok(Some(unit))
    }

    fn next_function(&mut self, identifier: &Token) -> Result<()> {
        if !self.env.is_valid_function(&identifier.text) {
            error!(UnknownFunction(identifier.range));
        }

        let open_bracket = &self.tokens[self.index];
        self.index += 1;

        let mut arguments: Vec<Vec<AstNode>> = Vec::new();
        let mut argument_start = self.index;

        let mut finished = false;
        let mut nesting_level = 1usize;

        while let Some(token) = self.tokens.get(self.index) {
            self.index += 1;
            match token.ty {
                TokenType::Comma => {
                    if argument_start == self.index - 1 {
                        error!(ExpectedElements(self.tokens[self.index - 1].range));
                    }
                    let argument = &self.tokens[argument_start..self.index - 1];

                    match Parser::from(self, argument).parse()? {
                        ParserResult::Calculation(ast) => arguments.push(ast),
                        res => error_with_args!(ExpectedExpression(argument[0].range.start..argument.last().unwrap().range.end) res.to_string()),
                    }
                    argument_start = self.index;
                }
                TokenType::CloseBracket => {
                    nesting_level -= 1;
                    if nesting_level == 0 {
                        if argument_start != self.index - 1 {
                            let argument = &self.tokens[argument_start..self.index - 1];
                            match Parser::from(self, argument).parse()? {
                                ParserResult::Calculation(ast) => arguments.push(ast),
                                res => error_with_args!(ExpectedExpression(argument[0].range.start..argument.last().unwrap().range.end) res.to_string()),
                            }
                        }
                        finished = true;
                        break;
                    }
                }
                TokenType::OpenBracket => nesting_level += 1,
                _ => {}
            }
        }

        if !finished {
            error!(MissingClosingBracket(open_bracket.range));
        }

        let range = identifier.range.start..self.tokens[self.index - 1].range.end;
        let name = identifier.text.to_lowercase();

        let function_args_count = self.env.function_argument_count(&name).unwrap();
        if arguments.len() != function_args_count {
            let range = open_bracket.range.start..self.tokens[self.index - 1].range.end;
            error_with_args!(WrongNumberOfArguments(range) function_args_count);
        }

        self.push_new_node(
            AstNodeData::FunctionInvocation(name, arguments),
            range,
        );
        Ok(())
    }

    fn push_new_node(&mut self, data: AstNodeData, range: std::ops::Range<usize>) {
        let mut new_node = AstNode::new(data, range);
        new_node.modifiers = mem::take(&mut self.next_token_modifiers);
        self.result.push(new_node);
    }

    fn infer_multiplication(&mut self, range: std::ops::Range<usize>) {
        if let Some(last_ty) = self.last_token_ty {
            if last_ty.is_number() {
                self.result.push(AstNode::new(
                    AstNodeData::Operator(Operator::Multiply),
                    range,
                ));
            }
        }
    }

    fn verify_valid_token(&self, token: &Token) -> Result<()> {
        if token.ty == TokenType::CloseBracket {
            error!(MissingOpeningBracket(token.range));
        } else if token.ty == TokenType::Comma {
            error!(UnexpectedComma(token.range));
        }

        let mut allowed_tokens = self.all_tokens_tys.clone();

        #[allow(unused_parens)]
            let mut error_type = match self.last_token_ty {
            Some(ty) => {
                if ty != TokenType::In {
                    remove_elems!(allowed_tokens, (|i| i.is_format()));
                }

                if ty.is_number() {
                    remove_elems!(allowed_tokens, (|i| i.is_literal()));
                    ErrorType::ExpectedOperator
                } else if ty.is_operator() {
                    if ty == TokenType::In {
                        remove_elems!(allowed_tokens, (|i| !i.is_format()));
                        allowed_tokens.push(TokenType::Identifier);
                        ErrorType::ExpectedFormat
                    } else {
                        remove_elems!(allowed_tokens, (|i| {
                            (i.is_operator() && *i != TokenType::Minus && *i != TokenType::Plus)
                            || *i == TokenType::PercentSign
                        }));
                        ErrorType::ExpectedNumber
                    }
                } else if ty.is_format() {
                    remove_elems!(allowed_tokens, (|i| i.is_format() || !i.is_operator()));
                    ErrorType::ExpectedOperator
                } else {
                    unreachable!()
                }
            }
            None => {
                remove_elems!(allowed_tokens, (|i| {
                    (i.is_operator() && *i != TokenType::Minus && *i != TokenType::Plus) ||
                    *i == TokenType::PercentSign || i.is_format()
                }));
                if token.ty.is_operator() || token.ty == TokenType::PercentSign {
                    ErrorType::ExpectedNumber
                } else if token.ty.is_format() {
                    ErrorType::ExpectedIn
                } else {
                    ErrorType::Nothing
                }
            }
        };

        #[allow(unused_parens)]
        if self.last_token_ty.is_some() && self.index == self.tokens.len() {
            remove_elems!(allowed_tokens, (|i| i.is_operator()));
            if !token.ty.is_number() {
                error_type = ErrorType::ExpectedNumber;
            }
        }

        if !allowed_tokens.contains(&token.ty) {
            return Err(error_type.with(token.range.clone()));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::astgen::tokenizer::tokenize;
    use super::*;

    macro_rules! parse {
        ($input:expr) => {
            parse(&tokenize($input)?, &Environment::new())
        }
    }

    macro_rules! assert_error_type {
        ($result:expr, $variant:ident) => {
            assert_eq!($result.err().unwrap().error, ErrorType::$variant)
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
    fn equality_check() -> Result<()> {
        let res = parse!("3 = 3")?;
        match res {
            ParserResult::EqualityCheck(lhs, rhs) => {
                assert!(matches!(lhs[0].data, AstNodeData::Literal(_)));
                assert!(matches!(rhs[0].data, AstNodeData::Literal(_)));
            }
            _ => unreachable!(),
        }
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
        assert_error_type!(ast, UnknownIdentifier);
        Ok(())
    }

    #[test]
    fn reserved_variable() -> Result<()> {
        let err = parse!("pi :=");
        assert_error_type!(err, ReservedVariable);
        Ok(())
    }

    #[test]
    fn reserved_function() -> Result<()> {
        let err = parse!("sin(x) :=");
        assert_error_type!(err, ReservedFunction);
        Ok(())
    }

    #[test]
    fn duplicate_argument() -> Result<()> {
        let err = parse!("f(x, x) :=");
        assert_error_type!(err, DuplicateArgument);
        Ok(())
    }

    #[test]
    fn expected_unit() -> Result<()> {
        let err = parse!("3km in k");
        assert_error_type!(err, ExpectedUnit);
        Ok(())
    }
}
