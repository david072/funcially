use ::Format;
use crate::astgen::ast::{AstNode, Operator, AstNodeData, AstNodeModifier};
use crate::astgen::tokenizer::{Token, TokenType};
use crate::common::*;
use crate::variables::is_valid_variable;
use strum::IntoEnumIterator;
use std::mem;
use functions::{get_arguments_count, is_valid_function};
use units::is_valid_unit;

pub enum ParserResult {
    Calculation(Vec<AstNode>),
    EqualityCheck(Vec<AstNode>, Vec<AstNode>),
}

pub fn parse(tokens: &[Token]) -> Result<ParserResult> {
    let mut parser = Parser::new(tokens, 0);
    let result = parser.parse()?;
    Ok(result)
}

struct Parser<'a> {
    tokens: &'a [Token],
    nesting_level: usize,

    index: usize,
    all_tokens_tys: Vec<TokenType>,
    last_token_ty: Option<TokenType>,
    next_token_modifiers: Vec<AstNodeModifier>,
    equals_sign_index: Option<usize>,
    result: Vec<AstNode>,
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

macro_rules! error {
    ($variant:ident($range:expr)) => {
        return Err(ErrorType::$variant.with($range.clone()))
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], nesting_level: usize) -> Parser {
        Parser {
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

    pub fn parse(&mut self) -> Result<ParserResult> {
        while self.next()? {}

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

                self.equals_sign_index = Some(self.index - 1);
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

        let group_range = group_start - 1..group_end;
        if group_end - group_start <= 1 {
            self.result.push(AstNode::new(AstNodeData::Literal(0.0), group_range));
            return Ok(());
        }

        let group_tokens = &self.tokens[group_start..group_end - 1];
        let group_ast = match Parser::new(group_tokens, self.nesting_level + 1).parse()? {
            ParserResult::Calculation(ast) => ast,
            ParserResult::EqualityCheck(_, _) => unreachable!(),
        };

        self.infer_multiplication(group_start..group_start + 1);
        self.push_new_node(AstNodeData::Group(group_ast), group_range);

        Ok(())
    }

    fn next_identifier(&mut self, identifier: &Token) -> Result<()> {
        match identifier.ty {
            TokenType::Identifier => {
                let multiplication_range = identifier.range.start..identifier.range.start + 1;

                if is_valid_variable(&identifier.text) {
                    self.infer_multiplication(multiplication_range);
                    self.push_new_node(
                        AstNodeData::VariableReference(identifier.text.clone()),
                        identifier.range.clone(),
                    );
                    Ok(())
                } else if is_valid_function(&identifier.text) {
                    if self.index >= self.tokens.len() || !matches!(self.tokens[self.index].ty, TokenType::OpenBracket) {
                        error!(MissingOpeningBracket(identifier.range.end..identifier.range.end + 1));
                    }
                    self.infer_multiplication(multiplication_range);
                    self.next_function(identifier)?;
                    Ok(())
                } else if is_valid_unit(&identifier.text) {
                    if matches!(self.last_token_ty, Some(TokenType::In)) {
                        self.push_new_node(AstNodeData::Unit(identifier.text.clone()), identifier.range.clone());
                        return Ok(());
                    }

                    if !matches!(self.last_token_ty, Some(_)) || !self.last_token_ty.unwrap().is_number() {
                        error!(ExpectedNumber(identifier.range));
                    }

                    let last = self.result.last_mut().unwrap();
                    if last.unit.is_some() {
                        error!(UnexpectedUnit(identifier.range));
                    }
                    self.result.last_mut().unwrap().unit = Some(identifier.text.clone());
                    Ok(())
                } else {
                    error!(UnknownIdentifier(identifier.range));
                }
            }
            _ => panic!("Must pass TokenType::Identifier to Parser::next_identifier()!"),
        }
    }

    fn next_function(&mut self, identifier: &Token) -> Result<()> {
        if !is_valid_function(&identifier.text) {
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
                    match parse(argument)? {
                        ParserResult::Calculation(ast) => arguments.push(ast),
                        ParserResult::EqualityCheck(_, _) => unreachable!(),
                    }
                    argument_start = self.index;
                }
                TokenType::CloseBracket => {
                    nesting_level -= 1;
                    if nesting_level == 0 {
                        if argument_start != self.index - 1 {
                            let argument = &self.tokens[argument_start..self.index - 1];
                            match parse(argument)? {
                                ParserResult::Calculation(ast) => arguments.push(ast),
                                ParserResult::EqualityCheck(_, _) => unreachable!(),
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

        let range = open_bracket.range.start..self.tokens[self.index - 1].range.end;
        if arguments.len() != get_arguments_count(&identifier.text).unwrap() {
            error!(WrongNumberOfArguments(range));
        }

        self.push_new_node(
            AstNodeData::FunctionInvocation(identifier.text.clone(), arguments),
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
                        remove_elems!(allowed_tokens, (|i| i.is_operator() || *i == TokenType::PercentSign));
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
                remove_elems!(allowed_tokens, (|i| *i == TokenType::PercentSign || i.is_format() || i.is_operator()));
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
            parse(&tokenize($input)?)
        }
    }

    macro_rules! assert_error_type {
        ($result:expr, $variant:ident) => {
            assert_eq!($result.err().unwrap().error, ErrorType::$variant)
        }
    }

    macro_rules! calculation {
        ($parser_res:expr) => {
            if let ParserResult::Calculation(ast) = $parser_res {
                ast
            } else {
                panic!("Expected ParserResult::Calculation");
            }
        }
    }

    #[test]
    fn basic() -> Result<()> {
        let ast = calculation!(parse!("1 - 3 + 4 * 5 / 6")?);
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
        let ast = calculation!(parse!("2! + 3% + !4 + 3!%")?);
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
        let ast = calculation!(parse!("20% of 3 ^ 2 & 1 | 4")?);
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
        let ast = calculation!(parse!("(1 + (1 + 1)) * 2")?);
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
        let ast = calculation!(parse!("pi")?);
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
        let ast = calculation!(parse!("sin(30)")?);
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
        let ast = calculation!(parse!("2(1)")?);
        assert!(ast.len() == 3 && matches!(ast[1].data, AstNodeData::Operator(Operator::Multiply)));
        let ast = calculation!(parse!("2pi")?);
        assert!(ast.len() == 3 && matches!(ast[1].data, AstNodeData::Operator(Operator::Multiply)));
        let ast = calculation!(parse!("2sin(30)")?);
        assert!(ast.len() == 3 && matches!(ast[1].data, AstNodeData::Operator(Operator::Multiply)));
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
        let ast = parse!("2 ++ 4");
        assert_error_type!(ast, ExpectedNumber);
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
    fn unknown_variable() -> Result<()> {
        let ast = parse!("asdf");
        assert_error_type!(ast, UnknownVariable);
        Ok(())
    }
}
