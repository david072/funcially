use ::Format;
use crate::astgen::ast::{AstNode, Operator, AstNodeData, AstNodeModifier};
use crate::astgen::tokenizer::{Token, TokenType};
use crate::common::*;
use crate::variables::is_valid_variable;
use strum::IntoEnumIterator;
use std::mem;

pub fn parse(tokens: &[Token]) -> Result<Vec<AstNode>> {
    let mut parser = Parser::new(tokens);
    let result = parser.parse()?;
    Ok(result)
}

struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    all_tokens_tys: Vec<TokenType>,
    last_token_ty: Option<TokenType>,
    next_token_modifiers: Vec<AstNodeModifier>,
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

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Parser {
        Parser {
            tokens,
            index: 0,
            all_tokens_tys: TokenType::iter().collect(),
            last_token_ty: None,
            next_token_modifiers: Vec::new(),
            result: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<AstNode>> {
        while self.next()? {}

        let result = mem::take(&mut self.result);
        Ok(result)
    }

    pub fn next(&mut self) -> Result<bool> {
        if self.index >= self.tokens.len() {
            return Ok(false);
        }

        let token = &self.tokens[self.index];
        self.index += 1;

        self.verify_valid_token(token)?;

        // Handle modifiers
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
            _ => {}
        }

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

        // Handle groups
        if token.ty == TokenType::OpenBracket {
            self.next_group(token)?;
            self.last_token_ty = Some(token.ty);
            return Ok(true);
        }

        // Handle variables
        if token.ty == TokenType::Identifier {
            self.next_identifier(token)?;
            self.last_token_ty = Some(token.ty);
            return Ok(true);
        }

        self.last_token_ty = Some(token.ty);

        let data = match token.ty {
            TokenType::DecimalLiteral => {
                let number = match token.text.parse() {
                    Ok(number) => number,
                    Err(_) => return Err(ErrorType::InvalidNumber.with(token.range.clone())),
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
            return Err(ErrorType::MissingClosingBracket.with(open_bracket_token.range.clone()));
        }

        let group_range = group_start - 1..group_end;
        if group_end - group_start <= 1 {
            self.result.push(AstNode::new(AstNodeData::Literal(0.0), group_range));
            return Ok(());
        }

        let group_tokens = &self.tokens[group_start..group_end - 1];
        let group_ast = Parser::new(group_tokens).parse()?;

        self.infer_multiplication(group_start..group_start + 1);
        self.push_new_node(AstNodeData::Group(group_ast), group_range);

        Ok(())
    }

    fn next_identifier(&mut self, identifier: &Token) -> Result<()> {
        match identifier.ty {
            TokenType::Identifier => {
                if !is_valid_variable(&identifier.text) {
                    return Err(ErrorType::UnknownVariable.with(identifier.range.clone()));
                }

                self.infer_multiplication(identifier.range.start..identifier.range.start + 1);
                self.push_new_node(
                    AstNodeData::VariableReference(identifier.text.clone()),
                    identifier.range.clone(),
                );

                Ok(())
            }
            _ => panic!("Must pass TokenType::Identifier to Parser::next_identifier()!"),
        }
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
            return Err(ErrorType::MissingClosingBracket.with(token.range.clone()));
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

    #[test]
    fn basic() -> Result<()> {
        let ast = parse!("1 - 3 + 4 * 5 / 6")?;
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
        let ast = parse!("2! + 3% + !4 + 3!%")?;
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
        let ast = parse!("20% of 3 ^ 2 & 1 | 4")?;
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
    fn expected_operand() -> Result<()> {
        let ast = parse!("2 3 + 4");
        assert!(ast.is_err());
        assert_eq!(ast.err().unwrap().error, ErrorType::ExpectedOperator);
        Ok(())
    }

    #[test]
    fn expected_number() -> Result<()> {
        let ast = parse!("2 ++ 4");
        assert_eq!(ast.err().unwrap().error, ErrorType::ExpectedNumber);
        let ast = parse!("2 +");
        assert_eq!(ast.err().unwrap().error, ErrorType::ExpectedNumber);
        Ok(())
    }

    #[test]
    fn percent_expected_number() -> Result<()> {
        let ast = parse!("3 + %");
        assert_eq!(ast.err().unwrap().error, ErrorType::ExpectedNumber);
        let ast = parse!("%");
        assert_eq!(ast.err().unwrap().error, ErrorType::ExpectedNumber);
        Ok(())
    }
}