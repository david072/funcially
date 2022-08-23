use crate::common::*;
use crate::astgen::ast::{AstNode, Operator, AstNodeData, AstNodeModifier};
use crate::astgen::tokenizer::{Token, TokenType};
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
        while self.next()?.is_some() {}

        let result = mem::take(&mut self.result);
        Ok(result)
    }

    pub fn next(&mut self) -> Result<Option<()>> {
        if self.index >= self.tokens.len() {
            return Ok(None);
        }

        let token = &self.tokens[self.index];
        self.index += 1;

        let mut allowed_tokens = self.all_tokens_tys.clone();

        #[allow(unused_parens)]
            let mut error_type = match self.last_token_ty {
            Some(ty) => {
                if ty.is_literal() {
                    remove_elems!(allowed_tokens, (|i| i.is_literal()));
                    ErrorType::ExpectedOperator
                } else if ty.is_operator() {
                    remove_elems!(allowed_tokens, (|i| i.is_operator() || *i == TokenType::PercentSign));
                    ErrorType::ExpectedNumber
                } else {
                    unreachable!()
                }
            }
            // if this is matched, there should never be an error
            None => ErrorType::Nothing,
        };

        #[allow(unused_parens)]
        if self.last_token_ty.is_none() {
            remove_elems!(allowed_tokens, (|i| *i == TokenType::PercentSign));
            error_type = ErrorType::ExpectedNumber;
        }

        #[allow(unused_parens)]
        if self.index == self.tokens.len() {
            remove_elems!(allowed_tokens, (|i| i.is_operator()));
            error_type = ErrorType::ExpectedNumber;
        }

        if !allowed_tokens.contains(&token.ty) {
            return Err(error_type.with(token.range.clone()));
        }

        // Handle modifiers
        match token.ty {
            TokenType::ExclamationMark => {
                return if let Some(last_ty) = self.last_token_ty {
                    if !last_ty.is_operator() {
                        // Exclamation mark is factorial
                        let last_node = self.result.last_mut().unwrap();
                        last_node.modifiers.push(AstNodeModifier::Factorial);
                        Ok(Some(()))
                    } else {
                        self.next_token_modifiers.push(AstNodeModifier::BitwiseNot);
                        Ok(Some(()))
                    }
                } else {
                    self.next_token_modifiers.push(AstNodeModifier::BitwiseNot);
                    Ok(Some(()))
                };
            }
            TokenType::PercentSign => {
                let last_node = self.result.last_mut().unwrap();
                last_node.modifiers.push(AstNodeModifier::Percent);
                return Ok(Some(()));
            }
            _ => {}
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
            _ => unreachable!(),
        }?;

        let mut new_node = AstNode::new(data, token.range.clone());
        new_node.modifiers = mem::take(&mut self.next_token_modifiers);
        self.result.push(new_node);
        Ok(Some(()))
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
        assert_eq!(ast.iter().map(|n| n.data).collect::<Vec<_>>(), vec![
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
                       .map(|n| n.data)
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