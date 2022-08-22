use crate::common::*;
use crate::astgen::ast::{AstNode, Operator};
use crate::astgen::tokenizer::{Token, TokenType};
use strum::IntoEnumIterator;

pub fn parse(tokens: &[Token]) -> Result<Vec<AstNode>> {
    let mut parser = Parser::new(tokens);
    let mut result = Vec::new();

    while let Some(node) = parser.next()? {
        result.push(node);
    }

    Ok(result)
}

struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    last_token_ty: Option<TokenType>,
    all_tokens_tys: Vec<TokenType>,
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

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Parser {
        Parser {
            tokens,
            index: 0,
            last_token_ty: None,
            all_tokens_tys: TokenType::iter().collect(),
        }
    }

    pub fn next(&mut self) -> Result<Option<AstNode>> {
        if self.index >= self.tokens.len() {
            return Ok(None);
        }

        let token = &self.tokens[self.index];
        self.index += 1;

        let mut allowed_tokens = self.all_tokens_tys.clone();

        #[allow(unused_parens)]
            let mut error_type = match self.last_token_ty {
            Some(ty) => match ty {
                TokenType::DecimalLiteral | TokenType::HexLiteral | TokenType::BinaryLiteral => {
                    remove_elems!(allowed_tokens, (|i| i.is_literal()));
                    ErrorType::ExpectedOperator
                }
                TokenType::Plus | TokenType::Minus | TokenType::Multiply | TokenType::Divide => {
                    remove_elems!(allowed_tokens, (|i| i.is_operator()));
                    ErrorType::ExpectedNumber
                }
                _ => unreachable!()
            }
            // if this is matched, there should never be an error
            None => ErrorType::Nothing,
        };

        #[allow(unused_parens)]
        if self.index == self.tokens.len() {
            remove_elems!(allowed_tokens, (|i| i.is_operator()));
            error_type = ErrorType::ExpectedNumber;
        }

        if !allowed_tokens.contains(&token.ty) {
            return Err(error_type.with(token.range.clone()));
        }

        self.last_token_ty = Some(token.ty);

        match token.ty {
            TokenType::DecimalLiteral => {
                let number = match token.text.parse() {
                    Ok(number) => number,
                    Err(_) => return Err(ErrorType::InvalidNumber.with(token.range.clone())),
                };
                Ok(Some(AstNode::Literal(number)))
            }
            TokenType::HexLiteral => Ok(Some(AstNode::Literal(parse_f64_radix!(token, 16)))),
            TokenType::BinaryLiteral => Ok(Some(AstNode::Literal(parse_f64_radix!(token, 2)))),
            TokenType::Plus => Ok(Some(AstNode::Operator(Operator::Plus))),
            TokenType::Minus => Ok(Some(AstNode::Operator(Operator::Minus))),
            TokenType::Multiply => Ok(Some(AstNode::Operator(Operator::Multiply))),
            TokenType::Divide => Ok(Some(AstNode::Operator(Operator::Divide))),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::astgen::tokenizer::tokenize;
    use super::*;

    #[test]
    fn basic() -> Result<()> {
        let ast = parse(&tokenize("1 - 3 + 4 * 5 / 6")?)?;
        assert_eq!(ast, vec![
            AstNode::Literal(1.0),
            AstNode::Operator(Operator::Minus),
            AstNode::Literal(3.0),
            AstNode::Operator(Operator::Plus),
            AstNode::Literal(4.0),
            AstNode::Operator(Operator::Multiply),
            AstNode::Literal(5.0),
            AstNode::Operator(Operator::Divide),
            AstNode::Literal(6.0),
        ]);
        Ok(())
    }

    #[test]
    fn expected_operand() -> Result<()> {
        let ast = parse(&tokenize("2 3 + 4")?);
        assert!(ast.is_err());
        assert_eq!(ast.err().unwrap().error, ErrorType::ExpectedOperator);
        Ok(())
    }

    #[test]
    fn expected_number() -> Result<()> {
        let ast = parse(&tokenize("2 ++ 4")?);
        assert_eq!(ast.err().unwrap().error, ErrorType::ExpectedNumber);
        Ok(())
    }
}