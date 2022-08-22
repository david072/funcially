use crate::common::*;
use crate::astgen::ast::{AstNode, Operator, AstNodeData};
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
            Some(ty) => {
                if ty.is_literal() {
                    remove_elems!(allowed_tokens, (|i| i.is_literal()));
                    ErrorType::ExpectedOperator
                } else if ty.is_operator() {
                    remove_elems!(allowed_tokens, (|i| i.is_operator()));
                    ErrorType::ExpectedNumber
                } else {
                    unreachable!()
                }
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
            TokenType::Plus => Ok(AstNodeData::Operator(Operator::Plus)),
            TokenType::Minus => Ok(AstNodeData::Operator(Operator::Minus)),
            TokenType::Multiply => Ok(AstNodeData::Operator(Operator::Multiply)),
            TokenType::Divide => Ok(AstNodeData::Operator(Operator::Divide)),
            TokenType::Exponentiation => Ok(AstNodeData::Operator(Operator::Exponentiation)),
            TokenType::BitwiseAnd => Ok(AstNodeData::Operator(Operator::BitwiseAnd)),
            TokenType::BitwiseOr => Ok(AstNodeData::Operator(Operator::BitwiseOr)),
            _ => unreachable!(),
        }?;
        Ok(Some(AstNode::new(data, token.range.clone())))
    }
}

#[cfg(test)]
mod tests {
    use crate::astgen::tokenizer::tokenize;
    use super::*;

    #[test]
    fn basic() -> Result<()> {
        let ast = parse(&tokenize("1 - 3 + 4 * 5 / 6")?)?;
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