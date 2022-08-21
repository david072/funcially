use crate::calculator::common::*;
use crate::calculator::ast::{AstNode, Operator};
use crate::calculator::tokenizer::{Token, TokenType};
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
        let error_type: ErrorType;

        #[allow(unused_parens)]
        match self.last_token_ty {
            Some(ty) => match ty {
                TokenType::Literal(_) => {
                    remove_elems!(allowed_tokens, (|i| matches!(i, TokenType::Literal(_))));
                    error_type = ErrorType::ExpectedOperator;
                }
                TokenType::Plus | TokenType::Minus | TokenType::Multiply | TokenType::Divide => {
                    remove_elems!(allowed_tokens, (|i| i.is_operator()));
                    error_type = ErrorType::ExpectedNumber;
                }
                _ => unreachable!()
            }
            // if this is matched, there should never be an error
            None => error_type = ErrorType::Nothing,
        }

        if !allowed_tokens.contains(&token.ty) {
            // since `self.all_token_tys` contains TokenType::Literal(0), there
            // has to be special logic for literals
            if matches!(token.ty, TokenType::Literal(_)) {
                if !allowed_tokens.contains(&TokenType::Literal(0)) {
                    return Err(Error::new(error_type, token.start..token.end));
                }
            } else {
                return Err(Error::new(error_type, token.start..token.end));
            }
        }

        self.last_token_ty = Some(token.ty);

        match &token.ty {
            TokenType::Literal(radix) => {
                let number: f64 = if *radix == 10 {
                    match token.text.parse() {
                        Ok(number) => number,
                        Err(_) => return Err(Error::new(ErrorType::InvalidNumber, token.start..token.end)),
                    }
                } else {
                    (match i64::from_str_radix(&token.text[2..], *radix) {
                        Ok(number) => number,
                        Err(_) => return Err(Error::new(ErrorType::InvalidNumber, token.start..token.end)),
                    }) as f64
                };
                Ok(Some(AstNode::Literal(number)))
            }
            TokenType::Plus => Ok(Some(AstNode::Operator(Operator::Plus))),
            TokenType::Minus => Ok(Some(AstNode::Operator(Operator::Minus))),
            TokenType::Multiply => Ok(Some(AstNode::Operator(Operator::Multiply))),
            TokenType::Divide => Ok(Some(AstNode::Operator(Operator::Divide))),
            _ => unreachable!(),
        }
    }
}
