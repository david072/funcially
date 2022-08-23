use crate::common::*;
use strum::{EnumIter};
use std::ops::Range;

#[derive(Debug, EnumIter, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Whitespace,
    DecimalLiteral,
    HexLiteral,
    BinaryLiteral,
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponentiation,
    BitwiseAnd,
    BitwiseOr,
    ExclamationMark,
    PercentSign,
    Of,
}

impl TokenType {
    pub fn is_literal(&self) -> bool {
        matches!(self, Self::DecimalLiteral | Self::HexLiteral | Self::BinaryLiteral)
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Self::Plus
            | Self::Minus
            | Self::Multiply
            | Self::Divide
            | Self::Exponentiation
            | Self::BitwiseAnd
            | Self::BitwiseOr
            | Self::Of)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Token {
    pub ty: TokenType,
    pub text: String,
    pub range: Range<usize>,
}

pub fn tokenize(input: &str) -> Result<Vec<Token>> {
    let mut tokenizer = Tokenizer::new(input.as_bytes());
    let mut result = Vec::new();

    while let Some(token) = tokenizer.next()? {
        match token.ty {
            TokenType::Whitespace => continue,
            _ => result.push(token),
        }
    }

    Ok(result)
}

const NUMBERS: &[u8] = b"0123456789";
const HEXADECIMAL_CHARS: &[u8] = b"0123456789abcdefABCDEF";
const BINARY_DIGITS: &[u8] = b"01";
const WHITESPACE: &[u8] = b" \t\r\n";

fn any_of(chars: &[u8]) -> impl Fn(u8) -> bool + '_ {
    move |c| chars.contains(&c)
}

struct Tokenizer<'a> {
    string: &'a [u8],
    index: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(string: &'a [u8]) -> Tokenizer {
        Tokenizer {
            string,
            index: 0,
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>> {
        if self.index >= self.string.len() {
            return Ok(None);
        }

        let start = self.index;
        let next_ty = self.next_type();
        let end = self.index;

        match next_ty {
            Some(ty) => {
                let slice = self.string[start..end].to_owned();
                Ok(Some(Token {
                    ty,
                    text: String::from_utf8(slice).unwrap(),
                    range: start..std::cmp::max(0, end),
                }))
            }
            None => Err(ErrorType::InvalidCharacter.with(start..end)),
        }
    }

    fn accept<F: Fn(u8) -> bool>(&mut self, predicate: F) -> bool {
        if self.index >= self.string.len() {
            return false;
        }

        if predicate(self.string[self.index]) {
            self.index += 1;
            return true;
        }

        false
    }

    fn next_type(&mut self) -> Option<TokenType> {
        if self.accept(any_of(WHITESPACE)) {
            while self.accept(any_of(WHITESPACE)) {}
            return Some(TokenType::Whitespace);
        }

        let c = self.string[self.index];
        self.index += 1;
        match c {
            b'0'..=b'9' => {
                if c == b'0' && self.index < self.string.len() {
                    // check next character for different representation
                    let c = self.string[self.index];
                    self.index += 1;
                    match c {
                        b'x' | b'X' => {
                            while self.accept(any_of(HEXADECIMAL_CHARS)) {}
                            return Some(TokenType::HexLiteral);
                        }
                        b'b' | b'B' => {
                            while self.accept(any_of(BINARY_DIGITS)) {}
                            return Some(TokenType::BinaryLiteral);
                        }
                        // fall through to after the if
                        b'0'..=b'9' | b'.' => {}
                        _ => {
                            // the character needs to be processed in the next iteration
                            self.index -= 1;
                            return Some(TokenType::DecimalLiteral);
                        }
                    }
                }

                while self.accept(any_of(NUMBERS)) {}
                self.accept(any_of(b"."));
                while self.accept(any_of(NUMBERS)) {}
                Some(TokenType::DecimalLiteral)
            }
            b'.' => {
                while self.accept(any_of(NUMBERS)) {}
                Some(TokenType::DecimalLiteral)
            }
            // number sign
            b'+' | b'-' if self.index < self.string.len() &&
                matches!(self.string[self.index], b'0'..=b'9' | b'.') => {
                self.index += 1;
                while self.accept(any_of(NUMBERS)) {}
                Some(TokenType::DecimalLiteral)
            }
            b'o' | b'O' if self.index < self.string.len() &&
                matches!(self.string[self.index], b'f' | b'F') => {
                self.index += 1;
                Some(TokenType::Of)
            }
            b'+' => Some(TokenType::Plus),
            b'-' => Some(TokenType::Minus),
            b'*' => Some(TokenType::Multiply),
            b'/' => Some(TokenType::Divide),
            b'^' => Some(TokenType::Exponentiation),
            b'&' => Some(TokenType::BitwiseAnd),
            b'|' => Some(TokenType::BitwiseOr),
            b'!' => Some(TokenType::ExclamationMark),
            b'%' => Some(TokenType::PercentSign),
            _ => None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Token {
        fn new(ty: TokenType, text: &str, range: Range<usize>) -> Token {
            Token { ty, text: text.to_owned(), range }
        }
    }

    #[test]
    fn literals() -> Result<()> {
        let tokens = tokenize("3 0x0123456789 0xABCdef 0b110")?;
        assert_eq!(tokens, vec![
            Token::new(TokenType::DecimalLiteral, "3", 0..1),
            Token::new(TokenType::HexLiteral, "0x0123456789", 2..14),
            Token::new(TokenType::HexLiteral, "0xABCdef", 15..23),
            Token::new(TokenType::BinaryLiteral, "0b110", 24..29),
        ]);
        Ok(())
    }

    #[test]
    fn operators() -> Result<()> {
        let tokens = tokenize("+ - * /")?;
        assert_eq!(tokens, vec![
            Token::new(TokenType::Plus, "+", 0..1),
            Token::new(TokenType::Minus, "-", 2..3),
            Token::new(TokenType::Multiply, "*", 4..5),
            Token::new(TokenType::Divide, "/", 6..7),
        ]);
        Ok(())
    }

    #[test]
    fn extended_operators() -> Result<()> {
        let tokens = tokenize("^ & | ! of %")?;
        assert_eq!(tokens, vec![
            Token::new(TokenType::Exponentiation, "^", 0..1),
            Token::new(TokenType::BitwiseAnd, "&", 2..3),
            Token::new(TokenType::BitwiseOr, "|", 4..5),
            Token::new(TokenType::ExclamationMark, "!", 6..7),
            Token::new(TokenType::Of, "of", 8..10),
            Token::new(TokenType::PercentSign, "%", 11..12),
        ]);
        Ok(())
    }

    #[test]
    fn floats() -> Result<()> {
        let tokens = tokenize("0.23 .23")?;
        assert_eq!(tokens, vec![
            Token::new(TokenType::DecimalLiteral, "0.23", 0..4),
            Token::new(TokenType::DecimalLiteral, ".23", 5..8),
        ]);
        Ok(())
    }

    #[test]
    fn signs() -> Result<()> {
        let tokens = tokenize("-3 +3 -.3")?;
        assert_eq!(tokens, vec![
            Token::new(TokenType::DecimalLiteral, "-3", 0..2),
            Token::new(TokenType::DecimalLiteral, "+3", 3..5),
            Token::new(TokenType::DecimalLiteral, "-.3", 6..9),
        ]);
        Ok(())
    }
}