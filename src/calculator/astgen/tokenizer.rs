use crate::common::*;
use strum::{EnumIter};
use std::ops::Range;

#[derive(Debug, EnumIter, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Whitespace,
    // Literals
    DecimalLiteral,
    HexLiteral,
    BinaryLiteral,
    // Brackets
    OpenBracket,
    CloseBracket,
    // Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponentiation,
    BitwiseAnd,
    BitwiseOr,
    Of,
    In,
    // Modifiers
    ExclamationMark,
    PercentSign,
    // Formats
    Decimal,
    Hex,
    Binary,
    // Identifier
    Identifier,
    Comma,
    EqualsSign,
}

impl TokenType {
    pub fn is_literal(&self) -> bool {
        matches!(self, Self::DecimalLiteral
            | Self::HexLiteral
            | Self::BinaryLiteral)
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Self::DecimalLiteral
            | Self::HexLiteral
            | Self::BinaryLiteral
            | Self::OpenBracket
            | Self::CloseBracket
            | Self::Identifier)
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Self::Plus
            | Self::Minus
            | Self::Multiply
            | Self::Divide
            | Self::Exponentiation
            | Self::BitwiseAnd
            | Self::BitwiseOr
            | Self::Of
            | Self::In
            | Self::EqualsSign) // '=' has the same rules as an operator
    }

    pub fn is_format(&self) -> bool {
        matches!(self, Self::Decimal | Self::Hex | Self::Binary)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Token {
    pub ty: TokenType,
    pub text: String,
    pub range: Range<usize>,
}

pub fn tokenize(input: &str) -> Result<Vec<Token>> {
    let chars = input.chars().collect::<Vec<_>>();
    let mut tokenizer = Tokenizer::new(&chars);
    let mut result = Vec::new();

    while let Some(token) = tokenizer.next()? {
        match token.ty {
            TokenType::Whitespace => continue,
            _ => result.push(token),
        }
    }

    Ok(result)
}

const NUMBERS: &str = "0123456789";
const LETTERS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ°";
const HEXADECIMAL_CHARS: &str = "0123456789abcdefABCDEF";
const BINARY_DIGITS: &str = "01";
const WHITESPACE: &str = " \t\r\n";

fn any_of(chars: &str) -> impl Fn(char) -> bool + '_ {
    move |c| chars.contains(&String::from(c))
}

struct Tokenizer<'a> {
    string: &'a [char],
    index: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(string: &'a [char]) -> Tokenizer {
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
            Some(mut ty) => {
                let slice = self.string[start..end].iter().collect::<String>();

                if ty == TokenType::Identifier {
                    ty = match slice.to_lowercase().as_str() {
                        "of" => TokenType::Of,
                        "in" => TokenType::In,
                        "decimal" => TokenType::Decimal,
                        "hex" => TokenType::Hex,
                        "binary" => TokenType::Binary,
                        _ => ty,
                    };
                }

                Ok(Some(Token {
                    ty,
                    text: slice,
                    range: start..std::cmp::max(0, end),
                }))
            }
            None => Err(ErrorType::InvalidCharacter.with(start..end)),
        }
    }

    fn accept<F: Fn(char) -> bool>(&mut self, predicate: F) -> bool {
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
        let res = match c {
            '0'..='9' => {
                if c == '0' && self.index < self.string.len() {
                    // check next character for different representation
                    let c = self.string[self.index];
                    self.index += 1;
                    match c {
                        'x' | 'X' => {
                            while self.accept(any_of(HEXADECIMAL_CHARS)) {}
                            return Some(TokenType::HexLiteral);
                        }
                        'b' | 'B' => {
                            while self.accept(any_of(BINARY_DIGITS)) {}
                            return Some(TokenType::BinaryLiteral);
                        }
                        // fall through to after the if
                        '0'..='9' | '.' => {}
                        _ => {
                            // the character needs to be processed in the next iteration
                            self.index -= 1;
                            return Some(TokenType::DecimalLiteral);
                        }
                    }
                }

                while self.accept(any_of(NUMBERS)) {}
                self.accept(any_of("."));
                while self.accept(any_of(NUMBERS)) {}
                Some(TokenType::DecimalLiteral)
            }
            '.' => {
                while self.accept(any_of(NUMBERS)) {}
                Some(TokenType::DecimalLiteral)
            }
            // number sign
            '+' | '-' if self.index < self.string.len() &&
                matches!(self.string[self.index], '0'..='9' | '.') => {
                self.index += 1;
                while self.accept(any_of(NUMBERS)) {}
                Some(TokenType::DecimalLiteral)
            }
            '+' => Some(TokenType::Plus),
            '-' => Some(TokenType::Minus),
            '*' => Some(TokenType::Multiply),
            '/' => Some(TokenType::Divide),
            '^' => Some(TokenType::Exponentiation),
            '&' => Some(TokenType::BitwiseAnd),
            '|' => Some(TokenType::BitwiseOr),
            '!' => Some(TokenType::ExclamationMark),
            '%' => Some(TokenType::PercentSign),
            '(' => Some(TokenType::OpenBracket),
            ')' => Some(TokenType::CloseBracket),
            '=' => Some(TokenType::EqualsSign),
            ',' => Some(TokenType::Comma),
            _ => None
        };

        if res.is_some() { return res; }

        if LETTERS.contains(&String::from(c)) {
            while self.accept(any_of(LETTERS)) {}
            return Some(TokenType::Identifier);
        }

        None
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
            Token::new(TokenType::HexLiteral, "0xabcdef", 15..23),
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
        let tokens = tokenize("^ & | ! of % =")?;
        assert_eq!(tokens, vec![
            Token::new(TokenType::Exponentiation, "^", 0..1),
            Token::new(TokenType::BitwiseAnd, "&", 2..3),
            Token::new(TokenType::BitwiseOr, "|", 4..5),
            Token::new(TokenType::ExclamationMark, "!", 6..7),
            Token::new(TokenType::Of, "of", 8..10),
            Token::new(TokenType::PercentSign, "%", 11..12),
            Token::new(TokenType::EqualsSign, "=", 13..14),
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

    #[test]
    fn groups() -> Result<()> {
        let tokens = tokenize("()")?;
        assert_eq!(tokens, vec![
            Token::new(TokenType::OpenBracket, "(", 0..1),
            Token::new(TokenType::CloseBracket, ")", 1..2),
        ]);
        Ok(())
    }

    #[test]
    fn identifiers() -> Result<()> {
        let tokens = tokenize("1 test tset")?;
        assert_eq!(tokens[1..], vec![
            Token::new(TokenType::Identifier, "test", 2..6),
            Token::new(TokenType::Identifier, "tset", 7..11),
        ]);
        Ok(())
    }
}