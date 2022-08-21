use crate::calculator::common::{Result, Error, ErrorType};

#[derive(Debug)]
pub enum TokenType {
    Whitespace,
    Literal,
    Plus,
    Minus,
    Multiply,
    Divide,
}

pub struct Token {
    pub ty: TokenType,
    pub text: String,
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
const OCTAL_DIGITS: &[u8] = b"01234567";
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
                }))
            }
            None => Err(Error::new(ErrorType::InvalidCharacter, start..start)),
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
                if c == b'0' {
                    // check next character for different representation
                    let c = self.string[self.index];
                    self.index += 1;
                    match c {
                        b'x' | b'X' => {
                            while self.accept(any_of(HEXADECIMAL_CHARS)) {}
                            return Some(TokenType::Literal);
                        }
                        b'b' | b'B' => {
                            while self.accept(any_of(BINARY_DIGITS)) {}
                            return Some(TokenType::Literal);
                        }
                        b'o' | b'O' => {
                            while self.accept(any_of(OCTAL_DIGITS)) {}
                            return Some(TokenType::Literal);
                        }
                        // fall through to after the if
                        b'0'..=b'9' => {}
                        _ => {
                            // the character needs to be processed in the next iteration
                            self.index -= 1;
                            return Some(TokenType::Literal);
                        }
                    }
                }

                while self.accept(any_of(NUMBERS)) {}
                Some(TokenType::Literal)
            }
            b'+' => Some(TokenType::Plus),
            b'-' => Some(TokenType::Minus),
            b'*' => Some(TokenType::Multiply),
            b'/' => Some(TokenType::Divide),
            _ => None
        }
    }
}
