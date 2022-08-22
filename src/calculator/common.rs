use std::ops::Range;

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorType {
    /// Not actually an error. Used when e.g.
    /// a variable needs a value, but will never be used.
    Nothing,
    // tokenizer
    InvalidCharacter,
    InvalidNumber,

    // parser
    ExpectedNumber,
    ExpectedOperator,

    // engine
    DivideByZero,
    ExpectedInteger,
    /// This should never happen
    InvalidAst,
}

impl ErrorType {
    pub fn with(self, range: Range<usize>) -> Error {
        Error {
            error: self,
            start: range.start,
            end: range.end,
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub error: ErrorType,
    pub start: usize,
    pub end: usize,
}

pub type Result<T> = std::result::Result<T, Error>;
