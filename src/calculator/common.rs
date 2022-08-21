use std::ops::Range;

#[derive(Debug)]
pub enum ErrorType {
    /// Not actually an error. Used when e.g.
    /// a variable needs a value, but will never be used.
    Nothing,
    InvalidCharacter,
    InvalidNumber,
    ExpectedNumber,
    ExpectedOperator,
}

pub struct Error {
    pub error: ErrorType,
    pub start: usize,
    pub end: usize,
}

impl Error {
    pub fn new(error: ErrorType, range: Range<usize>) -> Error {
        Error {
            error,
            start: range.start,
            end: range.end,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
