use std::ops::Range;

#[derive(Debug)]
pub enum ErrorType {
    InvalidCharacter,
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
