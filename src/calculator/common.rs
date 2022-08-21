#[derive(Debug)]
pub enum ErrorType {}

pub struct Error {
    pub start: usize,
    pub end: usize,
    pub error: ErrorType,
}

pub type Result<T> = std::result::Result<T, Error>;
