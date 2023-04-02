/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::{Display, Formatter};
use std::path::PathBuf;

use thiserror::Error;

const CRATE_NAME: &str = "funcially";

#[macro_export]
macro_rules! error {
    ($ty:ident: $($range:expr),+) => {
        return Err(ErrorType::$ty.with_multiple(vec![$($range),+]))
    };
    ($ty:ident($($arg:expr),+): $($range:expr),+) => {
        return Err(ErrorType::$ty($($arg),+).with_multiple(vec![$($range),+]))
    };
}

#[derive(Error, Debug, Clone)]
pub enum ErrorType {
    /// Not actually an error. Used when e.g.
    /// a variable needs a value, but will never be used.
    #[error("")]
    Nothing,
    // tokenizer
    #[error("Invalid Character {0}")]
    InvalidCharacter(String),
    #[error("Could not parse number ({0})")]
    InvalidNumber(String),

    // parser
    #[error("Expected Number")]
    ExpectedNumber,
    #[error("Expected Operator")]
    ExpectedOperator,
    #[error("Expected 'in'")]
    ExpectedIn,
    #[error("Expected a format (hex/binary/decimal)")]
    ExpectedFormat,
    #[error("Missing opening bracket")]
    MissingOpeningBracket,
    #[error("Missing closing bracket")]
    MissingClosingBracket,
    #[error("Expected an identifier")]
    ExpectedIdentifier,
    #[error("Unknown Identifier \"{0}\"")]
    UnknownIdentifier(String),
    #[error("Unknown Variable \"{0}\"")]
    UnknownVariable(String),
    #[error("Missing equals sign")]
    MissingEqualsSign,
    #[error("Boolean operators are only allowed at the top level")]
    UnexpectedBooleanOperator,
    #[error("Boolean operators and definitions are not allowed")]
    DisallowedBooleanOperator,
    #[error("Second boolean operator")]
    UnexpectedSecondBooleanOperator,
    #[error("Unknown function \"{0}\"")]
    UnknownFunction(String),
    #[error("Wrong number of arguments (expected {0} arguments)")]
    WrongNumberOfArguments(usize),
    #[error("Wrong number of arguments (expected one of {0:?} arguments)")]
    WrongNumberOfArgumentsMultiple(&'static [usize]),
    #[error("Expected unit")]
    ExpectedUnit,
    #[error("Unexpected unit")]
    UnexpectedUnit,
    #[error("Expected text")]
    ExpectedElements,
    #[error("Expected end")]
    UnexpectedElements,
    #[error("Unexpected comma")]
    UnexpectedComma,
    #[error("Definitions are only allowed at the top level")]
    UnexpectedDefinition,
    #[error("Postfix definition disallowed since normal definition was used")]
    DisallowedPostfixDefinitionNormalDefinitionWasUsed,
    #[error("Expected expression, found {0}")]
    ExpectedExpression(String),
    #[error("Expected opening bracket")]
    ExpectedOpenBracket,
    #[error("Expected closing bracket")]
    ExpectedCloseBracket,
    #[error("Unexpected closing bracket")]
    UnexpectedCloseBracket,
    #[error("Expected opening square bracket")]
    ExpectedOpenSquareBracket,
    #[error("Expected closing square bracket")]
    ExpectedCloseSquareBracket,
    #[error("Expected opening curly bracket")]
    ExpectedOpenCurlyBracket,
    #[error("Expected closing curly bracket")]
    ExpectedCloseCurlyBracket,
    #[error("Expected comma")]
    ExpectedComma,
    #[error("Can't redefine standard variable \"{0}\"")]
    ReservedVariable(String),
    #[error("Can't redefine standard function \"{0}\"")]
    ReservedFunction(String),
    #[error("Argument \"{0}\" already given")]
    DuplicateArgument(String),
    #[error("Unexpected question mark")]
    UnexpectedQuestionMark,
    #[error("A question mark is not allowed here")]
    QuestionMarkNotAllowed,
    #[error("Can't use what's being defined")]
    CantUseIdentifierInDefinition,
    #[error("Unknown object \"{0}\"")]
    UnknownObject(String),
    #[error("Expected the name of the object")]
    ExpectedObjectName,
    #[error("Invalid date")]
    InvalidDate,
    #[error("Expected a dot")]
    ExpectedDot,
    #[error("This number is too big")]
    TooBig,
    // Stupid
    #[error("The number is too big, or negative (found {0})")]
    NotU32(i32),

    // engine
    #[error("Cannot divide by zero")]
    DivideByZero,
    #[error("Expected integer for operator '{0}'")]
    ExpectedIntegerWithOperator(String),
    #[error("Expected an integer, found {0}")]
    ExpectedInteger(f64),
    #[error("Expected percentage for 'of' operator")]
    ExpectedPercentage,
    #[error("Expected a vector")]
    ExpectedVector,
    #[error("The lengths don't match")]
    VectorLengthsNotMatching,
    #[error("Argument 1 must be less than argument 2")]
    Arg1GreaterThanArg2,
    #[error("Unknown conversion ({0} -> {1})")]
    UnknownConversion(String, String),
    #[error("The units don't match")]
    UnitsNotMatching,
    #[error("Not a number")]
    NotANumber,
    #[error("Powers can't be used around the unknown variable")]
    ForbiddenExponentiation,
    #[error("Expected unknown variable (?)")]
    ExpectedQuestionMark,
    #[error("Wrong unit, expected {0}")]
    WrongUnit(String),
    #[error("This operation is not supported")]
    UnsupportedOperation,
    #[error("Invalid side")]
    InvalidSide,
    #[error("Expected a value with a time unit")]
    ExpectedTimeValue,
    #[error("This date is too big")]
    DateTooBig,
    #[error("The operands are in the wrong order")]
    WrongOrder,
    #[error("This is not callable")]
    NotCallable,
    /// This should never happen
    #[error("Invalid AST (this is a bug!)")]
    InvalidAst,
    #[error("Invalid token (this is a bug!)")]
    InvalidToken,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub struct SourceRange {
    pub start_line: usize,
    pub start_char: usize,
    pub end_line: usize,
    pub end_char: usize,
}

impl SourceRange {
    pub fn new(start_line: usize, start: usize, end_line: usize, end: usize) -> Self {
        Self { start_line, start_char: start, end_line, end_char: end }
    }

    pub fn line(line: usize, start: usize, end: usize) -> Self {
        Self { start_line: line, end_line: line + 1, start_char: start, end_char: end }
    }

    pub fn empty() -> Self { Self::default() }

    pub fn extend(self, other: SourceRange) -> Self {
        Self {
            start_line: self.start_line,
            start_char: self.start_char,
            end_char: self.end_char.max(other.end_char),
            end_line: self.end_line.max(other.end_line),
        }
    }
}

impl Default for SourceRange {
    fn default() -> Self {
        Self { start_line: 0, start_char: 0, end_line: 1, end_char: 1 }
    }
}

impl Display for SourceRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}..{}:{}", self.start_line, self.start_char, self.end_line, self.end_char)
    }
}

#[macro_export]
macro_rules! range {
    ($start_line:expr, $start_char:expr ; $end_line:expr, $end_char:expr) => {
        SourceRange::new($start_line, $start_char, $end_line, $end_char)
    };
    (line $line:expr => $char_range:expr) => {
        SourceRange::line($line, $char_range.start, $char_range.end)
    }
}

impl ErrorType {
    pub fn with(self, range: SourceRange) -> Error {
        Error {
            error: self,
            ranges: vec![range],
        }
    }

    pub fn with_multiple(self, ranges: Vec<SourceRange>) -> Error {
        Error {
            error: self,
            ranges,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub error: ErrorType,
    pub ranges: Vec<SourceRange>,
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn round_dp(n: f64, dp: i32) -> String {
    if n.is_nan() { return "NaN".to_owned(); }
    if !n.is_finite() { return "infinity".to_owned(); }

    let multiplier = 10f64.powi(dp);
    ((n * multiplier).round() / multiplier).to_string()
}

#[cfg(not(target_arch = "wasm32"))]
pub fn cache_dir() -> PathBuf { dirs::cache_dir().unwrap().join(CRATE_NAME) }

pub fn data_dir() -> PathBuf { dirs::data_local_dir().unwrap().join(CRATE_NAME) }

pub mod math {
    pub fn round(num: f64, dp: i32) -> f64 {
        let multiplier = 10f64.powi(dp);
        (num * multiplier).round() / multiplier
    }

    pub fn factorial(num: f64) -> f64 {
        if num == 0.0 || num == 1.0 {
            1.0
        } else {
            let factor = if num.is_sign_negative() { -1.0 } else { 1.0 };
            factor * factorial(num.abs() - 1.0) * num
        }
    }
}
