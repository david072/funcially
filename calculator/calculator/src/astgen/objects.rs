/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::cmp::Ordering;
use std::fmt::Debug;

use chrono::{Duration, Local, NaiveDate};

use crate::{Context, DateFormat, error, NumberValue, range, Settings};
use crate::astgen::ast::{AstNode, AstNodeData, Operator};
use crate::common::{ErrorType, Result, SourceRange};
use crate::engine::{Engine, Value};
use crate::environment::currencies::Currencies;
use crate::environment::units;
use crate::environment::units::Unit;

#[derive(Debug, PartialEq)]
pub enum ObjectArgument {
    Ast(Vec<AstNode>, SourceRange),
    String(String, SourceRange),
}

impl ObjectArgument {
    pub fn range(&self) -> &SourceRange {
        match self {
            Self::Ast(_, r) | Self::String(_, r) => r,
        }
    }

    pub fn is_ast(&self) -> bool {
        matches!(self, Self::Ast(..))
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, serde::Serialize, serde::Deserialize)]
pub enum CalculatorObject {
    Date(DateObject),
    Vector(Vector),
}

impl CalculatorObject {
    pub(crate) fn parse(
        (name, name_range): (String, SourceRange),
        args: Vec<ObjectArgument>,
        context: Context,
        range: SourceRange,
    ) -> Result<Self> {
        match name.as_str() {
            "date" => Ok(Self::Date(DateObject::parse(args, context, range)?)),
            _ => Err(ErrorType::UnknownObject(name).with(name_range))
        }
    }

    pub fn is_valid_object(name: &str) -> bool {
        matches!(name, "date")
    }

    pub fn is_callable(&self) -> bool {
        match self {
            Self::Date(_) => false,
            Self::Vector(_) => true,
        }
    }

    pub fn apply(&self, self_range: SourceRange, op: (Operator, SourceRange), other: &AstNode, self_in_rhs: bool) -> Result<AstNode> {
        match self {
            Self::Date(date) => date.apply(self_range, op, other, self_in_rhs),
            Self::Vector(vec) => vec.apply(self_range, op, other, self_in_rhs),
        }
    }

    pub fn call(&self, self_range: SourceRange, args: &[(NumberValue, SourceRange)], args_range: SourceRange) -> Result<AstNode> {
        match self {
            Self::Date(date) => date.call(self_range, args, args_range),
            Self::Vector(vec) => vec.call(self_range, args, args_range),
        }
    }

    pub fn to_string(&self, settings: &Settings) -> String {
        match self {
            Self::Date(date) => date.to_string(settings),
            Self::Vector(vec) => vec.to_string(settings),
        }
    }
}

trait Object: Sized {
    fn to_string(&self, settings: &Settings) -> String;

    fn parse(given_args: Vec<ObjectArgument>, context: Context, full_range: SourceRange) -> Result<Self>;

    fn apply(&self, self_range: SourceRange, op: (Operator, SourceRange), other: &AstNode, self_is_rhs: bool) -> Result<AstNode>;

    fn call(&self, self_range: SourceRange, args: &[(NumberValue, SourceRange)], args_range: SourceRange) -> Result<AstNode>;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub struct DateObject {
    pub(crate) date: NaiveDate,
}

impl Object for DateObject {
    fn to_string(&self, settings: &Settings) -> String {
        let fmt = match settings.date.format {
            DateFormat::Dmy => format!("%d{d}%m{d}%Y", d = settings.date.delimiter),
            DateFormat::Mdy => format!("%m{d}%d{d}%Y", d = settings.date.delimiter),
            DateFormat::Ymd => format!("%Y{d}%m{d}%d", d = settings.date.delimiter),
        };
        self.date.format(&fmt).to_string()
    }

    fn parse(
        given_args: Vec<ObjectArgument>,
        context: Context,
        full_range: SourceRange,
    ) -> Result<Self> {
        if given_args.is_empty() {
            error!(ExpectedElements: full_range);
        }

        if let ObjectArgument::String(s, range) = &given_args[0] {
            let s = s.trim().to_lowercase();
            if s.starts_with("now") {
                if s.len() > 3 {
                    let mut range = *range;
                    range.start_char += 3;
                    error!(UnexpectedElements: range);
                }
                if given_args.len() > 1 {
                    error!(UnexpectedElements: given_args[1].range().extend(*given_args.last().unwrap().range()));
                }

                return Ok(Self { date: Local::now().date_naive() });
            }
        }

        if given_args.len() > 5 {
            let range = given_args[5].range().extend(*given_args.last().unwrap().range());
            error!(UnexpectedElements: range);
        }

        let mut args = vec![];
        for arg in given_args {
            match arg {
                ObjectArgument::Ast(..) => args.push(arg),
                ObjectArgument::String(str, range) => {
                    let mut range_offset = range.start_char;

                    args.append(&mut str.split(context.borrow().settings.date.delimiter)
                        .map(|s| {
                            let char_range = range_offset..(range_offset + s.len()).max(range_offset + 1);
                            let mut range = range!(line range.start_line => char_range);
                            range_offset = range.end_char + 1;
                            let prev_len = s.len();
                            let s = s.trim_start();
                            range.start_char += s.len().saturating_sub(prev_len);
                            let prev_len = s.len();
                            let s = s.trim_end();
                            range.end_char -= s.len().saturating_sub(prev_len);
                            (s.to_owned(), range)
                        })
                        .map(|(s, range)| ObjectArgument::String(s, range))
                        .collect::<Vec<_>>());
                }
            }
        }

        let mut i = 0usize;
        while i < args.len() {
            if args[i].is_ast() {
                if i != args.len() - 1 {
                    if matches!(&args[i + 1], ObjectArgument::String(s, ..) if s.is_empty()) {
                        args.remove(i + 1);
                    } else {
                        let range = args[i].range();
                        let range = range!(line range.start_line => range.end_char..range.end_char + 1);
                        error!(ExpectedDot: range);
                    }
                }
                if i != 0 {
                    if matches!(&args[i - 1], ObjectArgument::String(s, ..) if s.is_empty()) {
                        args.remove(i - 1);
                        continue;
                    } else {
                        let range = args[i].range();
                        let range = range!(line range.start_line => range.end_char..range.end_char + 1);
                        error!(ExpectedDot: range);
                    }
                }
            }
            i += 1;
        }

        if let Some(range) = args.iter().find_map(|arg| {
            match arg {
                ObjectArgument::String(s, range) if s.is_empty() => Some(*range),
                _ => None,
            }
        }) {
            error!(ExpectedElements: range);
        }

        match args.len().cmp(&3) {
            Ordering::Greater => {
                error!(UnexpectedElements: args[3].range().extend(*args.last().unwrap().range()))
            }
            Ordering::Less => {
                let last = args.last().unwrap().range();
                let range = range!(line last.start_line => last.end_char..last.end_char + 1);
                error!(ExpectedElements: range);
            }
            _ => {}
        }

        let as_number = |arg: &ObjectArgument| {
            match arg {
                ObjectArgument::String(s, range) => s.parse::<i32>().map_err(|err| ErrorType::InvalidNumber(err.to_string()).with(*range)),
                ObjectArgument::Ast(ast, range) => {
                    match Engine::evaluate(ast.clone(), context.clone())? {
                        Value::Number(res) => {
                            if res.number.fract() != 0.0 { return Err(ErrorType::ExpectedInteger(res.number).with(*range)); }
                            Ok(res.number as i32)
                        }
                        Value::Object(_) => Err(ErrorType::ExpectedNumber.with(*range)),
                    }
                }
            }
        };

        let year = as_number(&args[context.borrow().settings.date.format.year_index()])?;
        let month = as_number(&args[context.borrow().settings.date.format.month_index()])?;
        let month: u32 = month.try_into().map_err(|_| ErrorType::NotU32(month).with(*args[1].range()))?;
        let day = as_number(&args[context.borrow().settings.date.format.day_index()])?;
        let day: u32 = day.try_into().map_err(|_| ErrorType::NotU32(day).with(*args[0].range()))?;

        let Some(date) = NaiveDate::from_ymd_opt(year, month, day) else {
            let range = args.first().unwrap().range().extend(*args.last().unwrap().range());
            error!(InvalidDate: range);
        };
        Ok(Self { date })
    }

    fn apply(&self, self_range: SourceRange, op: (Operator, SourceRange), other: &AstNode, self_is_rhs: bool) -> Result<AstNode> {
        fn as_nanoseconds(unit: Option<&Unit>, n: f64, range: SourceRange) -> Result<f64> {
            unit.and_then(|unit| {
                units::convert(
                    unit,
                    &Unit::from("ns"),
                    n,
                    &Currencies::none(),
                    range,
                ).ok()
            }).map_or_else(|| Err(ErrorType::ExpectedTimeValue.with(range)), Ok)
        }

        match op.0 {
            Operator::Plus => match other.data {
                AstNodeData::Literal(n) => {
                    let n = as_nanoseconds(other.unit.as_ref(), n, other.range)? as i64;
                    let Some(new_date) = self.date.checked_add_signed(Duration::nanoseconds(n)) else {
                        return Err(ErrorType::DateTooBig.with(self_range.extend(other.range)));
                    };
                    Ok(AstNode::new(AstNodeData::Object(CalculatorObject::Date(DateObject { date: new_date })), SourceRange::empty()))
                }
                _ => Err(ErrorType::InvalidSide.with(other.range))
            }
            Operator::Minus => match other.data {
                AstNodeData::Literal(n) => {
                    if self_is_rhs {
                        return Err(ErrorType::WrongOrder.with_multiple(vec![other.range, self_range]));
                    }

                    let n = as_nanoseconds(other.unit.as_ref(), n, other.range)? as i64;
                    let Some(new_date) = self.date.checked_sub_signed(Duration::nanoseconds(n)) else {
                        return Err(ErrorType::DateTooBig.with(self_range.extend(other.range)));
                    };
                    Ok(AstNode::new(AstNodeData::Object(CalculatorObject::Date(DateObject { date: new_date })), self_range))
                }
                AstNodeData::Object(CalculatorObject::Date(ref object)) => {
                    let duration = self.date.signed_duration_since(object.date);
                    let days = duration.num_milliseconds() as f64 / 1000.0 / 60.0 / 60.0 / 24.0;
                    let mut result = AstNode::new(AstNodeData::Literal(days), self_range);
                    result.unit = Some(Unit::from("d"));
                    Ok(result)
                }
                _ => Err(ErrorType::InvalidSide.with(other.range))
            }
            _ => Err(ErrorType::UnsupportedOperation.with(op.1))
        }
    }

    fn call(&self, _: SourceRange, _: &[(NumberValue, SourceRange)], _: SourceRange) -> Result<AstNode> { unreachable!(); }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, serde::Serialize, serde::Deserialize)]
pub struct Vector {
    pub(crate) numbers: Vec<f64>,
}

impl Vector {
    pub(crate) fn length(&self) -> f64 {
        self.numbers.iter().fold(0f64, |acc, n| acc + n.powi(2)).sqrt()
    }
}

impl Object for Vector {
    fn to_string(&self, _: &Settings) -> String {
        let mut result = "[".to_string();
        for (i, num) in self.numbers.iter().enumerate() {
            result += &format!("{num}{}", if i != self.numbers.len() - 1 { "; " } else { "" });
        }
        result + "]"
    }

    fn parse(_: Vec<ObjectArgument>, _: Context, _: SourceRange) -> Result<Self> {
        // This object cannot be constructed using the object syntax
        unreachable!()
    }

    fn apply(&self, self_range: SourceRange, op: (Operator, SourceRange), other: &AstNode, self_is_rhs: bool) -> Result<AstNode> {
        let numbers = self.numbers.clone();

        match op.0 {
            Operator::Multiply => {
                let AstNodeData::Literal(n) = other.data else { error!(ExpectedNumber: other.range); };
                let numbers = numbers.into_iter().map(|num| num * n).collect::<Vec<_>>();
                Ok(AstNode::new(AstNodeData::Object(CalculatorObject::Vector(Self { numbers })), self_range))
            }
            Operator::Plus | Operator::Minus => {
                let AstNodeData::Object(CalculatorObject::Vector(other_vec)) = &other.data else { error!(ExpectedVector: other.range); };
                if numbers.len() != other_vec.numbers.len() {
                    error!(VectorLengthsNotMatching: self_range, other.range);
                }

                let numbers = numbers.into_iter()
                    .zip(other_vec.numbers.iter())
                    .map(|(num, other)| {
                        match op.0 {
                            Operator::Plus => num + *other,
                            Operator::Minus => if self_is_rhs {
                                *other - num
                            } else {
                                num - *other
                            }
                            _ => unreachable!()
                        }
                    })
                    .collect::<Vec<_>>();
                Ok(AstNode::new(AstNodeData::Object(CalculatorObject::Vector(Self { numbers })), self_range))
            }
            _ => error!(UnsupportedOperation: op.1),
        }
    }

    fn call(&self, self_range: SourceRange, args: &[(NumberValue, SourceRange)], args_range: SourceRange) -> Result<AstNode> {
        if args.len() > 1 { error!(WrongNumberOfArguments(1): args_range); }

        let (number, range) = &args[0];
        if number.number.fract() != 0.0 { error!(ExpectedInteger(number.number): *range); }
        if number.number.is_sign_negative() { return Ok(AstNode::new(AstNodeData::Literal(f64::NAN), self_range)); }
        return match self.numbers.get(number.number as usize) {
            Some(n) => Ok(AstNode::new(AstNodeData::Literal(*n), self_range)),
            None => Ok(AstNode::new(AstNodeData::Literal(f64::NAN), self_range)),
        };
    }
}
