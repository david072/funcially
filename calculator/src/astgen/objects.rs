/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::cmp::Ordering;
use std::fmt::Debug;
use std::ops::Range;

use chrono::{Duration, Local, NaiveDate};

use crate::{Context, DateFormat, error, Settings};
use crate::astgen::ast::{AstNode, AstNodeData, Operator};
use crate::common::{ErrorType, Result};
use crate::engine::{Engine, Value};
use crate::environment::currencies::Currencies;
use crate::environment::units;
use crate::environment::units::Unit;

#[derive(Debug, PartialEq)]
pub enum ObjectArgument {
    Ast(Vec<AstNode>, Range<usize>),
    String(String, Range<usize>),
}

impl ObjectArgument {
    pub fn range(&self) -> &Range<usize> {
        match self {
            Self::Ast(_, r) | Self::String(_, r) => r,
        }
    }

    pub fn is_ast(&self) -> bool {
        matches!(self, Self::Ast(..))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, serde::Serialize, serde::Deserialize)]
pub enum CalculatorObject {
    Date(DateObject),
}

impl CalculatorObject {
    pub(crate) fn parse(
        (name, name_range): (String, Range<usize>),
        args: Vec<ObjectArgument>,
        context: Context,
        range: Range<usize>,
    ) -> Result<Self> {
        match name.as_str() {
            "date" => Ok(Self::Date(DateObject::parse(args, context, range)?)),
            _ => Err(ErrorType::UnknownObject(name).with(name_range))
        }
    }

    pub fn is_valid_object(name: &str) -> bool {
        matches!(name, "date")
    }

    pub fn apply(&self, self_range: Range<usize>, op: (Operator, Range<usize>), other: &AstNode, self_in_rhs: bool) -> Result<AstNode> {
        match self {
            Self::Date(date) => date.apply(self_range, op, other, self_in_rhs),
        }
    }

    pub fn to_string(&self, settings: &Settings) -> String {
        match self {
            Self::Date(date) => date.to_string(settings),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub struct DateObject {
    pub(crate) date: NaiveDate,
}

impl DateObject {
    pub fn to_string(&self, settings: &Settings) -> String {
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
        full_range: Range<usize>,
    ) -> Result<Self> {
        if given_args.is_empty() {
            error!(ExpectedElements: full_range);
        }

        if let ObjectArgument::String(s, range) = &given_args[0] {
            let s = s.trim().to_lowercase();
            if s.starts_with("now") {
                if s.len() > 3 {
                    error!(UnexpectedElements: range.start + 3..range.end);
                }
                if given_args.len() > 1 {
                    error!(UnexpectedElements: given_args[1].range().start..given_args.last().unwrap().range().end);
                }

                return Ok(Self { date: Local::now().date_naive() });
            }
        }

        if given_args.len() > 5 {
            let range = given_args[5].range().start..given_args.last().unwrap().range().end;
            error!(UnexpectedElements: range);
        }

        let mut args = vec![];
        for arg in given_args {
            match arg {
                ObjectArgument::Ast(..) => args.push(arg),
                ObjectArgument::String(str, range) => {
                    let mut range_offset = range.start;
                    args.append(&mut str.split(context.settings.date.delimiter)
                        .map(|s| {
                            let mut range = range_offset..(range_offset + s.len()).max(range_offset + 1);
                            range_offset = range.end + 1;
                            let prev_len = s.len();
                            let s = s.trim_start();
                            range.start += s.len().saturating_sub(prev_len);
                            let prev_len = s.len();
                            let s = s.trim_end();
                            range.end -= s.len().saturating_sub(prev_len);
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
                        let range = range.end..range.end + 1;
                        error!(ExpectedDot: range);
                    }
                }
                if i != 0 {
                    if matches!(&args[i - 1], ObjectArgument::String(s, ..) if s.is_empty()) {
                        args.remove(i - 1);
                        continue;
                    } else {
                        let range = args[i].range();
                        let range = range.start - 1..range.start;
                        error!(ExpectedDot: range);
                    }
                }
            }
            i += 1;
        }

        if let Some(range) = args.iter().find_map(|arg| {
            match arg {
                ObjectArgument::String(s, range) if s.is_empty() => Some(range.clone()),
                _ => None,
            }
        }) {
            error!(ExpectedElements: range);
        }

        match args.len().cmp(&3) {
            Ordering::Greater => {
                error!(UnexpectedElements: args[3].range().start..args.last().unwrap().range().end)
            },
            Ordering::Less => {
                let last = args.last().unwrap();
                error!(ExpectedElements: last.range().end..last.range().end + 1);
            }
            _ => {}
        }

        let as_number = |arg: &ObjectArgument| {
            match arg {
                ObjectArgument::String(s, range) => s.parse::<i32>().map_err(|err| ErrorType::InvalidNumber(err.to_string()).with(range.clone())),
                ObjectArgument::Ast(ast, range) => {
                    match Engine::evaluate(ast.clone(), context)? {
                        Value::Number(res) => {
                            if res.number.fract() != 0.0 { return Err(ErrorType::ExpectedInteger(res.number).with(range.clone())); }
                            Ok(res.number as i32)
                        }
                        Value::Object(_) => Err(ErrorType::ExpectedNumber.with(range.clone())),
                    }
                }
            }
        };

        let year = as_number(&args[context.settings.date.format.year_index()])?;
        let month = as_number(&args[context.settings.date.format.month_index()])?;
        let month: u32 = month.try_into().map_err(|_| ErrorType::NotU32(month).with(args[1].range().clone()))?;
        let day = as_number(&args[context.settings.date.format.day_index()])?;
        let day: u32 = day.try_into().map_err(|_| ErrorType::NotU32(day).with(args[0].range().clone()))?;

        let Some(date) = NaiveDate::from_ymd_opt(year, month, day) else {
            let range = args.first().unwrap().range().start..args.last().unwrap().range().end;
            error!(InvalidDate: range);
        };
        Ok(Self { date })
    }

    fn apply(&self, self_range: Range<usize>, op: (Operator, Range<usize>), other: &AstNode, self_is_rhs: bool) -> Result<AstNode> {
        fn as_nanoseconds(unit: Option<&Unit>, n: f64, range: Range<usize>) -> Result<f64> {
            unit.and_then(|unit| {
                units::convert(
                    unit,
                    &Unit::from("ns"),
                    n,
                    &Currencies::none(),
                    &range,
                ).ok()
            }).map_or_else(|| Err(ErrorType::ExpectedTimeValue.with(range)), Ok)
        }

        match op.0 {
            Operator::Plus => match other.data {
                AstNodeData::Literal(n) => {
                    let n = as_nanoseconds(other.unit.as_ref(), n, other.range.clone())? as i64;
                    let Some(new_date) = self.date.checked_add_signed(Duration::nanoseconds(n)) else {
                        return Err(ErrorType::DateTooBig.with(self_range.start..other.range.end));
                    };
                    Ok(AstNode::new(AstNodeData::Object(CalculatorObject::Date(DateObject { date: new_date })), 0usize..1usize))
                }
                _ => Err(ErrorType::InvalidSide.with(other.range.clone()))
            }
            Operator::Minus => match other.data {
                AstNodeData::Literal(n) => {
                    if self_is_rhs {
                        return Err(ErrorType::WrongOrder.with(other.range.start..self_range.end));
                    }

                    let n = as_nanoseconds(other.unit.as_ref(), n, other.range.clone())? as i64;
                    let Some(new_date) = self.date.checked_sub_signed(Duration::nanoseconds(n)) else {
                        return Err(ErrorType::DateTooBig.with(self_range.start..other.range.end));
                    };
                    Ok(AstNode::new(AstNodeData::Object(CalculatorObject::Date(DateObject { date: new_date })), self_range))
                }
                AstNodeData::Object(CalculatorObject::Date(ref object)) => {
                    let duration = self.date.signed_duration_since(object.date);
                    let days = duration.num_milliseconds() as f64 / 1000.0 / 60.0 / 60.0 / 24.0;
                    let mut result = AstNode::new(AstNodeData::Literal(days), self_range);
                    result.unit = Some(Unit::Unit("d".to_string()));
                    Ok(result)
                }
                _ => Err(ErrorType::InvalidSide.with(other.range.clone()))
            }
            _ => Err(ErrorType::UnsupportedOperation.with(op.1))
        }
    }
}
