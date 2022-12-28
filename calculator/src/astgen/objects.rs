/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;

use chrono::{Duration, Local, NaiveDate};

use crate::astgen::ast::{AstNode, AstNodeData, Operator};
use crate::astgen::tokenizer::{Token, TokenType};
use crate::common::{ErrorType, Result};
use crate::environment::currencies::Currencies;
use crate::environment::units;
use crate::environment::units::Unit;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, serde::Serialize, serde::Deserialize)]
pub enum CalculatorObject {
    Date(DateObject),
}

impl CalculatorObject {
    pub fn parse((name, name_range): (String, Range<usize>), tokens: &[Token], fallback_range: Range<usize>) -> Result<Self> {
        match name.as_str() {
            "date" => Ok(Self::Date(DateObject::parse(tokens, fallback_range)?)),
            _ => Err(ErrorType::UnknownObject(name).with(name_range))
        }
    }

    pub fn apply(&self, self_range: Range<usize>, op: (Operator, Range<usize>), other: &AstNode, self_in_rhs: bool) -> Result<AstNode> {
        match self {
            Self::Date(date) => date.apply(self_range, op, other, self_in_rhs),
        }
    }
}

impl Display for CalculatorObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Date(date) => write!(f, "{date}")?,
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub struct DateObject {
    date: NaiveDate,
}

impl Display for DateObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.date.format("%d.%m.%Y"))
    }
}

impl DateObject {
    fn parse(tokens: &[Token], fallback_range: Range<usize>) -> Result<Self> {
        if tokens.is_empty() {
            return Err(ErrorType::ExpectedElements.with(fallback_range));
        }

        if tokens.len() == 1 && tokens[0].ty == TokenType::Identifier && tokens[0].text.to_lowercase() == "now" {
            return Ok(Self { date: Local::now().date_naive() });
        }

        let str = tokens.iter().map(|t| t.text.clone()).collect::<String>();
        let range = tokens.first().unwrap().range().start..tokens.last().unwrap().range().end;
        let date = match NaiveDate::parse_from_str(&str, "%d.%m.%Y") {
            Ok(d) => d,
            Err(e) => return Err(ErrorType::InvalidDate(e).with(range)),
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
                    result.unit = Some(Unit::from("d"));
                    Ok(result)
                }
                _ => Err(ErrorType::InvalidSide.with(other.range.clone()))
            }
            _ => Err(ErrorType::UnsupportedOperation.with(op.1))
        }
    }
}
