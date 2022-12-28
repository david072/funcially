/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Range};

use chrono::{Days, NaiveDate};

use crate::astgen::ast::{AstNode, AstNodeData, Operator};
use crate::astgen::tokenizer::Token;
use crate::common::{ErrorType, Result};

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub enum CalculatorObject {
    Date(DateObject),
}

impl CalculatorObject {
    pub fn parse((name, name_range): (String, Range<usize>), tokens: &[Token]) -> Result<Self> {
        match name.as_str() {
            "date" => Ok(Self::Date(DateObject::parse(tokens))),
            _ => Err(ErrorType::UnknownObject(name).with(name_range))
        }
    }

    pub fn apply(&self, op: (Operator, Range<usize>), other: &AstNode, self_in_rhs: bool) -> Result<AstNode> {
        match self {
            Self::Date(date) => date.apply(op, other, self_in_rhs),
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

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct DateObject {
    date: NaiveDate,
}

impl Display for DateObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.date.format("%d.%m.%Y"))
    }
}

impl DateObject {
    // TODO
    fn parse(tokens: &[Token]) -> Self {
        Self {
            date: NaiveDate::from_ymd_opt(2022, 12, 27).unwrap(),
        }
    }

    // FIXME: this is shit
    fn apply(&self, op: (Operator, Range<usize>), other: &AstNode, self_is_rhs: bool) -> Result<AstNode> {
        match op.0 {
            Operator::Plus => match other.data {
                AstNodeData::Literal(n) if other.unit.as_ref().map(|u| u.0 == "d" && u.1.is_none()).unwrap_or_default() => {
                    let new_date = self.date.checked_add_days(Days::new(n as u64)).unwrap();
                    Ok(AstNode::new(AstNodeData::Object(CalculatorObject::Date(DateObject { date: new_date })), 0usize..1usize))
                }
                _ => Err(ErrorType::InvalidSide.with(other.range.clone()))
            }
            _ => Err(ErrorType::UnsupportedOperation.with(op.1))
        }
    }
}
