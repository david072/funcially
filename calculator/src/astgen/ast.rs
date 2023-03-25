/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;

use crate::{common::*, environment::{
    currencies::Currencies,
    units::convert,
}, error, Format};
use crate::astgen::objects::CalculatorObject;
use crate::environment::units::Unit;

#[derive(Debug, PartialEq, Eq, Copy, Clone, serde::Serialize, serde::Deserialize)]
pub enum BooleanOperator {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
}

impl BooleanOperator {
    pub fn check<T: PartialEq + PartialOrd>(&self, lhs: T, rhs: T) -> bool {
        use BooleanOperator::*;
        match self {
            Equal => lhs == rhs,
            NotEqual => lhs != rhs,
            GreaterThan => lhs > rhs,
            GreaterThanEqual => lhs >= rhs,
            LessThan => lhs < rhs,
            LessThanEqual => lhs <= rhs,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, serde::Serialize, serde::Deserialize)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponentiation,
    BitwiseAnd,
    BitwiseOr,
    Xor,
    BitShiftLeft,
    BitShiftRight,
    Of,
    In,
    Modulo,
    Call,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum AstNodeData {
    Literal(f64),
    Operator(Operator),
    Group(Vec<AstNode>),
    Identifier(String),
    Unit(Unit),
    QuestionMark,
    Object(CalculatorObject),
    Arguments(Vec<Vec<AstNode>>),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, serde::Serialize, serde::Deserialize)]
pub enum AstNodeModifier {
    Factorial,
    BitwiseNot,
    Percent,
    Minus,
    Plus,
    Power(i32),
}

impl AstNodeModifier {
    pub fn is_prefix(&self) -> bool {
        matches!(self, AstNodeModifier::BitwiseNot
            | AstNodeModifier::Minus
            | AstNodeModifier::Plus)
    }
}

impl Display for AstNodeModifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNodeModifier::Factorial => write!(f, "!"),
            AstNodeModifier::BitwiseNot => write!(f, "!"),
            AstNodeModifier::Percent => write!(f, "%"),
            AstNodeModifier::Minus => write!(f, "-"),
            AstNodeModifier::Plus => write!(f, "+"),
            AstNodeModifier::Power(e) => write!(f, "^{e}"),
        }
    }
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct AstNode {
    pub data: AstNodeData,
    pub modifiers: Vec<AstNodeModifier>,
    pub unit: Option<Unit>,
    pub format: Format,
    pub range: Range<usize>,
    #[serde(skip)]
    did_apply_modifiers: bool,
}

#[macro_export]
macro_rules! match_ast_node {
    ($matcher:pat, $var:ident, $node:expr) => {
        if let $matcher = $node.data { $var }
        else {
            return Err(ErrorType::ExpectedNumber.with($node.range.clone()));
        }
    }
}

macro_rules! expect {
    ($bool:expr, $error_variant:ident, $range:expr) => {
        if !($bool) {
            return Err(ErrorType::$error_variant.with($range.clone()));
        }
    }
}

macro_rules! expect_args {
    ($bool:expr, $error_variant:ident($($arg:expr),+), $range:expr) => {
        if !($bool) {
            return Err(ErrorType::$error_variant($($arg),+).with($range.clone()));
        }
    }
}

macro_rules! expect_int {
    ($val:expr, $range:expr, $operator:expr) => {
        expect_args!($val.fract() == 0.0, ExpectedIntegerWithOperator(format!("{:?}", $operator)), $range);
    }
}

impl AstNode {
    pub fn new(data: AstNodeData, range: Range<usize>) -> AstNode {
        AstNode {
            data,
            modifiers: Vec::new(),
            unit: None,
            format: Format::Decimal,
            range,
            did_apply_modifiers: false,
        }
    }

    pub fn from(other: &AstNode, data: AstNodeData) -> AstNode {
        AstNode {
            data,
            modifiers: other.modifiers.clone(),
            unit: other.unit.clone(),
            format: other.format,
            range: other.range.clone(),
            did_apply_modifiers: false,
        }
    }

    pub fn apply(&mut self, operator: &Self, rhs: &mut Self, currencies: &Currencies) -> Result<()> {
        self.apply_modifiers()?;
        rhs.apply_modifiers()?;

        let full_range = self.range.start..rhs.range.end;

        let lhs = match_ast_node!(AstNodeData::Literal(ref mut lhs), lhs, self);
        let op = match_ast_node!(AstNodeData::Operator(op), op, operator);

        if op == Operator::In {
            let rhs_value = match_ast_node!(AstNodeData::Unit(ref name), name, rhs);
            if self.unit.is_none() {
                self.unit = Some(rhs_value.clone());
                return Ok(());
            }

            *lhs = convert(
                self.unit.as_ref().unwrap(),
                rhs_value,
                *lhs,
                currencies,
                &full_range,
            )?;
            self.unit = Some(rhs_value.clone());
            return Ok(());
        }

        let mut rhs_value = match_ast_node!(AstNodeData::Literal(rhs), rhs, rhs);

        self.format = rhs.format;

        if rhs.unit.is_some() && self.unit.is_none() {
            self.unit = rhs.unit.clone();
        } else if rhs.unit.is_some() && rhs.unit != self.unit {
            if let Ok(rhs) = convert(
                rhs.unit.as_ref().unwrap(),
                self.unit.as_ref().unwrap(),
                rhs_value,
                currencies,
                &full_range,
            ) {
                rhs_value = rhs;
            } else {
                let rhs_unit = rhs.unit.take().unwrap();
                let lhs_unit = self.unit.as_mut().unwrap();
                match op {
                    Operator::Multiply => lhs_unit.push_unit(rhs_unit),
                    Operator::Divide => {
                        if let Unit::Fraction(rhs_num, rhs_denom) = rhs_unit {
                            // Multiply by the inverse of the fraction
                            lhs_unit.push_unit(Unit::Fraction(rhs_denom, rhs_num));
                        } else {
                            *lhs_unit = Unit::Fraction(Box::new(lhs_unit.clone()), Box::new(rhs_unit));
                        }
                    }
                    _ => error!(UnknownConversion(rhs_unit.format(false, false), lhs_unit.format(false, false)): full_range),
                }

                if !lhs_unit.simplify() { self.unit = None; }
            }
        }

        match op {
            Operator::Multiply => *lhs *= rhs_value,
            Operator::Divide => {
                expect!(rhs_value != 0.0, DivideByZero, rhs.range);
                *lhs /= rhs_value;
            }
            Operator::Plus => *lhs += rhs_value,
            Operator::Minus => *lhs -= rhs_value,
            Operator::Exponentiation => *lhs = lhs.powf(rhs_value),
            Operator::BitwiseAnd | Operator::BitwiseOr | Operator::Xor | Operator::BitShiftLeft | Operator::BitShiftRight => {
                expect_int!(lhs, self.range, op);
                expect_int!(rhs_value, self.range, op);

                match op {
                    Operator::BitwiseAnd => *lhs = (*lhs as i64 & rhs_value as i64) as f64,
                    Operator::BitwiseOr => *lhs = (*lhs as i64 | rhs_value as i64) as f64,
                    Operator::Xor => *lhs = (*lhs as i64 ^ rhs_value as i64) as f64,
                    Operator::BitShiftLeft => *lhs = ((*lhs as i64) << (rhs_value as i64)) as f64,
                    Operator::BitShiftRight => *lhs = ((*lhs as i64) >> (rhs_value as i64)) as f64,
                    _ => unreachable!(),
                }
            }
            Operator::Of => {
                expect!(self.modifiers.contains(&AstNodeModifier::Percent),
                    ExpectedPercentage, self.range);
                *lhs *= rhs_value;
            }
            Operator::Modulo => *lhs %= rhs_value,
            Operator::In | Operator::Call => {}
        }

        Ok(())
    }

    pub fn apply_modifiers(&mut self) -> Result<()> {
        if self.modifiers.is_empty() || self.did_apply_modifiers {
            return Ok(());
        }

        let value = match_ast_node!(AstNodeData::Literal(ref mut v), v, self);
        for m in &self.modifiers {
            match m {
                AstNodeModifier::Factorial => {
                    expect_int!(value, self.range, m);
                    *value = math::factorial(*value);
                }
                AstNodeModifier::BitwiseNot => {
                    expect_int!(value, self.range, m);
                    let inverted = format!("{:b}", *value as i64)
                        .chars()
                        .map(|c| match c {
                            '1' => '0',
                            '0' => '1',
                            _ => unreachable!(),
                        })
                        .collect::<String>();
                    *value = match i64::from_str_radix(&inverted, 2) {
                        Ok(n) => n,
                        Err(_) => unreachable!(),
                    } as f64;
                }
                AstNodeModifier::Percent => *value /= 100.0,
                AstNodeModifier::Minus => *value *= -1.0,
                AstNodeModifier::Plus => *value *= 1.0,
                AstNodeModifier::Power(e) => *value *= 10f64.powi(*e),
            }
        }

        self.did_apply_modifiers = true;
        Ok(())
    }

    fn prefix_modifiers(&self) -> String {
        let mods = self.modifiers.iter()
            .filter(|m| m.is_prefix())
            .collect::<Vec<_>>();

        let mut res = String::new();
        for m in mods {
            res += format!("{m}").as_str();
        }

        res
    }

    fn suffix_modifiers(&self) -> String {
        let mods = self.modifiers.iter()
            .filter(|m| !m.is_prefix())
            .collect::<Vec<_>>();

        let mut res = String::new();
        for m in mods {
            res += format!("{m}").as_str();
        }

        res
    }

    fn unit(&self) -> String {
        match self.unit {
            Some(ref unit) => format!("{unit:?}"),
            None => String::new(),
        }
    }
}

impl PartialEq for AstNode {
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range
    }
}

impl Debug for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            AstNodeData::Literal(number) => write!(f, "Number: {p}{n}{s} {unit} ({fmt})",
                                                   p = self.prefix_modifiers(),
                                                   n = number,
                                                   s = self.suffix_modifiers(),
                                                   unit = self.unit(),
                                                   fmt = self.format),
            AstNodeData::Operator(operator) => write!(f, "Operator: {operator:?}"),
            AstNodeData::Group(ast) => {
                writeln!(f, "{p}Group{s} {unit} ({fmt}):",
                         p = self.prefix_modifiers(),
                         s = self.suffix_modifiers(),
                         unit = self.unit(),
                         fmt = self.format)?;

                for (i, node) in ast.iter().enumerate() {
                    for _ in 0..f.width().unwrap_or(0) + 4 {
                        write!(f, " ")?;
                    }
                    write!(f, "{:width$}", node, width = f.width().unwrap_or(0) + 4)?;
                    if i != ast.len() - 1 { writeln!(f)?; }
                }
                Ok(())
            }
            AstNodeData::Identifier(name) => write!(f, "Identifier: {p}{name}{s} {unit} ({fmt})",
                                                    p = self.prefix_modifiers(),
                                                    name = name,
                                                    s = self.suffix_modifiers(),
                                                    unit = self.unit(),
                                                    fmt = self.format),
            AstNodeData::Unit(name) => write!(f, "Unit: {name}"),
            AstNodeData::QuestionMark => write!(f, "QuestionMark"),
            AstNodeData::Object(object) => write!(f, "Object: {object:?}"),
            AstNodeData::Arguments(args) => {
                writeln!(f, "Arguments: ")?;
                for arg in args {
                    for node in arg {
                        for _ in 0..f.width().unwrap_or(0) + 4 {
                            write!(f, " ")?;
                        }
                        writeln!(f, "{:>width$}", node, width = f.width().unwrap_or(0) + 4)?;
                    }
                }

                Ok(())
            }
        }
    }
}
