use crate::common::*;
use std::fmt::{Formatter, Display};
use std::ops::Range;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponentiation,
    BitwiseAnd,
    BitwiseOr,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum AstNodeData {
    Literal(f64),
    Operator(Operator),
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum AstNodeModifier {
    Factorial,
    BitwiseNot,
}

impl AstNodeModifier {
    pub fn is_prefix(&self) -> bool {
        matches!(self, AstNodeModifier::BitwiseNot)
    }

    pub fn is_suffix(&self) -> bool {
        matches!(self, AstNodeModifier::Factorial)
    }
}

impl Display for AstNodeModifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNodeModifier::Factorial => write!(f, "!"),
            AstNodeModifier::BitwiseNot => write!(f, "!"),
        }
    }
}

pub struct AstNode {
    pub data: AstNodeData,
    pub modifiers: Vec<AstNodeModifier>,
    pub range: Range<usize>,
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

macro_rules! expect_int {
    ($val:expr, $range:expr) => {
        if ($val.fract() != 0.0) {
            return Err(ErrorType::ExpectedInteger.with($range.clone()));
        }
    }
}

impl AstNode {
    pub fn new(data: AstNodeData, range: Range<usize>) -> AstNode {
        AstNode { data, modifiers: Vec::new(), range }
    }

    pub fn apply(&mut self, operator: &Self, rhs: &mut Self) -> Result<()> {
        self.apply_modifiers()?;
        rhs.apply_modifiers()?;

        let lhs = match_ast_node!(AstNodeData::Literal(ref mut lhs), lhs, self);
        let op = match_ast_node!(AstNodeData::Operator(op), op, operator);
        let rhs_value = match_ast_node!(AstNodeData::Literal(rhs), rhs, rhs);

        match op {
            Operator::Multiply => *lhs *= rhs_value,
            Operator::Divide => {
                if rhs_value == 0.0 {
                    return Err(ErrorType::DivideByZero.with(rhs.range.clone()));
                }
                *lhs /= rhs_value;
            }
            Operator::Plus => *lhs += rhs_value,
            Operator::Minus => *lhs -= rhs_value,
            Operator::Exponentiation => *lhs = lhs.powf(rhs_value),
            Operator::BitwiseAnd | Operator::BitwiseOr => {
                expect_int!(lhs, self.range);
                expect_int!(rhs_value, self.range);

                match op {
                    Operator::BitwiseAnd => *lhs = (*lhs as i64 & rhs_value as i64) as f64,
                    Operator::BitwiseOr => *lhs = (*lhs as i64 | rhs_value as i64) as f64,
                    _ => unreachable!(),
                }
            }
        }

        Ok(())
    }

    pub fn apply_modifiers(&mut self) -> Result<()> {
        if self.modifiers.is_empty() {
            return Ok(());
        }

        let value = match_ast_node!(AstNodeData::Literal(ref mut v), v, self);
        for m in &self.modifiers {
            match m {
                AstNodeModifier::Factorial => {
                    expect_int!(value, self.range);
                    *value = math::factorial(*value as i64) as f64;
                }
                AstNodeModifier::BitwiseNot => {
                    expect_int!(value, self.range);
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
            }
        }

        Ok(())
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.data {
            AstNodeData::Literal(number) => {
                let (prefixes, suffixes): (Vec<AstNodeModifier>, Vec<AstNodeModifier>) =
                    self.modifiers.iter().partition(|m| m.is_prefix());
                for ref m in prefixes {
                    write!(f, "{}", m)?;
                }
                write!(f, "Number: {}", number)?;
                for ref m in suffixes {
                    write!(f, "{}", m)?;
                }

                Ok(())
            }
            AstNodeData::Operator(operator) => write!(f, "Operator: {:?}", operator),
        }
    }
}
