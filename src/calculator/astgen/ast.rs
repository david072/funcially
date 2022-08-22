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

pub struct AstNode {
    pub data: AstNodeData,
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

impl AstNode {
    pub fn new(data: AstNodeData, range: Range<usize>) -> AstNode {
        AstNode { data, range }
    }

    pub fn apply(&mut self, operator: &Self, rhs: &Self) -> Result<()> {
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
                if lhs.fract() != 0.0 {
                    return Err(ErrorType::ExpectedInteger.with(self.range.clone()));
                }
                if rhs_value.fract() != 0.0 {
                    return Err(ErrorType::ExpectedInteger.with(rhs.range.clone()));
                }

                match op {
                    Operator::BitwiseAnd => *lhs = (*lhs as i64 & rhs_value as i64) as f64,
                    Operator::BitwiseOr => *lhs = (*lhs as i64 | rhs_value as i64) as f64,
                    _ => unreachable!(),
                }
            }
        }

        Ok(())
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.data {
            AstNodeData::Literal(number) => write!(f, "Number: {}", number),
            AstNodeData::Operator(operator) => write!(f, "Operator: {:?}", operator),
        }
    }
}
