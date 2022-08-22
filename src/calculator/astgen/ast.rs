use crate::common::*;
use std::fmt::Formatter;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum AstNode {
    Literal(f64),
    Operator(Operator),
}

#[macro_export]
macro_rules! match_ast_node {
    ($matcher:pat, $var:ident, $node:expr) => {
        if let $matcher = $node { $var }
        else {
            return Err(ErrorType::ExpectedNumber.with(0..0));
        }
    }
}

impl AstNode {
    pub fn apply(&mut self, operator: &Self, rhs: &Self) -> Result<()> {
        let lhs = match_ast_node!(Self::Literal(ref mut lhs), lhs, self);
        let op = match_ast_node!(Self::Operator(op), op, operator);
        let rhs = match_ast_node!(Self::Literal(rhs), rhs, rhs);

        match op {
            Operator::Multiply => *lhs *= rhs,
            Operator::Divide => {
                if *rhs == 0.0 {
                    // FIXME: Somehow get the correct range here
                    return Err(ErrorType::DivideByZero.with(0..0));
                }
                *lhs /= rhs;
            }
            Operator::Plus => *lhs += rhs,
            Operator::Minus => *lhs -= rhs,
        }

        Ok(())
    }
}

impl std::fmt::Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(number) => write!(f, "Number: {}", number),
            Self::Operator(operator) => write!(f, "Operator: {:?}", operator),
        }
    }
}
