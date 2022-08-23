use ::Format;
use crate::common::*;
use std::fmt::{Formatter, Display, Debug};
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
    Of,
    In,
}

#[derive(PartialEq, Clone)]
pub enum AstNodeData {
    Literal(f64),
    Operator(Operator),
    Group(Vec<AstNode>),
    VariableReference(String),
}

impl Debug for AstNodeData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AstNodeData::")?;
        match self {
            AstNodeData::Literal(n) => write!(f, "Literal({:?})", n),
            AstNodeData::Operator(op) => write!(f, "Operator({:?})", op),
            AstNodeData::Group(_) => write!(f, "Group(...)"),
            AstNodeData::VariableReference(name) => write!(f, "VariableReference({})", name),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum AstNodeModifier {
    Factorial,
    BitwiseNot,
    Percent,
}

impl AstNodeModifier {
    pub fn is_prefix(&self) -> bool {
        matches!(self, AstNodeModifier::BitwiseNot)
    }
}

impl Display for AstNodeModifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNodeModifier::Factorial => write!(f, "!"),
            AstNodeModifier::BitwiseNot => write!(f, "!"),
            AstNodeModifier::Percent => write!(f, "%"),
        }
    }
}

#[derive(Clone)]
pub struct AstNode {
    pub data: AstNodeData,
    pub modifiers: Vec<AstNodeModifier>,
    pub format: Format,
    pub range: Range<usize>,
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

macro_rules! expect_int {
    ($val:expr, $range:expr) => {
        expect!($val.fract() == 0.0, ExpectedInteger, $range);
    }
}

impl AstNode {
    pub fn new(data: AstNodeData, range: Range<usize>) -> AstNode {
        AstNode {
            data,
            modifiers: Vec::new(),
            format: Format::Decimal,
            range,
            did_apply_modifiers: false,
        }
    }

    pub fn from(other: &AstNode, data: AstNodeData) -> AstNode {
        AstNode {
            data,
            modifiers: other.modifiers.clone(),
            format: other.format,
            range: other.range.clone(),
            did_apply_modifiers: false,
        }
    }

    pub fn apply(&mut self, operator: &Self, rhs: &mut Self) -> Result<()> {
        self.apply_modifiers()?;
        rhs.apply_modifiers()?;

        let lhs = match_ast_node!(AstNodeData::Literal(ref mut lhs), lhs, self);
        let op = match_ast_node!(AstNodeData::Operator(op), op, operator);
        let rhs_value = match_ast_node!(AstNodeData::Literal(rhs), rhs, rhs);

        self.format = rhs.format;

        match op {
            Operator::Multiply => *lhs *= rhs_value,
            Operator::Divide => {
                expect!(rhs_value != 0.0, DivideByZero, rhs.range);
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
            Operator::Of => {
                expect!(self.modifiers.contains(&AstNodeModifier::Percent),
                    ExpectedPercentage, self.range);
                *lhs *= rhs_value;
            }
            Operator::In => {}
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
                AstNodeModifier::Percent => *value /= 100.0,
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
            res += format!("{}", m).as_str();
        }

        res
    }

    fn suffix_modifiers(&self) -> String {
        let mods = self.modifiers.iter()
            .filter(|m| !m.is_prefix())
            .collect::<Vec<_>>();

        let mut res = String::new();
        for m in mods {
            res += format!("{}", m).as_str();
        }

        res
    }
}

impl PartialEq for AstNode {
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.data {
            AstNodeData::Literal(number) => write!(f, "Number: {}{}{} ({})",
                                                   self.prefix_modifiers(),
                                                   number,
                                                   self.suffix_modifiers(),
                                                   self.format),
            AstNodeData::Operator(ref operator) => write!(f, "Operator: {:?}", operator),
            AstNodeData::Group(ref ast) => {
                writeln!(f, "{}Group{} ({}):", self.prefix_modifiers(), self.suffix_modifiers(), self.format)?;

                for (i, node) in ast.iter().enumerate() {
                    for _ in 0..f.width().unwrap_or(0) + 4 {
                        write!(f, " ")?;
                    }
                    write!(f, "{:width$}", node, width = f.width().unwrap_or(0) + 4)?;
                    if i != ast.len() - 1 { writeln!(f)?; }
                }
                Ok(())
            }
            AstNodeData::VariableReference(ref name) => write!(f, "VariableRef: {}{}{} ({})",
                                                           self.prefix_modifiers(),
                                                           name,
                                                           self.suffix_modifiers(),
                                                           self.format),
        }
    }
}
