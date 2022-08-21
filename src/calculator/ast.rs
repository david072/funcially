use std::fmt::Formatter;

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

pub enum AstNode {
    Literal(f64),
    Operator(Operator),
}

impl std::fmt::Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(number) => write!(f, "Number: {}", number),
            Self::Operator(operator) => write!(f, "Operator: {:?}", operator),
        }
    }
}
