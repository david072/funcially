use crate::common::{Result, ErrorType};
use std::f64::consts::{PI, E, TAU};

const VARIABLES: [&str; 2] = ["pi", "e"];

pub fn is_valid_variable(str: &str) -> bool {
    VARIABLES.contains(&str)
}

pub fn resolve(var: &str, ast_node_range: &std::ops::Range<usize>) -> Result<f64> {
    match var {
        "pi" => Ok(PI),
        "e" => Ok(E),
        "tau" => Ok(TAU),
        _ => Err(ErrorType::UnknownVariable.with(ast_node_range.clone())),
    }
}