use crate::common::{Result, ErrorType};
use std::f64::consts::{PI, E, TAU};

const VARIABLES: [&str; 4] = ["pi", "e", "tau", "ans"];

pub fn is_valid_variable(str: &str) -> bool {
    VARIABLES.contains(&str.to_lowercase().as_str())
}

pub struct Variables {
    pi: f64,
    e: f64,
    tau: f64,
    ans: f64,
}

impl Variables {
    pub fn new() -> Variables {
        Variables { pi: PI, e: E, tau: TAU, ans: 0.0 }
    }

    pub fn resolve(&self, var: &str, ast_node_range: &std::ops::Range<usize>) -> Result<f64> {
        match var {
            "pi" => Ok(self.pi),
            "e" => Ok(self.e),
            "tau" => Ok(self.tau),
            "ans" => Ok(self.ans),
            _ => Err(ErrorType::UnknownVariable.with(ast_node_range.clone())),
        }
    }

    pub fn set(&mut self, var: &str, value: f64) {
        if var != "ans" {
            panic!("Can't set variable {}", var);
        }

        self.ans = value;
    }
}
