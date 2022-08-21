mod common;

use common::Result;

#[derive(Debug, Clone, Copy)]
pub enum Verbosity {
    None,
    Tokens,
    Ast,
}

impl Verbosity {
    pub fn from_str(str: Option<&String>) -> Verbosity {
        match str {
            Some(str) => match str.as_str() {
                "tokens" => Verbosity::Tokens,
                "ast" => Verbosity::Ast,
                _ => Verbosity::None
            }
            None => Verbosity::None
        }
    }
}

pub fn calculate(input: String, verbosity: Verbosity) -> Result<i64> {
    println!("calculating {} (verbosity: {:?})", input, verbosity);
    Ok(0)
}