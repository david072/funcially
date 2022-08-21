pub mod common;
mod tokenizer;

use common::Result;
use tokenizer::tokenize;

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

pub fn calculate(input: &String, verbosity: Verbosity) -> Result<f64> {
    let tokens = tokenize(input)?;
    if matches!(verbosity, Verbosity::Tokens | Verbosity::Ast) {
        for token in tokens {
            println!("{} => {:?}", token.text, token.ty);
        }
    }

    Ok(0.0)
}