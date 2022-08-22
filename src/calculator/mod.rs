pub mod common;
mod astgen;

use common::Result;
use astgen::parser::parse;
use astgen::tokenizer::tokenize;

#[derive(Debug, Clone, Copy)]
pub enum Verbosity {
    None,
    Tokens,
    Ast,
}

impl std::str::FromStr for Verbosity {
    type Err = ();

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(match s {
            "tokens" => Verbosity::Tokens,
            "ast" => Verbosity::Ast,
            _ => Verbosity::None
        })
    }
    // pub fn from_str(str: Option<&String>) -> Verbosity {
    //     match str {
    //         Some(str) => match str.as_str() {
    //             "tokens" => Verbosity::Tokens,
    //             "ast" => Verbosity::Ast,
    //             _ => Verbosity::None
    //         }
    //         None => Verbosity::None
    //     }
    // }
}

pub fn calculate(input: &str, verbosity: Verbosity) -> Result<f64> {
    let tokens = tokenize(input)?;
    if matches!(verbosity, Verbosity::Tokens | Verbosity::Ast) {
        println!("Tokens:");
        for token in &tokens {
            println!("{} => {:?}", token.text, token.ty);
        }
    }

    let ast = parse(&tokens)?;
    if matches!(verbosity, Verbosity::Ast) {
        println!("AST:");
        for node in &ast {
            println!("{}", node);
        }
    }

    Ok(0.0)
}