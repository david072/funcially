/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

extern crate calculator;
extern crate clap;

use clap::{Arg, ArgAction, Command};
use std::io::{stdin, stdout, Write};
use calculator::{calculate, Environment, Verbosity, Format, round_dp, CalculatorResultData};

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

fn main() {
    let matches = Command::new(NAME)
        .about(DESCRIPTION)
        .version(VERSION)
        .author(AUTHORS)
        .subcommand_required(false)
        .arg(Arg::new("verbosity")
            .short('v')
            .long("verbosity")
            .help("How much debug info should be printed per calculation. One of 'none', 'tokens', 'ast'")
            .action(ArgAction::Set)
            .default_value("none")
            .takes_value(true))
        .get_matches();

    let verbosity = match matches.get_one::<String>("verbosity") {
        Some(verbosity) => verbosity.parse::<Verbosity>().unwrap(),
        None => Verbosity::None,
    };

    let mut environment = Environment::new();

    // TODO: Properly handle CTRL-C
    loop {
        print!("> ");
        stdout().flush().unwrap();

        let mut input = String::new();
        stdin().read_line(&mut input).unwrap();
        input = input.trim().to_string();

        match input.as_str() {
            "quit" | "exit" => break,
            _ => {
                match calculate(&input, &mut environment, verbosity) {
                    Ok(res) => {
                        match res.data {
                            CalculatorResultData::Number { result: n, unit, format } => {
                                let unit = unit.unwrap_or_default();
                                match format {
                                    Format::Decimal => println!("= {}{}", round_dp(n, 10), unit),
                                    Format::Hex => println!("= {:#X}{}", n as i64, unit),
                                    Format::Binary => println!("= {:#b}{}", n as i64, unit),
                                }
                            }
                            CalculatorResultData::Boolean(b) => println!("=> {}", if b { "True" } else { "False" }),
                            CalculatorResultData::Nothing => {}
                        }
                    }
                    Err(error) => {
                        println!("An error occurred: {:?}", error.error);

                        let slice_start = std::cmp::max(0, error.start as isize - 5) as usize;
                        let slice_end = std::cmp::min(input.len(), error.end + 5);
                        let slice = &input[slice_start..slice_end];
                        println!("{}", slice);

                        for _ in 0..error.start - slice_start {
                            print!(" ");
                        }
                        print!("^");
                        for _ in 0..error.end - error.start - 1 {
                            print!("-");
                        }

                        println!(" {:?}", error.error);
                    }
                }
            }
        }
    }
}
