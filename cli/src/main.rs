/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use clap::{Arg, ArgAction, Command};
use std::io::{stdin, stdout, Write};
use colored::Colorize;
use calculator::{Calculator, Verbosity, ResultData};

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
            .default_value("none"))
        .arg(Arg::new("thousands_separator")
            .short('s')
            .long("thousands_separator")
            .help("Whether to use a '_' as the thousands separator in the results")
            .action(ArgAction::SetTrue))
        .get_matches();

    let verbosity = match matches.get_one::<String>("verbosity") {
        Some(verbosity) => verbosity.parse::<Verbosity>().unwrap(),
        None => Verbosity::None,
    };

    let use_thousands_separator = match matches.get_one::<bool>("thousands_separator") {
        Some(b) => *b,
        None => false,
    };

    let mut calculator = Calculator::new(verbosity);

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
                match calculator.calculate(&input) {
                    Ok(res) => match res.data {
                        ResultData::Value(value)  => {
                            println!("= {}", value.format(use_thousands_separator));
                        }
                        ResultData::Boolean(b) => {
                            println!("=> {}", if b { "True".green() } else { "False".red() });
                        }
                        ResultData::Function { .. } | ResultData::Nothing => {}
                    },
                    Err(error) => {
                        eprintln!("{}: {}", "Error".red(), error.error);

                        let slice_start = std::cmp::max(0, error.start as isize - 5) as usize;
                        let slice_end = std::cmp::min(input.len(), error.end + 5);
                        let slice = &input[slice_start..slice_end];
                        eprintln!("{}", slice);

                        for _ in 0..error.start - slice_start {
                            eprint!(" ");
                        }
                        eprint!("{}", "^".cyan());
                        for _ in 0..error.end - error.start - 1 {
                            eprint!("{}", "-".cyan());
                        }

                        eprintln!(" {}", error.error.to_string().cyan());
                    }
                }
            }
        }
    }
}
