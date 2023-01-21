/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::io::{stdin, stdout, Write};
use std::path::PathBuf;

use clap::{Arg, ArgAction, Command};
use colored::Colorize;

use calculator::{AccessError, Calculator, data_dir, ResultData, Settings, Verbosity};

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

fn settings_path() -> PathBuf {
    let mut p = data_dir();
    p.push("cli-settings");
    p
}

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

    let settings = std::fs::read_to_string(settings_path())
        .ok()
        .and_then(|s| ron::from_str::<Settings>(&s).ok())
        .unwrap_or_else(Settings::default);

    let mut calculator = Calculator::new(verbosity, settings);

    // TODO: Properly handle CTRL-C
    loop {
        print!("> ");
        stdout().flush().unwrap();

        let mut input = String::new();
        stdin().read_line(&mut input).unwrap();
        input = input.trim().to_string();

        let mut words = input.split(' ').filter(|s| !s.is_empty());
        let first_word = words.next().map(|s| s.to_lowercase());
        if matches!(input.as_str(), "quit" | "exit") {
            break;
        }

        if first_word.is_some() && first_word.clone().unwrap() == "set" {
            let Some(path) = words.next() else {
                eprintln!("{}", "Expected a setting.".red());
                continue;
            };
            let path = path.split('.').collect::<Vec<_>>();

            let next = words.next();
            if next.is_some() && next.unwrap() == "?" {
                let mut path = path;
                path.push("");
                let err = calculator.settings.set(&path, "").unwrap_err();
                if let AccessError::InvalidPath(options) = err { println!("Options: {options:?}") }
                continue;
            }

            if next.is_none() || next.unwrap() != "=" {
                eprintln!("{}", "Expected equals sign.".red());
                eprintln!("{}", "Hint: Put a question mark at the end of the line to see the available options.".cyan());
                continue;
            }
            let Some(value) = words.next() else {
                eprintln!("{}", "Expected the value.".red());
                continue;
            };
            if words.next().is_some() {
                eprintln!("{}", "Too many arguments.".red());
                continue;
            }

            if let Err(e) = calculator.settings.set(&path, value) {
                eprintln!("{}", e.to_string().red());
            } else if let Ok(contents) = ron::to_string(&calculator.settings) {
                let _ = std::fs::write(settings_path(), contents);
            }
            continue;
        } else if first_word.is_some() && first_word.unwrap() == "get" {
            let Some(path) = words.next() else {
                eprintln!("{}", "Expected a setting.".red());
                continue;
            };

            if words.next().is_some() {
                eprintln!("{}", "Too many arguments.".red());
                continue;
            }

            let path = path.split('.').collect::<Vec<_>>();
            match calculator.settings.get(&path) {
                Ok(v) => println!("Setting's value: {v:?}"),
                Err(e) => eprintln!("{}", e.to_string().red()),
            }
            continue;
        }

        match input.as_str() {
            "quit" | "exit" => break,
            _ => {
                match calculator.calculate(&input) {
                    Ok(res) => match res.data {
                        ResultData::Value(value) => {
                            println!("= {}", value.format(&calculator.settings, use_thousands_separator));
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
                        eprintln!("{slice}");

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
