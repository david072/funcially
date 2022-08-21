mod calculator;

use clap::{Arg, ArgAction, Command};
use std::io::{stdin, stdout, Write};
use calculator::{calculate, Verbosity};

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

    let verbosity = Verbosity::from_str(matches.get_one::<String>("verbosity"));

    loop {
        print!("> ");
        stdout().flush().unwrap();

        let mut input = String::new();
        stdin().read_line(&mut input).unwrap();

        match input.as_str() {
            "quit" | "exit" => break,
            _ => {
                let result = calculate(input, verbosity);
                println!("= {}", result);
            }
        }
    }
}
