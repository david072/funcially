/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::collections::HashMap;

const OUTPUT_FILE: &str = "src/environment/default_currencies.rs";

#[derive(serde::Deserialize, Debug)]
struct ApiResponse {
    base: String,
    rates: HashMap<String, serde_json::Value>,
}

fn main() -> reqwest::Result<()> {
    if &std::env::var("PROFILE").unwrap_or_default() != "release" &&
        std::path::PathBuf::from(OUTPUT_FILE).try_exists().unwrap_or(false) {
        return Ok(());
    }

    let ApiResponse { base, rates } =
        reqwest::blocking::get("https://api.exchangerate.host/latest?base=EUR")?
            .json::<ApiResponse>()?;

    let mut output = String::new();
    output += r#"use phf::{Map, phf_map};

pub const BASE_CURRENCY: &str = ""#;
    output += &base;

    output += r#"";
pub static CURRENCIES: Map<&'static str, f64> = phf_map! {"#;

    for (key, value) in rates {
        output += "\n\t\"";
        output += &key;
        output.push('"');
        output.push_str(" => ");

        let num = value.as_f64().unwrap();
        output += &num.to_string();
        if num.fract() == 0.0 {
            output += ".0";
        }

        output += ",";
    }

    output += r#"
};"#;

    std::fs::write(OUTPUT_FILE, output).expect("Could not write to file");
    Ok(())
}
