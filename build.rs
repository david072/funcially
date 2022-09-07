/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::collections::HashMap;

const OUTPUT_FILE: &str = "src/calculator/environment/default_currencies.rs";

#[derive(serde::Deserialize, Debug)]
struct ApiResponse {
    base: String,
    rates: HashMap<String, serde_json::Value>,
}

fn main() -> reqwest::Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    let ApiResponse { base, rates } =
        reqwest::blocking::get("https://api.exchangerate.host/latest?base=EUR")?
            .json::<ApiResponse>()?;

    let mut output = String::new();
    output += r#"use std::ops::Range;
use crate::common::{Result, ErrorType};
use phf::{Map, phf_map};

const BASE_CURRENCY: &str = ""#;
    output += &base;

    output += r#"";
static CURRENCIES: Map<&'static str, f64> = phf_map! {"#;

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
};

pub fn is_currency(str: &str) -> bool {
    CURRENCIES.contains_key(str)
}

pub fn convert(src_curr: &str, dst_curr: &str, n: f64, range: &Range<usize>) -> Result<f64> {
    if src_curr == dst_curr { return Ok(n); }

    let mut value = n;
    // Convert to base currency if needed
    if src_curr != BASE_CURRENCY {
        value /= match CURRENCIES.get(src_curr) {
            Some(v) => v,
            None => return Err(ErrorType::UnknownIdentifier.with(range.clone())),
        };
    }
    // Convert from base currency to dst currency if needed
    if dst_curr != BASE_CURRENCY {
        value *= match CURRENCIES.get(dst_curr) {
            Some(v) => v,
            None => return Err(ErrorType::UnknownIdentifier.with(range.clone())),
        };
    }

    Ok(value)
}"#;

    std::fs::write(OUTPUT_FILE, output).expect("Could not write to file");
    Ok(())
}
