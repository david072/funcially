/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;
use std::collections::HashMap;
use std::sync::Mutex;
use crate::common::{Result, ErrorType};
use crate::environment::default_currencies;

const CRATE_NAME: &str = env!("CARGO_CRATE_NAME");
const CURRENCIES_FILE_NAME: &str = "currencies.txt";
const CURRENCY_API_URL: &str = "https://api.exchangerate.host/latest?base=EUR";

fn cache_folder_path() -> std::path::PathBuf {
    dirs::cache_dir().unwrap().join(CRATE_NAME)
}

fn cache_file_path() -> std::path::PathBuf {
    cache_folder_path().join(CURRENCIES_FILE_NAME)
}


pub fn is_currency(str: &str) -> bool {
    default_currencies::is_currency(str)
}

#[derive(serde::Deserialize, Debug)]
struct ApiResponse {
    base: String,
    rates: HashMap<String, f64>,
}

pub struct Currencies {
    base: Mutex<Option<String>>,
    currencies: Mutex<Option<HashMap<String, f64>>>,
}

impl Currencies {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Currencies {
        let (base, currencies) =
            if let Some((base, currencies)) = Self::load_currencies() {
                (Some(base), Some(currencies))
            } else {
                (None, None)
            };

        Currencies {
            base: Mutex::new(base),
            currencies: Mutex::new(currencies),
        }
    }

    pub fn update_currencies(currencies: std::sync::Arc<Currencies>) {
        std::thread::spawn(move || {
            let ApiResponse { base, rates } =
                reqwest::blocking::get(CURRENCY_API_URL).unwrap().json().unwrap();

            let mut file_content = String::new();
            file_content += &base;

            for (name, num) in &rates {
                file_content.push('\n');
                file_content += name;
                file_content.push(':');
                file_content += &num.to_string();
            }

            *currencies.base.lock().unwrap() = Some(base);
            *currencies.currencies.lock().unwrap() = Some(rates);

            if !cache_folder_path().try_exists().unwrap_or(false) {
                let _ = std::fs::create_dir(cache_folder_path());
            }

            let file = cache_file_path();
            let _ = std::fs::write(file, file_content);
        });
    }

    fn load_currencies() -> Option<(String, HashMap<String, f64>)> {
        let file = cache_file_path();
        if !file.try_exists().unwrap_or(false) { return None; }

        let file_contents = match std::fs::read_to_string(file) {
            Ok(contents) => contents,
            Err(_) => return None,
        };
        if file_contents.is_empty() { return None; }

        let mut result = HashMap::new();

        let mut lines = file_contents.lines();
        let base = lines.next().unwrap().to_owned();

        for line in file_contents.lines() {
            if line.is_empty() { continue; }
            let parts = line.split(':').collect::<Vec<_>>();
            if parts.len() != 2 { continue; }

            let name = parts[0];
            let num: f64 = match parts[1].parse() {
                Ok(n) => n,
                Err(_) => continue,
            };

            result.insert(name.to_string(), num);
        }

        Some((base, result))
    }

    pub fn convert(&self, src_curr: &str, dst_curr: &str, n: f64, range: &Range<usize>) -> Result<f64> {
        if src_curr == dst_curr { return Ok(n); }

        let base = &*self.base.lock().unwrap();
        let currencies = &*self.currencies.lock().unwrap();

        if base.is_none() || currencies.is_none() {
            return default_currencies::convert(src_curr, dst_curr, n, range);
        }

        let base = base.as_ref().unwrap();
        let currencies = currencies.as_ref().unwrap();

        let mut value = n;
        // Convert to base currency if needed
        if src_curr != base {
            value /= match currencies.get(src_curr) {
                Some(v) => v,
                None => return Err(ErrorType::UnknownIdentifier.with(range.clone())),
            };
        }
        // Convert from base currency to dst currency if needed
        if dst_curr != base {
            value *= match currencies.get(dst_curr) {
                Some(v) => v,
                None => return Err(ErrorType::UnknownIdentifier.with(range.clone())),
            };
        }

        Ok(value)
    }
}
