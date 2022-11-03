/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::collections::HashMap;
use std::ops::Range;
use std::sync::Mutex;

use crate::{
    common::{ErrorType, Result},
    environment::default_currencies,
};

pub fn is_currency(str: &str) -> bool {
    default_currencies::CURRENCIES.contains_key(str)
}

pub struct Currencies {
    base: Mutex<Option<String>>,
    currencies: Mutex<Option<HashMap<String, f64>>>,
}

impl Currencies {
    #[allow(clippy::new_without_default)]
    pub fn new_arc() -> std::sync::Arc<Currencies> {
        let (base, currencies) =
            if let Some((base, currencies)) = updating::load_currencies() {
                (Some(base), Some(currencies))
            } else {
                (None, None)
            };

        let res = std::sync::Arc::new(Currencies {
            base: Mutex::new(base),
            currencies: Mutex::new(currencies),
        });
        updating::update_currencies(Some(res.clone()));
        res
    }

    pub fn none() -> Currencies {
        Currencies {
            base: Mutex::new(None),
            currencies: Mutex::new(None),
        }
    }

    pub fn update() { updating::update_currencies(None); }

    pub fn convert(&self, src_curr: &str, dst_curr: &str, n: f64, range: &Range<usize>) -> Result<f64> {
        if src_curr == dst_curr { return Ok(n); }

        let base = &*self.base.lock().unwrap();
        let currencies = &*self.currencies.lock().unwrap();

        let use_default = base.is_none() || currencies.is_none();
        let base = if use_default { default_currencies::BASE_CURRENCY } else { base.as_ref().unwrap() };

        let get_currency = |curr| {
            if use_default {
                default_currencies::CURRENCIES.get(curr)
            } else {
                currencies.as_ref().unwrap().get(curr)
            }
        };

        let mut value = n;
        // Convert to base currency if needed
        if src_curr != base {
            value /= match get_currency(src_curr) {
                Some(v) => v,
                None => return Err(ErrorType::UnknownIdentifier(src_curr.to_owned())
                    .with(range.clone())),
            };
        }
        // Convert from base currency to dst currency if needed
        if dst_curr != base {
            value *= match get_currency(dst_curr) {
                Some(v) => v,
                None => return Err(ErrorType::UnknownIdentifier(dst_curr.to_owned())
                    .with(range.clone())),
            };
        }

        Ok(value)
    }
}

mod updating {
    use std::collections::HashMap;

    #[cfg(not(target_arch = "wasm32"))]
    use crate::common::cache_dir;

    use super::Currencies;

    #[cfg(not(target_arch = "wasm32"))]
    const CURRENCIES_FILE_NAME: &str = "currencies.txt";
    #[cfg(target_arch = "wasm32")]
    const LOCAL_STORAGE_KEY: &str = "currencies";
    const CURRENCY_API_URL: &str = "https://api.exchangerate.host/latest?base=EUR";

    #[derive(serde::Deserialize, Debug)]
    struct ApiResponse {
        base: String,
        rates: HashMap<String, f64>,
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn cache_file_path() -> std::path::PathBuf { cache_dir().join(CURRENCIES_FILE_NAME) }

    /// Update currency file, and optionally update `Currencies` struct
    #[cfg(not(target_arch = "wasm32"))]
    pub fn update_currencies(currencies: Option<std::sync::Arc<Currencies>>) {
        std::thread::spawn(move || {
            let response: ApiResponse = reqwest::blocking::get(CURRENCY_API_URL).unwrap()
                .json().unwrap();

            if let Some(currencies) = currencies {
                *currencies.base.lock().unwrap() = Some(response.base.to_owned());
                *currencies.currencies.lock().unwrap() = Some(response.rates.clone());
            }

            if !cache_dir().try_exists().unwrap_or(false) {
                let _ = std::fs::create_dir(cache_dir());
            }

            let file_content = encode_currencies(&response);
            let file = cache_file_path();
            let _ = std::fs::write(file, file_content);
        });
    }

    #[cfg(target_arch = "wasm32")]
    pub fn update_currencies(currencies: Option<std::sync::Arc<Currencies>>) {
        wasm_bindgen_futures::spawn_local(async {
            async fn get() -> reqwest::Result<ApiResponse> {
                reqwest::get(CURRENCY_API_URL).await?.json().await
            }

            let response = match get().await {
                Ok(v) => v,
                Err(_) => return,
            };

            if let Some(currencies) = currencies {
                *currencies.base.lock().unwrap() = Some(response.base.to_owned());
                *currencies.currencies.lock().unwrap() = Some(response.rates.clone());
            }

            let content = encode_currencies(&response);
            set_local_storage_item(LOCAL_STORAGE_KEY, &content);
        });
    }

    fn encode_currencies(response: &ApiResponse) -> String {
        let mut result = String::new();
        result += &response.base;

        for (name, num) in &response.rates {
            result.push('\n');
            result += name;
            result.push(':');
            result += &num.to_string();
        }

        result
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn load_currencies() -> Option<(String, HashMap<String, f64>)> {
        let file = cache_file_path();
        if !file.try_exists().unwrap_or(false) { return None; }

        let file_contents = match std::fs::read_to_string(file) {
            Ok(contents) => contents,
            Err(_) => return None,
        };
        if file_contents.is_empty() { return None; }

        Some(decode_currencies(&file_contents))
    }

    #[cfg(target_arch = "wasm32")]
    pub fn load_currencies() -> Option<(String, HashMap<String, f64>)> {
        get_local_storage_item(LOCAL_STORAGE_KEY).map(|str| decode_currencies(&str))
    }

    fn decode_currencies(str: &str) -> (String, HashMap<String, f64>) {
        let mut result = HashMap::new();

        let mut lines = str.lines();
        let base = lines.next().unwrap().to_owned();

        for line in str.lines() {
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

        (base, result)
    }

    #[cfg(target_arch = "wasm32")]
    fn get_local_storage_item(key: &str) -> Option<String> {
        web_sys::window()
            .and_then(|win| win.local_storage().ok().flatten())
            .and_then(|storage| storage.get_item(key).ok().flatten())
    }

    #[cfg(target_arch = "wasm32")]
    fn set_local_storage_item(key: &str, value: &str) {
        web_sys::window()
            .and_then(|win| win.local_storage().ok().flatten())
            .and_then(|storage| storage.set_item(key, value).ok());
    }
}
