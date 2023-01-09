/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::collections::HashMap;
use std::fs;

const OUTPUT_FILE: &str = "src/environment/default_currencies.rs";
const UNITS_OUTPUT_FILE: &str = "src/environment/unit_conversion.rs";

#[derive(serde::Deserialize, Debug)]
struct ApiResponse {
    base: String,
    rates: HashMap<String, serde_json::Value>,
}

fn update_units_file() -> Result<(), Box<dyn std::error::Error>> {
    let src_file_modified = fs::metadata("unit_data.txt")
        .ok()
        .and_then(|meta| meta.modified().ok());
    let output_file_modified = fs::metadata(UNITS_OUTPUT_FILE)
        .ok()
        .and_then(|meta| meta.modified().ok());

    if std::path::PathBuf::from(UNITS_OUTPUT_FILE).try_exists().unwrap_or(false)
        && src_file_modified == output_file_modified { return Ok(()); }

    let source = fs::read_to_string("unit_data.txt")?;
    let mut generator = unit_data::Generator::new(source, UNITS_OUTPUT_FILE.into());
    generator.generate()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    update_units_file()?;

    if &std::env::var("PROFILE").unwrap_or_default() != "release" &&
        std::path::PathBuf::from(OUTPUT_FILE).try_exists().unwrap_or(false) {
        return Ok(());
    }

    let ApiResponse { base, rates } =
        reqwest::blocking::get("https://api.exchangerate.host/latest?base=EUR")?
            .json::<ApiResponse>()?;

    let mut output = String::new();
    output += r#"// this file is generated in build.rs. Make changes there!

use phf::{Map, phf_map};

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

    fs::write(OUTPUT_FILE, output).expect("Could not write to file");
    Ok(())
}

mod unit_data {
    use std::fmt::{Debug, Display, Formatter};
    use std::fs;

    const MODIFIER_NORMAL: char = 'n';
    const MODIFIER_POWER: char = 'p';

    #[derive(Debug)]
    struct Unit {
        abbreviation: String,
        name: String,
        plural_name: String,
    }

    impl Unit {
        fn new(abbreviation: String, name: String, plural_name: Option<String>) -> Self {
            let plural_name = plural_name.unwrap_or_else(|| {
                let mut name = name.clone();
                name.push('s');
                name
            });
            Self {
                abbreviation,
                name,
                plural_name,
            }
        }
    }

    #[derive(Debug)]
    struct Conversion {
        src: String,
        dst: String,
        src_to_dst_expr: String,
        dst_to_src_expr: Option<String>,
        modifiers: Vec<char>,
    }

    impl Conversion {
        fn new(
            src: String,
            dst: String,
            std_expr: String,
            dts_expr: Option<String>,
            modifiers: String,
        ) -> Self {
            Self {
                src,
                dst,
                src_to_dst_expr: std_expr,
                dst_to_src_expr: dts_expr,
                modifiers: modifiers.chars().collect::<Vec<_>>(),
            }
        }

        fn generate_lines(&self) -> Vec<String> {
            let mut result = Vec::new();

            result.push(format!("\t\t(r#\"{}\"#, r#\"{}\"#) => Ok({}),", self.src, self.dst, self.src_to_dst_expr));
            if let Some(dts_expr) = &self.dst_to_src_expr {
                result.push(format!("\t\t(r#\"{}\"#, r#\"{}\"#) => Ok({}),", self.dst, self.src, dts_expr));
            }

            fn invert_operator(op: char) -> char {
                match op {
                    '*' => '/',
                    '/' => '*',
                    '+' => '-',
                    '-' => '+',
                    _ => op,
                }
            }

            for modifier in &self.modifiers {
                match *modifier {
                    MODIFIER_NORMAL => 'blk: {
                        if self.dst_to_src_expr.is_some() { break 'blk; }

                        let mut parts = self.src_to_dst_expr.split(' ');
                        let mut dts_expr = parts.next().unwrap().to_string();
                        let new_operator = invert_operator(parts.next().unwrap().chars().next().unwrap());
                        dts_expr += &format!(" {} ", new_operator);
                        dts_expr += &parts.collect::<Vec<_>>().join(" ");
                        result.push(format!("\t\t(r#\"{}\"#, r#\"{}\"#) => Ok({}),", self.dst, self.src, dts_expr));
                    }
                    MODIFIER_POWER => {
                        let mut parts = self.src_to_dst_expr.split(' ');
                        let start = parts.next().unwrap().to_string();
                        let operator = parts.next().unwrap().chars().next().unwrap();

                        let number = parts.next().unwrap().parse::<f64>().unwrap();

                        for exponent in 2..=3 {
                            let number_powered = number.powi(exponent);
                            let mut powered = number_powered.to_string();
                            if number_powered.fract() == 0.0 { powered += ".0"; }

                            result.push(format!(
                                "\t\t(r#\"{}^{exponent}\"#, r#\"{}^{exponent}\"#) => Ok({start} {operator} {powered}),",
                                self.src,
                                self.dst,
                            ));

                            if self.modifiers.contains(&MODIFIER_NORMAL) {
                                result.push(format!(
                                    "\t\t(r#\"{}^{exponent}\"#, r#\"{}^{exponent}\"#) => Ok({start} {} {powered}),",
                                    self.dst,
                                    self.src,
                                    invert_operator(operator),
                                ));
                            }
                        }
                    }
                    _ => {}
                }
            }

            result
        }
    }

    pub struct Generator {
        source: Vec<String>,
        index: usize,
        lines: usize,
        output_file_path: String,

        units: Vec<Unit>,
        conversions: Vec<Conversion>,
    }

    macro_rules! next_or_err {
    ($iter:expr, $curr_idx:expr) => {
        if let Some(v) = $iter.next() {
            v
        }
        else { return Err(Error($curr_idx).into()) }
    }
}

    impl Generator {
        pub fn new(source: String, output_file_path: String) -> Self {
            Self {
                source: source.lines().map(|s| s.to_string()).collect::<Vec<String>>(),
                output_file_path,
                index: 0,
                lines: source.lines().count(),
                units: Vec::new(),
                conversions: Vec::new(),
            }
        }

        pub fn generate(&mut self) -> std::result::Result<(), Box<dyn std::error::Error>> {
            self.parse_units()?;

            while self.index != self.lines {
                let line = self.source[self.index].trim();
                if line.is_empty() || line.starts_with('#') {
                    self.index += 1;
                    continue;
                }

                let mut parts = line.split([':', '|']);
                let (src, dst) = self.parse_conversion_head(
                    parts.next().unwrap_or_default()
                )?;

                let expressions = next_or_err!(parts, self.index).trim().to_string();
                if expressions.is_empty() { return Err(Error(self.index).into()); }

                let std_expr: String;
                let mut dts_expr: Option<String> = None;
                if let Some(semicolon_idx) = expressions.find(';') {
                    std_expr = expressions[0..semicolon_idx].trim().to_string();
                    let _dts_expr = expressions[semicolon_idx + 1..].trim().to_string();
                    if std_expr.is_empty() || _dts_expr.is_empty() { return Err(Error(self.index).into()); }
                    dts_expr = Some(_dts_expr);
                } else {
                    std_expr = expressions;
                }

                let modifiers = parts.next().unwrap_or_default().trim().to_string();
                if dts_expr.is_some() && !modifiers.is_empty() { return Err(Error(self.index).into()); }

                self.conversions.push(Conversion::new(src, dst, std_expr, dts_expr, modifiers));
                self.index += 1;
            }

            self.write_to_file()?;

            Ok(())
        }

        fn write_to_file(&self) -> std::io::Result<()> {
            let mut file_content = String::new();
            file_content += r#"// this file is generated in build.rs. Make changes there!

use std::f64::consts::PI;
use std::ops::Range;
use crate::{
    common::Result,
    common::ErrorType,
    environment::currencies::{Currencies, is_currency},
    environment::units::{PREFIXES, prefix_to_string, is_unit},
};
"#;
            let unit_strings = self.units.iter()
                .flat_map(|u| {
                    // Check if we need to add powers
                    if self.conversions.iter()
                        .find(|conv| {
                            conv.src == u.abbreviation || conv.dst == u.abbreviation
                        })
                        .map(|conv| conv.modifiers.contains(&'p'))
                        .unwrap_or(false) {
                        let mut result = Vec::new();
                        result.push(format!("r#\"{}\"#", u.abbreviation));

                        for exponent in 2..=3 {
                            result.push(format!("r#\"{}^{}\"#", u.abbreviation, exponent));
                        }
                        result
                    } else {
                        vec![format!("r#\"{}\"#", u.abbreviation)]
                    }
                })
                .collect::<Vec<_>>();

            file_content += &format!("\npub const UNITS: [&str; {}] = [\n", unit_strings.len());
            file_content += &format!("\t{}", unit_strings.join(", "));

            file_content += "\n];\n";

            file_content += r#"
fn unit_prefix(unit: &str) -> Option<(char, i32)> {
    if unit.len() < 2 { return None; }
    if is_unit(unit) { return None; }

    let char = unit.chars().next().unwrap();
    PREFIXES.into_iter().find(|&prefix| prefix.0 == char)
}

pub fn convert_units(src_unit: &str, dst_unit: &str, x: f64, currencies: &Currencies, range: &Range<usize>) -> Result<f64> {
    if src_unit == dst_unit { return Ok(x); }

    let mut src = src_unit;
    let mut dst = dst_unit;

    let src_power = unit_prefix(src).map(|x| x.1).unwrap_or(0);
    if src_power != 0 { src = &src[1..]; }
    let dst_power = unit_prefix(dst).map(|x| x.1).unwrap_or(0);
    if dst_power != 0 { dst = &dst[1..]; }

    let x = x * 10f64.powi(src_power - dst_power);

    if src == dst { return Ok(x); }

    match (src, dst) {
"#;

            file_content += &self.conversions.iter()
                .flat_map(|c| {
                    c.generate_lines()
                })
                .collect::<Vec<_>>()
                .join("\n");

            file_content += r#"
        _ => {
            // if either isn't a currency, this is an error
            // -> you can't convert from a currency to a normal unit, and if both aren't a
            //    currency, then we would have handled it in this match statement
            if !is_currency(src) || !is_currency(dst) {
                Err(ErrorType::UnknownConversion(src_unit.to_string(), dst_unit.to_string())
                    .with(range.clone()))
            }
            else {
                currencies.convert(src, dst, x, range)
           }
        }
    }
}

pub fn format_unit(unit: &str, plural: bool) -> String {
    fn lowercase_first(s: &str) -> String {
        let mut chars = s.chars();
        match chars.next() {
            Some(char) => char.to_lowercase().chain(chars).collect(),
            None => String::new(),
        }
    }

    let prefix = unit_prefix(unit).map(|x| x.0);
    let unit = if prefix.is_some() { &unit[1..] } else { unit };

    let mut result = String::new();

    if let Some(prefix) = prefix {
        result += prefix_to_string(prefix).unwrap();
    }

    if is_currency(unit) {
        if !result.is_empty() { result.push(' '); }
        return result + unit;
    }

    if plural {
        let str = match unit {
"#;

            file_content += &self.format_unit_long_form_match_arms(true);
            file_content += r#"
            _ => unreachable!(),
        };

        if prefix.is_some() { result + &lowercase_first(str) }
        else { result + str }
    }
    else {
        let str = match unit {
"#;

            file_content += &self.format_unit_long_form_match_arms(false);
            file_content += r#"
            _ => unreachable!(),
        };

        if prefix.is_some() { result + &lowercase_first(str) }
        else { result + str }
    }
}
"#;

            fs::write(self.output_file_path.clone(), file_content)?;

            Ok(())
        }

        fn format_unit_long_form_match_arms(&self, plural: bool) -> String {
            self.units.iter()
                .flat_map(|unit| {
                    let name = if plural { &unit.plural_name } else { &unit.name };

                    if self.conversions.iter()
                        .find(|conv| {
                            conv.src == unit.abbreviation || conv.dst == unit.abbreviation
                        })
                        .map(|conv| conv.modifiers.contains(&'p'))
                        .unwrap_or(false) {
                        vec![
                            format!("\t\t\tr#\"{}\"# => r#\"{}\"#,", unit.abbreviation, name),
                            format!("\t\t\tr#\"{}^2\"# => r#\"{} squared\"#,", unit.abbreviation, name),
                            format!("\t\t\tr#\"{}^3\"# => r#\"{} cubed\"#,", unit.abbreviation, name),
                        ]
                    } else {
                        vec![format!("\t\t\tr#\"{}\"# => r#\"{}\"#,", unit.abbreviation, name)]
                    }
                })
                .collect::<Vec<_>>()
                .join("\n")
        }

        fn parse_conversion_head(&self, s: &str) -> Result<(String, String)> {
            let mut parts = s.split("->");
            let src = next_or_err!(parts, self.index).trim();
            let dst = next_or_err!(parts, self.index).trim();

            Ok((src.into(), dst.into()))
        }

        fn parse_units(&mut self) -> Result<()> {
            while self.index != self.lines {
                let line = self.source[self.index].trim();
                if line.is_empty() || line.starts_with('#') {
                    self.index += 1;
                    continue;
                } else if line.starts_with("----") {
                    self.index += 1;
                    break;
                }

                let line_parts = line.split(',');
                for unit in line_parts {
                    if unit.is_empty() { continue; }
                    let Some(ci) = unit.find(':') else { return Err(self.index.into()); };
                    let abb = unit[0..ci].trim().to_string();
                    let long_forms = unit[ci + 1..].trim().to_string();

                    let singular: String;
                    let mut plural: Option<String> = None;
                    if let Some(slash_idx) = long_forms.find('/') {
                        singular = long_forms[0..slash_idx].trim().to_string();
                        plural = Some(long_forms[slash_idx + 1..].trim().to_string());
                    } else {
                        singular = long_forms;
                    }

                    self.units.push(Unit::new(abb, singular, plural));
                }

                self.index += 1;
            }

            Ok(())
        }
    }

    type Result<T> = std::result::Result<T, Error>;

    #[derive(Debug)]
    struct Error(usize);

    impl From<usize> for Error {
        fn from(i: usize) -> Self {
            Self(i)
        }
    }

    impl std::error::Error for Error {}

    impl Display for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }
}