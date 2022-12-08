/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;

use crate::{
    common::{ErrorType, Result},
    environment::currencies::{Currencies, is_currency},
    environment::unit_conversion::{convert_units, format_unit, UNITS},
};

/// A struct representing a unit, holding a numerator and an optional denominator unit.
///
/// e.g. for `"km/h"`:
/// - numerator (0): `"km"`
/// - denominator (1): `"h"`
#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Unit(pub String, pub Option<String>);

impl Unit {
    pub fn format(&self, full_unit: bool, plural: bool) -> String {
        if !full_unit {
            return self.to_string();
        }
        else {
            let mut result = " ".to_string();
            let numerator = format_unit(&self.0, plural);
            result += &numerator;

            if let Some(denom) = &self.1 {
                let denom = format_unit(denom, false);
                result += " per ";
                result += &denom;
            }

            result
        }
    }
}

impl From<&str> for Unit {
    fn from(s: &str) -> Self { Unit(s.to_owned(), None) }
}

impl std::fmt::Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        if let Some(denom) = &self.1 {
            write!(f, "/{}", denom)?;
        }
        Ok(())
    }
}

// Stores prefix with its power (e.g. k => * 10^3)
pub const PREFIXES: [(char, i32); 19] = [
    ('y', -24), ('z', -21), ('a', -18), ('f', -15), ('p', -12), ('n', -9), ('m', -3), ('c', -2), ('d', -1),
    ('\0', 0),
    ('h', 2), ('k', 3), ('M', 6), ('G', 9), ('T', 12), ('P', 15), ('E', 18), ('Z', 21), ('Y', 24),
];

pub fn prefix_to_string(prefix: char) -> Option<&'static str> {
    match prefix {
        'y' => Some("Yocto"),
        'z' => Some("Zepto"),
        'a' => Some("Atto"),
        'f' => Some("Femto"),
        'p' => Some("Pico"),
        'n' => Some("Nano"),
        'm' => Some("Milli"),
        'c' => Some("Centi"),
        'd' => Some("Deci"),
        'h' => Some("Hecto"),
        'k' => Some("Kilo"),
        'M' => Some("Mega"),
        'G' => Some("Giga"),
        'T' => Some("Tera"),
        'P' => Some("Peta"),
        'E' => Some("Exa"),
        'Z' => Some("Zetta"),
        'Y' => Some("Yotta"),
        _ => None,
    }
}

pub fn is_unit(str: &str) -> bool {
    UNITS.contains(&str) || is_currency(str)
}

pub fn is_prefix(c: char) -> bool {
    for (p, _) in PREFIXES {
        if p == c { return true; }
    }
    false
}

pub fn get_prefix_power(c: char) -> Option<i32> {
    for (p, e) in PREFIXES {
        if p == c { return Some(e); }
    }
    None
}

pub fn convert(src_unit: &Unit, dst_unit: &Unit, n: f64, currencies: &Currencies, range: &Range<usize>) -> Result<f64> {
    let numerator = convert_units(&src_unit.0, &dst_unit.0, n, currencies, range)?;

    if src_unit.1.is_none() && dst_unit.1.is_none() {
        Ok(numerator)
    } else if src_unit.1.is_some() && dst_unit.1.is_some() {
        let denominator = convert_units(
            src_unit.1.as_ref().unwrap(),
            dst_unit.1.as_ref().unwrap(),
            1.0,
            currencies,
            range,
        )?;
        Ok(numerator / denominator)
    } else {
        Err(ErrorType::UnknownConversion(src_unit.to_string(), dst_unit.to_string())
            .with(range.clone()))
    }
}
