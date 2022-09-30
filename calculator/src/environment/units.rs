/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::f64::consts::PI;
use std::ops::Range;
use crate::{
    common::{Result, ErrorType},
    environment::currencies::{is_currency, Currencies},
};

/// A struct representing a unit, holding a numerator and an optional denominator unit.
///
/// e.g. for `"km/h"`:
/// - numerator (0): `"km"`
/// - denominator (1): `"h"`
#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Unit(pub String, pub Option<String>);

impl std::fmt::Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        if let Some(denom) = &self.1 {
            write!(f, "/{}", denom)?;
        }
        Ok(())
    }
}

const UNITS: [&str; 40] = [
    "m", "mi", "ft", "in", "yd", // length
    "a", "m^2", "mi^2", "ft^2", "in^2", "yd^2",  // area
    "l", "tsp", "tbsp", "floz", "cup", "m^3", "mi^3", "ft^3", "in^3", "yd^3", // volume
    "°", "rad", // angle
    "s", "min", "h", "d", "mo", "y", // time
    "g", "lb", "t", // mass
    "Pa", "bar", "psi", // pressure
    "°C", "°F", "K", // temperature
    "cal", "b", // misc
];

// Stores prefix with it's power (e.g. k => * 10^3)
const PREFIXES: [(char, i32); 9] = [
    ('m', -3), ('c', -2), ('d', -1),
    ('\0', 0),
    ('h', 2), ('k', 3), ('M', 6), ('G', 9), ('T', 12)
];

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

fn unit_prefix(unit: &str) -> Option<(char, i32)> {
    if unit.len() < 2 { return None; }
    if UNITS.contains(&unit) { return None; }

    let char = unit.chars().next().unwrap();
    for prefix in PREFIXES {
        if prefix.0 == char { return Some(prefix); }
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

fn convert_units(src_unit: &str, dst_unit: &str, n: f64, currencies: &Currencies, range: &Range<usize>) -> Result<f64> {
    if src_unit == dst_unit { return Ok(n); }

    let mut src_unit = src_unit;
    let mut dst_unit = dst_unit;

    let src_power = unit_prefix(src_unit).map(|x| x.1).unwrap_or(0);
    if src_power != 0 { src_unit = &src_unit[1..]; }
    let dst_power = unit_prefix(dst_unit).map(|x| x.1).unwrap_or(0);
    if dst_power != 0 { dst_unit = &dst_unit[1..]; }

    let n = n * 10f64.powi(src_power - dst_power);

    if src_unit == dst_unit { return Ok(n); }

    match (src_unit, dst_unit) {
        // distance
        ("m", "mi") => Ok(n / 1609.344),
        ("m", "ft") => Ok(n * 3.281),
        ("m", "in") => Ok(n * 39.37),
        ("m", "yd") => Ok(n * 1.094),

        ("ft", "m") => Ok(n / 3.281),
        ("ft", "mi") => Ok(n / 5280.0),
        ("ft", "in") => Ok(n * 12.0),
        ("ft", "yd") => Ok(n / 3.0),

        ("mi", "m") => Ok(n * 1609.344),
        ("mi", "ft") => Ok(n * 5280.0),
        ("mi", "in") => Ok(n * 63360.0),
        ("mi", "yd") => Ok(n * 1760.0),

        ("in", "m") => Ok(n / 39.37),
        ("in", "ft") => Ok(n / 12.0),
        ("in", "mi") => Ok(n / 63360.0),
        ("in", "yd") => Ok(n / 36.0),

        ("yd", "m") => Ok(n * 1.094),
        ("yd", "ft") => Ok(n * 3.0),
        ("yd", "mi") => Ok(n / 1760.0),
        ("yd", "in") => Ok(n * 36.0),

        // area
        ("m^2", "mi^2") => Ok(n / 2_590_000.0),
        ("m^2", "ft^2") => Ok(n * 10.764),
        ("m^2", "in^2") => Ok(n * 1550.0),
        ("m^2", "yd^2") => Ok(n * 1.196),
        ("m^2", "a") => Ok(n / 100.0),

        ("ft^2", "m^2") => Ok(n / 10.764),
        ("ft^2", "mi^2") => Ok(n / 27_880_000.0),
        ("ft^2", "in^2") => Ok(n * 144.0),
        ("ft^2", "yd^2") => Ok(n / 9.0),
        ("ft^2", "a") => Ok(n / 1076.0),

        ("mi^2", "m^2") => Ok(n * 2_590_000.0),
        ("mi^2", "ft^2") => Ok(n * 27_880_000.0),
        ("mi^2", "in^2") => Ok(n * 4_014_000_000.0),
        ("mi^2", "yd^2") => Ok(n * 3_098_000.0),
        ("mi^2", "a") => Ok(n * 25_900.0),

        ("in^2", "m^2") => Ok(n / 1550.0),
        ("in^2", "ft^2") => Ok(n / 144.0),
        ("in^2", "mi^2") => Ok(n / 4_014_000_000.0),
        ("in^2", "yd^2") => Ok(n / 1296.0),
        ("in^2", "a") => Ok(n / 155_000.0),

        ("yd^2", "m^2") => Ok(n / 1.196),
        ("yd^2", "ft^2") => Ok(n * 9.0),
        ("yd^2", "mi^2") => Ok(n / 3_098_000.0),
        ("yd^2", "in^2") => Ok(n * 1296.0),
        ("yd^2", "a") => Ok(n / 119.6),

        ("a", "m^2") => Ok(n * 100.0),
        ("a", "mi^2") => Ok(n / 25_900.0),
        ("a", "ft^2") => Ok(n * 1076.0),
        ("a", "in^2") => Ok(n * 155_000.0),
        ("a", "yd^2") => Ok(n * 119.6),

        // volume
        ("l", "tsp") => Ok(n * 202.9),
        ("l", "tbsp") => Ok(n * 67.628),
        ("l", "floz") => Ok(n * 33.814),
        ("l", "cup") => Ok(n * 4.227),
        ("l", "m^3") => Ok(n / 1000.0),
        ("l", "mi^3") => Ok(n / 4_168_000_000_000.0),
        ("l", "ft^3") => Ok(n / 28.317),
        ("l", "in^3") => Ok(n * 61.024),
        ("l", "yd^3") => Ok(n / 764.6),

        ("tsp", "l") => Ok(n / 202.9),
        ("tsp", "tbsp") => Ok(n / 3.0),
        ("tsp", "floz") => Ok(n / 6.0),
        ("tsp", "cup") => Ok(n / 48.0),
        ("tsp", "m^3") => Ok(n / 202_900.0),
        ("tsp", "mi^3") => Ok(n / 845_700_000_000_000.0),
        ("tsp", "ft^3") => Ok(n / 5745.0),
        ("tsp", "in^3") => Ok(n / 3.325),
        ("tsp", "yd^3") => Ok(n / 155_100.0),

        ("tbsp", "l") => Ok(n / 67.628),
        ("tbsp", "tsp") => Ok(n * 3.0),
        ("tbsp", "floz") => Ok(n / 2.0),
        ("tbsp", "cup") => Ok(n / 16.0),
        ("tbsp", "m^3") => Ok(n / 67_630.0),
        ("tbsp", "mi^3") => Ok(n / 281_900_000_000_000.0),
        ("tbsp", "ft^3") => Ok(n / 1915.0),
        ("tbsp", "in^3") => Ok(n / 1.108),
        ("tbsp", "yd^3") => Ok(n / 51_710.0),

        ("floz", "l") => Ok(n / 33.814),
        ("floz", "tsp") => Ok(n * 6.0),
        ("floz", "tbsp") => Ok(n * 2.0),
        ("floz", "cup") => Ok(n / 8.0),
        ("floz", "m^3") => Ok(n / 284_130.0),
        ("floz", "mi^3") => Ok(n / 146_700_000_000_000.0),
        ("floz", "ft^3") => Ok(n / 996.6),
        ("floz", "in^3") => Ok(n / 1.1734),
        ("floz", "yd^3") => Ok(n / 25_850.0),

        ("cup", "l") => Ok(n / 4.227),
        ("cup", "tsp") => Ok(n * 48.0),
        ("cup", "tbsp") => Ok(n * 16.0),
        ("cup", "floz") => Ok(n * 8.0),
        ("cup", "m^3") => Ok(n / 4227.0),
        ("cup", "mi^3") => Ok(n / 146_700_000_000_000.0),
        ("cup", "ft^3") => Ok(n / 119.7),
        ("cup", "in^3") => Ok(n * 14.438),
        ("cup", "yd^3") => Ok(n / 3232.0),

        ("m^3", "mi^3") => Ok(n / 4_168_000_000.0),
        ("m^3", "ft^3") => Ok(n * 35.315),
        ("m^3", "in^3") => Ok(n * 61_020.0),
        ("m^3", "yd^3") => Ok(n * 1.308),
        ("m^3", "l") => Ok(n * 1000.0),
        ("m^3", "tsp") => Ok(n * 202_900.0),
        ("m^3", "tbsp") => Ok(n * 67_630.0),
        ("m^3", "floz") => Ok(n * 284_130.0),
        ("m^3", "cup") => Ok(n * 4227.0),

        ("ft^3", "m^3") => Ok(n / 35.315),
        ("ft^3", "mi^3") => Ok(n / 147_200_000_000.0),
        ("ft^3", "in^3") => Ok(n * 1728.0),
        ("ft^3", "yd^3") => Ok(n / 27.0),
        ("ft^3", "l") => Ok(n * 28.317),
        ("ft^3", "tsp") => Ok(n * 5745.0),
        ("ft^3", "tbsp") => Ok(n * 1915.0),
        ("ft^3", "cup") => Ok(n * 119.7),
        ("ft^3", "floz") => Ok(n * 996.6),

        ("mi^3", "m^3") => Ok(n * 4_168_000_000.0),
        ("mi^3", "ft^3") => Ok(n * 147_200_000_000.0),
        ("mi^3", "in^3") => Ok(n * 254_400_000_000_000.0),
        ("mi^3", "yd^3") => Ok(n * 5_452_000_000.0),
        ("mi^3", "l") => Ok(n * 4_168_000_000_000.0),
        ("mi^3", "tsp") => Ok(n * 845_700_000_000_000.0),
        ("mi^3", "tbsp") => Ok(n * 281_900_000_000_000.0),
        ("mi^3", "floz") => Ok(n * 146_700_000_000_000.0),
        ("mi^3", "cup") => Ok(n * 146_700_000_000_000.0),

        ("in^3", "m^3") => Ok(n / 61_020.0),
        ("in^3", "ft^3") => Ok(n / 1728.0),
        ("in^3", "mi^3") => Ok(n / 254_400_000_000_000.0),
        ("in^3", "yd^3") => Ok(n / 46_660.0),
        ("in^3", "l") => Ok(n / 61.024),
        ("in^3", "tsp") => Ok(n * 3.325),
        ("in^3", "tbsp") => Ok(n * 1.108),
        ("in^3", "floz") => Ok(n * 1.1734),
        ("in^3", "cup") => Ok(n / 14.438),

        ("yd^3", "m^3") => Ok(n / 1.308),
        ("yd^3", "ft^3") => Ok(n * 27.0),
        ("yd^3", "mi^3") => Ok(n / 5_452_000_000.0),
        ("yd^3", "in^3") => Ok(n * 46_660.0),
        ("yd^3", "l") => Ok(n * 764.6),
        ("yd^3", "tsp") => Ok(n * 155_100.0),
        ("yd^3", "tbsp") => Ok(n * 51_710.0),
        ("yd^3", "floz") => Ok(n * 25_850.0),
        ("yd^3", "cup") => Ok(n * 3232.0),

        // angle
        ("°", "rad") => Ok(n * PI / 180.0),
        ("rad", "°") => Ok(n * 180.0 / PI),

        // time
        ("s", "min") => Ok(n / 60.0),
        ("s", "h") => Ok(n / 3600.0),
        ("s", "d") => Ok(n / 86_400.0),
        ("s", "mo") => Ok(n / 2_628_000.0),
        ("s", "y") => Ok(n / 31_540_000.0),

        ("min", "s") => Ok(n * 60.0),
        ("min", "h") => Ok(n / 60.0),
        ("min", "d") => Ok(n / 1440.0),
        ("min", "mo") => Ok(n / 43_800.0),
        ("min", "y") => Ok(n / 525_600.0),

        ("h", "s") => Ok(n * 3600.0),
        ("h", "min") => Ok(n * 60.0),
        ("h", "d") => Ok(n / 24.0),
        ("h", "mo") => Ok(n / 730.0),
        ("h", "y") => Ok(n / 8760.0),

        ("d", "s") => Ok(n * 86_400.0),
        ("d", "min") => Ok(n * 1440.0),
        ("d", "h") => Ok(n * 24.0),
        ("d", "mo") => Ok(n / 30.417),
        ("d", "y") => Ok(n / 365.0),

        ("mo", "s") => Ok(n * 2_628_000.0),
        ("mo", "min") => Ok(n * 43_800.0),
        ("mo", "h") => Ok(n * 730.0),
        ("mo", "d") => Ok(n * 30.417),
        ("mo", "y") => Ok(n / 12.0),

        ("y", "s") => Ok(n * 31_540_000.0),
        ("y", "min") => Ok(n * 525_600.0),
        ("y", "h") => Ok(n * 8760.0),
        ("y", "d") => Ok(n * 365.0),
        ("y", "mo") => Ok(n * 12.0),

        // mass
        ("g", "lb") => Ok(n / 453.59237),
        ("g", "t") => Ok(n / 1_000_000.0),

        ("lb", "g") => Ok(n * 453.59237),
        ("lb", "t") => Ok(n / 2205.0),

        ("t", "g") => Ok(n * 1_000_000.0),
        ("t", "lb") => Ok(n * 2205.0),

        // pressure
        ("Pa", "bar") => Ok(n / 100_000.0),
        ("Pa", "psi") => Ok(n / 6895.0),

        ("bar", "Pa") => Ok(n * 100_000.0),
        ("bar", "psi") => Ok(n / 14.504),

        ("psi", "Pa") => Ok(n * 6895.0),
        ("psi", "bar") => Ok(n * 14.504),

        // temperature
        ("°C", "°F") => Ok((n * 9.0 / 5.0) + 32.0),
        ("°C", "K") => Ok(n + 273.15),

        ("°F", "°C") => Ok((n - 32.0) * 5.0 / 9.0),
        ("°F", "K") => Ok((n - 32.0) * 5.0 / 9.0 - 273.15),

        ("K", "°C") => Ok(n - 273.15),
        ("K", "°F") => Ok((n - 273.15) * 9.0 / 5.0 + 32.0),
        _ => currencies.convert(src_unit, dst_unit, n, range),
    }
}

pub fn format(unit: &Unit, plural: bool) -> String {
    let mut result = String::new();
    let numerator = format_unit(&unit.0, plural);
    result += &numerator;

    if let Some(denom) = &unit.1 {
        let denom = format_unit(denom, false);
        result += " per";
        result += &denom;
    }

    result
}

fn format_unit(unit: &str, plural: bool) -> String {
    fn lowercase_first(s: &str) -> String {
        let mut chars = s.chars();
        match chars.next() {
            Some(char) => char.to_lowercase().chain(chars).collect(),
            None => String::new(),
        }
    }

    let prefix = unit_prefix(unit).map(|x| x.0);
    let unit = if prefix.is_some() { &unit[1..] } else { unit };

    let mut result = " ".to_string();

    if let Some(prefix) = prefix {
        result.push_str(match prefix {
            'm' => "Milli",
            'c' => "Centi",
            'd' => "Deci",
            'h' => "Hecto",
            'k' => "Kilo",
            'M' => "Mega",
            'G' => "Giga",
            'T' => "Tera",
            _ => unreachable!()
        });
    }

    let unit_str = if plural {
        match unit {
            "ft" => "Feet",
            "in" => "Inches",
            "pa" => "Pascal",
            "°C" => "Degrees Celsius",
            "°F" => "Degrees Fahrenheit",
            "K" => "Kelvin",
            "psi" => "Pounds per square inch",

            "m^2" => "Meters squared",
            "mi^2" => "Miles squared",
            "ft^2" => "Feet squared",
            "in^2" => "Inches squared",
            "yd^2" => "Yards squared",

            "m^3" => "Meters cubed",
            "mi^3" => "Miles cubed",
            "ft^3" => "Feet cubed",
            "in^3" => "Inches cubed",
            "yd^3" => "Yards cubed",
            _ => "",
        }
    } else { "" };

    if unit_str.is_empty() {
        let singular = match unit {
            // length
            "m" => "Meter",
            "mi" => "Mile",
            "ft" => "Foot",
            "in" => "Inch",
            "yd" => "Yard",
            // area
            "m^2" => "Meter squared",
            "mi^2" => "Mile squared",
            "ft^2" => "Foot squared",
            "in^2" => "Inch squared",
            "yd^2" => "Yard squared",
            "a" => "Are",
            // volume
            "l" => "Liter",
            "tsp" => "Teaspoon",
            "tbsp" => "Tablespoon",
            "floz" => "Fluid Ounce",
            "cup" => "Cup",
            "m^3" => "Meter cubed",
            "mi^3" => "Mile cubed",
            "ft^3" => "Foot cubed",
            "in^3" => "Inch cubed",
            "yd^3" => "Yard cubed",
            // angle
            "°" => "Degree",
            "rad" => "Radian",
            // time
            "s" => "Second",
            "min" => "Minute",
            "h" => "Hour",
            "d" => "Day",
            "mo" => "Month",
            "y" => "Year",
            // mass
            "g" => "Gram",
            "lb" => "Pound",
            "t" => "Tonne",
            // pressure
            "Pa" => "Pascal",
            "bar" => "Bar",
            "psi" => "Pound per square inch",
            // temperature
            "°C" => "Degree Celsius",
            "°F" => "Degree Fahrenheit",
            "K" => "Kelvin",
            // misc
            "cal" => "Calorie",
            "b" => "Byte",
            _ => ""
        };

        if is_currency(unit) {
            if result.len() - 1 != 0 { result.push(' '); }
            result += unit;
        } else if !singular.is_empty() {
            if prefix.is_some() {
                result.push_str(lowercase_first(singular).as_str());
            } else {
                result.push_str(singular);
            }

            if plural {
                result.push('s');
            }
        } else {
            unreachable!()
        }
    } else if prefix.is_some() {
        result.push_str(lowercase_first(unit_str).as_str());
    } else {
        result.push_str(unit_str);
    }

    result
}