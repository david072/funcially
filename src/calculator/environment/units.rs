/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::f64::consts::PI;
use ::common::{Result, ErrorType};

const UNITS: [&str; 30] = [
    "m", "mi", "ft", "in", "yd", // length
    "a", // area
    "l", "tsp", "tbsp", "floz", "cup", // volume
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
    UNITS.contains(&str)
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

pub fn convert(src_unit: &str, dst_unit: &str, n: f64, range: &std::ops::Range<usize>) -> Result<f64> {
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
        ("m", "in") => Ok(n / 0.0254),
        ("m", "yd") => Ok(n / 0.9144),

        ("ft", "m") => Ok(n / 3.281),
        ("ft", "mi") => Ok(n / 5280.0),
        ("ft", "in") => Ok(n * 12.0),
        ("ft", "yd") => Ok(n / 3.0),

        ("mi", "m") => Ok(n * 1609.344),
        ("mi", "ft") => Ok(n * 5280.0),
        ("mi", "in") => Ok(n * 63360.0),
        ("mi", "yd") => Ok(n * 1760.0),

        ("in", "m") => Ok(n * 0.0254),
        ("in", "ft") => Ok(n / 12.0),
        ("in", "mi") => Ok(n / 63360.0),
        ("in", "yd") => Ok(n / 36.0),

        ("yd", "m") => Ok(n * 0.9144),
        ("yd", "ft") => Ok(n * 3.0),
        ("yd", "mi") => Ok(n / 1760.0),
        ("yd", "in") => Ok(n * 36.0),

        // area

        // volume
        ("l", "tsp") => Ok(n * 202.9),
        ("l", "tbsp") => Ok(n * 67.628),
        ("l", "floz") => Ok(n * 33.814),
        ("l", "cup") => Ok(n * 4.227),

        ("tsp", "l") => Ok(n / 202.9),
        ("tsp", "tbsp") => Ok(n / 3.0),
        ("tsp", "floz") => Ok(n / 6.0),
        ("tsp", "cup") => Ok(n / 48.0),

        ("tbsp", "l") => Ok(n / 67.628),
        ("tbsp", "tsp") => Ok(n * 3.0),
        ("tbsp", "floz") => Ok(n / 2.0),
        ("tbsp", "cup") => Ok(n / 16.0),

        ("floz", "l") => Ok(n / 33.814),
        ("floz", "tsp") => Ok(n * 6.0),
        ("floz", "tbsp") => Ok(n * 2.0),
        ("floz", "cup") => Ok(n / 8.0),

        ("cup", "l") => Ok(n / 4.227),
        ("cup", "tsp") => Ok(n * 48.0),
        ("cup", "tbsp") => Ok(n * 16.0),
        ("cup", "floz") => Ok(n * 8.0),

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
        _ => Err(ErrorType::UnknownConversion(
            src_unit.to_string(), dst_unit.to_string()).with(range.clone())),
    }
}

pub fn format(unit: &str, plural: bool) -> String {
    fn lowercase_first(s: &str) -> String {
        let mut chars = s.chars();
        match chars.next() {
            Some(char) => char.to_lowercase().chain(chars).collect(),
            None => String::new(),
        }
    }

    let prefix = unit_prefix(unit).map(|x| x.0);
    let unit = if prefix.is_some() { unit[1..].to_lowercase() } else { unit.to_lowercase() };
    let unit = unit.as_str();

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
            "a" => "Are",
            // volume
            "l" => "Liter",
            "tsp" => "Teaspoon",
            "tbsp" => "Tablespoon",
            "floz" => "Fluid Ounce",
            "cup" => "Cup",
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
            _ => unreachable!(),
        };

        if prefix.is_some() {
            result.push_str(lowercase_first(singular).as_str());
        } else {
            result.push_str(singular);
        }

        if plural {
            result.push('s');
        }
    } else if prefix.is_some() {
        result.push_str(lowercase_first(unit_str).as_str());
    } else {
        result.push_str(unit_str);
    }

    result
}