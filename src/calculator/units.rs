use std::f64::consts::PI;
use ::common::{Result, ErrorType};

const UNITS: [&str; 19] = [
    "m", "mi", "ft", "in", "yd", // distance
    "s", "min", "h", // time
    "g", "lb", // mass
    "pa", "bar", // pressure
    "°", "rad", // angle
    "°c", "°f", "k", // temperature
    "cal", "b", // misc
];

// Stores prefix with it's power (e.g. k => * 10^3)
const PREFIXES: [(char, i32); 9] = [
    ('m', -3), ('c', -2), ('d', -1),
    ('\0', 0),
    ('h', 2), ('k', 3), ('M', 6), ('g', 9), ('t', 12)
];

pub fn is_valid_unit(s: &str) -> bool {
    let lowercase = s.to_lowercase();
    if lowercase.is_empty() { return false; }
    if UNITS.contains(&lowercase.as_str()) { return true; }

    let first = s.chars().next().unwrap();
    for prefix in PREFIXES {
        if prefix.0 == first {
            let unit = &s[1..];
            return !unit.is_empty() && UNITS.contains(&unit);
        }
    }
    false
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

    match (src_unit.to_lowercase().as_str(), dst_unit.to_lowercase().as_str()) {
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

        // time
        ("s", "min") => Ok(n / 60.0),
        ("s", "h") => Ok(n / 3600.0),

        ("min", "s") => Ok(n * 60.0),
        ("min", "h") => Ok(n / 60.0),

        ("h", "s") => Ok(n * 3600.0),
        ("h", "min") => Ok(n / 60.0),

        // mass
        ("g", "lb") => Ok(n / 453.59237),
        ("lb", "g") => Ok(n * 453.59237),

        // pressure
        ("pa", "bar") => Ok(n / 100_000.0),
        ("bar", "pa") => Ok(n * 100_000.0),

        // angle
        ("°", "rad") => Ok(n * PI / 180.0),
        ("rad", "°") => Ok(n * 180.0 / PI),

        // temperature
        ("°c", "°f") => Ok((n * 9.0 / 5.0) + 32.0),
        ("°c", "k") => Ok(n + 273.15),

        ("°f", "°c") => Ok((n - 32.0) * 5.0 / 9.0),
        ("°f", "k") => Ok((n - 32.0) * 5.0 / 9.0 - 273.15),

        ("k", "°c") => Ok(n - 273.15),
        ("k", "°f") => Ok((n - 273.15) * 9.0 / 5.0 + 32.0),
        _ => Err(ErrorType::UnknownConversion.with(range.clone())),
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
            'g' => "Giga",
            't' => "Tera",
            _ => unreachable!()
        });
    }

    let unit_str = if plural {
        match unit {
            "ft" => "Feet",
            "in" => "Inches",
            "pa" => "Pascal",
            "°c" => "Degrees Celsius",
            "°f" => "Degrees Fahrenheit",
            "k" => "Kelvin",
            _ => "",
        }
    } else { "" };

    if unit_str.is_empty() {
        let singular = match unit {
            "m" => "Meter",
            "mi" => "Mile",
            "ft" => "Foot",
            "in" => "Inch",
            "yd" => "Yard",
            "s" => "Second",
            "min" => "Minute",
            "h" => "Hour",
            "g" => "Gram",
            "lb" => "Pound",
            "pa" => "Pascal",
            "bar" => "Bar",
            "°" => "Degree",
            "rad" => "Radian",
            "°c" => "Degree Celsius",
            "°f" => "Degree Fahrenheit",
            "k" => "Kelvin",
            "cal" => "Calorie",
            "b" => "Byte",
            _ => unreachable!(),
        };

        if !plural {
            if prefix.is_some() {
                result.push_str(lowercase_first(singular).as_str());
            } else {
                result.push_str(singular);
            }
        } else {
            if prefix.is_some() {
                result.push_str(lowercase_first(singular).as_str());
            } else {
                result.push_str(singular);
            }
            result.push('s');
        }
    } else if prefix.is_some() {
        result.push_str(lowercase_first(unit_str).as_str());
    } else {
        result.push_str(unit_str);
    }

    result
}