/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;

use crate::{common::{ErrorType, Result}, environment::currencies::{Currencies, is_currency}, environment::unit_conversion::{convert_units, format_unit, UNITS}, error};

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub enum Unit {
    Product(Vec<Unit>),
    Fraction(Box<Unit>, Box<Unit>),
    Unit(String),
}

impl Unit {
    pub fn push_unit(&mut self, other: Unit) {
        match self {
            ref unit @ Self::Unit(_) => *self = Unit::Product(vec![(*unit).clone(), other]),
            Self::Product(units) => units.push(other),
            Self::Fraction(num, denom) => {
                if let Self::Fraction(other_num, other_denom) = other {
                    num.push_unit(*other_num);
                    denom.push_unit(*other_denom);
                } else {
                    num.push_unit(other);
                }
            }
        }
    }

    /// Tries to simplify the unit by e.g. reducing fractions
    pub fn simplify(&mut self) {
        match self {
            Self::Fraction(num, denom) => {
                // TODO: More cases
                match (&mut **num, &mut **denom) {
                    // Reduce the fraction by finding units that are both in the numerator and in the denominator
                    (Self::Product(num_units), Self::Product(denom_units)) => {
                        let mut i = 0usize;
                        while i < num_units.len() {
                            if let Some(denom_i) = denom_units.iter().position(|denom_unit| *denom_unit == num_units[i]) {
                                num_units.remove(i);
                                denom_units.remove(denom_i);

                                // FIXME: Allow the unit to remove itself (e.g. `h/h`)
                                if num_units.len() == 1 { break; }

                                if denom_units.is_empty() {
                                    let units = std::mem::take(denom_units);
                                    *self = Self::Product(units);
                                    return;
                                }
                                continue;
                            }
                            i += 1;
                        }

                        // Remove unnecessary `Unit::Product`s
                        if num_units.len() == 1 { **num = num_units.remove(0); }
                        if denom_units.len() == 1 { **denom = denom_units.remove(0); }
                    }
                    // Reduce the fraction by checking if the denominator is in the numerator
                    (Self::Product(num_units), Self::Unit(_)) if num_units.len() > 1 => {
                        if let Some(i) = num_units.iter().position(|u| *u == **denom) {
                            num_units.remove(i);
                            if num_units.len() == 1 {
                                let new_self = num_units.remove(0);
                                *self = new_self;
                            } else {
                                *self = (**num).clone();
                            }
                        }
                    }
                    _ => {}
                }
            }
            Self::Product(units) => {
                for unit in units { unit.simplify(); }
            }
            Self::Unit(_) => {}
        }
    }

    pub fn format(&self, full_unit: bool, plural: bool) -> String {
        if !full_unit {
            match self {
                Self::Product(units) => {
                    units.iter().enumerate()
                        .map(|(i, unit)| (i, unit.format(full_unit, plural)))
                        .fold(String::new(), |mut acc, (i, str)| {
                            acc += &str;
                            if i != units.len() - 1 { acc.push('*'); }
                            acc
                        })
                }
                Self::Fraction(numerator, denominator) => {
                    let mut result = String::new();
                    if !matches!(**numerator, Self::Unit(_)) {
                        result += &format!("({})", numerator.format(full_unit, plural));
                    } else {
                        result += &numerator.format(full_unit, plural);
                    }

                    result.push('/');

                    if !matches!(**denominator, Self::Unit(_)) {
                        result += &format!("({})", denominator.format(full_unit, plural));
                    } else {
                        result += &denominator.format(full_unit, plural);
                    }
                    result
                }
                Self::Unit(str) => str.to_string(),
            }
        } else {
            match self {
                Self::Product(units) => {
                    let mut result = String::new();
                    result += &units.first().unwrap().format(full_unit, false);

                    fn lowercase_first(str: String) -> String {
                        let mut iter = str.chars();
                        iter.next().unwrap().to_lowercase().chain(iter).collect()
                    }

                    if units.len() > 3 {
                        for unit in &units[1..units.len() - 1] {
                            let str = unit.format(full_unit, false);
                            result += &lowercase_first(str);
                        }
                    }

                    if units.len() >= 2 {
                        let str = units.last().unwrap().format(full_unit, plural);
                        result += &lowercase_first(str);
                    }
                    result
                }
                Self::Fraction(numerator, denominator) => {
                    format!("{} per {}", numerator.format(full_unit, plural), denominator.format(full_unit, false))
                }
                Self::Unit(str) => format_unit(str, plural),
            }
        }
    }
}

impl From<&str> for Unit {
    fn from(value: &str) -> Self {
        Self::Unit(value.to_string())
    }
}

impl std::fmt::Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format(false, false))
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

pub fn is_unit_with_prefix(str: &str) -> bool {
    is_unit(str) || (is_prefix(str.chars().next().unwrap()) && is_unit(&str[1..]))
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
    match src_unit {
        Unit::Product(src_units) => {
            let Unit::Product(dst_units) = dst_unit else { error!(UnitsNotMatching: range.clone()); };
            src_units.iter()
                .zip(dst_units)
                .try_fold(n, |n, (src, dst)| {
                    convert(src, dst, n, currencies, range)
                })
        }
        Unit::Fraction(src_numerator, src_denominator) => {
            let Unit::Fraction(dst_numerator, dst_denominator) = dst_unit else { error!(UnitsNotMatching: range.clone()); };
            let numerator = convert(src_numerator, dst_numerator, n, currencies, range)?;
            let denominator = convert(src_denominator, dst_denominator, 1.0, currencies, range)?;
            Ok(numerator / denominator)
        }
        Unit::Unit(src) => {
            let Unit::Unit(dst) = dst_unit else { error!(UnitsNotMatching: range.clone()); };
            convert_units(src, dst, n, currencies, range)
        }
    }
}
