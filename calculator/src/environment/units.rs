/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;

use crate::{common::{ErrorType, Result}, environment::currencies::{Currencies, is_currency}, environment::unit_conversion::{convert_units, format_unit, UNITS}, error};

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum Unit {
    Product(Vec<Unit>),
    Fraction(Box<Unit>, Box<Unit>),
    Unit(String, f64),
}

impl Unit {
    pub fn new(str: &str, power: f64) -> Unit { Unit::Unit(str.to_string(), power) }

    pub fn new_fraction(numerator: &str, denominator: &str) -> Unit {
        Unit::Fraction(
            Box::new(Unit::new(numerator, 1.0)),
            Box::new(Unit::new(denominator, 1.0)),
        )
    }

    pub fn push_unit(&mut self, other: Unit) {
        match self {
            Self::Unit(..) => {
                if let Self::Fraction(mut other_num, other_denom) = other {
                    other_num.push_unit(self.clone());
                    *self = Unit::Fraction(other_num, other_denom);
                } else {
                    *self = Unit::Product(vec![self.clone(), other]);
                }
            }
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
    ///
    /// Returns whether the unit should be kept after calling the function.
    pub fn simplify(&mut self) -> bool {
        match self {
            Self::Fraction(num, denom) => 'blk: {
                num.simplify();

                if !denom.simplify() {
                    let new_self = (**num).clone();
                    *self = new_self;
                    break 'blk;
                }

                fn can_be_shortened(first: &Unit, second: &Unit) -> bool {
                    if let Unit::Unit(first_unit, _) = first {
                        if let Unit::Unit(second_unit, _) = second {
                            return first_unit == second_unit;
                        }
                    }

                    *first == *second
                }

                fn shorten_powers(num: &mut f64, denom: &mut f64) -> (bool, bool, bool) {
                    let mut result = (false, false, false);
                    if *num < *denom {
                        *denom -= *num;
                        if *denom == 0.0 { result.2 = true }
                        result.1 = true;
                        result.0 = true;
                    } else if *denom < *num {
                        *num -= *denom;
                        if *num == 0.0 { result.1 = true; }
                        result.2 = true;
                        result.0 = true;
                    }

                    result
                }

                match (&mut **num, &mut **denom) {
                    // Reduce the fraction by finding units that are both in the numerator and in the denominator
                    (Self::Product(num_units), Self::Product(denom_units)) => {
                        let mut i = 0usize;
                        while i < num_units.len() {
                            if num_units.len() == 1 && denom_units.len() > 1 {
                                break;
                            }

                            if let Some(denom_i) = denom_units.iter().position(|denom_unit| can_be_shortened(denom_unit, &num_units[i])) {
                                'inner: {
                                    if let Unit::Unit(_, denom_power) = &mut denom_units[denom_i] {
                                        if let Unit::Unit(_, num_power) = &mut num_units[i] {
                                            let (break_, remove_num, remove_denom) = shorten_powers(num_power, denom_power);
                                            if remove_num { num_units.remove(i); }
                                            if remove_denom { denom_units.remove(denom_i); }
                                            if break_ { break 'inner; }
                                        }
                                    }

                                    num_units.remove(i);
                                    denom_units.remove(denom_i);
                                }

                                if num_units.is_empty() && denom_units.is_empty() { return false; }

                                if denom_units.is_empty() && !num_units.is_empty() {
                                    let mut units = std::mem::take(num_units);
                                    if units.len() == 1 {
                                        *self = units.remove(0);
                                    } else {
                                        *self = Self::Product(units);
                                    }
                                    break 'blk;
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
                    (Self::Product(num_units), Self::Unit(..)) if num_units.len() > 1 => {
                        if let Some(i) = num_units.iter().position(|u| can_be_shortened(u, denom)) {
                            let mut did_remove_denom = false;
                            'inner: {
                                if let Unit::Unit(_, denom_power) = &mut **denom {
                                    if let Unit::Unit(_, num_power) = &mut num_units[i] {
                                        let (break_, remove_num, remove_denom) = shorten_powers(num_power, denom_power);
                                        if remove_num { num_units.remove(i); }
                                        if remove_denom { did_remove_denom = true; }
                                        if break_ { break 'inner; }
                                    }
                                }

                                num_units.remove(i);
                                did_remove_denom = true;
                            }

                            if did_remove_denom {
                                if num_units.len() == 1 {
                                    let new_self = num_units.remove(0);
                                    *self = new_self;
                                } else {
                                    *self = (**num).clone();
                                }
                            } else if num_units.len() == 1 {
                                let new_numerator = num_units.remove(0);
                                **num = new_numerator;
                            }
                        }
                    }
                    (Self::Unit(num_str, num_pow), Self::Unit(denom_str, denom_pow)) => {
                        if num_str == denom_str {
                            if num_pow == denom_pow {
                                return false;
                            } else if denom_pow < num_pow {
                                *num_pow -= *denom_pow;
                                *self = Self::Unit(num_str.clone(), *num_pow);
                            }
                        }
                    }
                    _ => {}
                }
            }
            Self::Product(units) => {
                // Combine equal units into the unit with a power
                {
                    let units_value = std::mem::take(units);

                    let mut unique_units: Vec<(String, f64)> = vec![];
                    let mut remaining_units = vec![];
                    for unit in units_value {
                        if let Unit::Unit(unit, power) = unit {
                            if let Some((_, p)) = unique_units.iter_mut().find(|(str, _)| str == &unit) {
                                *p += power;
                            } else {
                                unique_units.push((unit, power));
                            }
                        } else {
                            remaining_units.push(unit);
                        }
                    }

                    *units = remaining_units.into_iter()
                        .chain(unique_units.iter()
                            .map(|(unit, power)| Unit::new(unit, *power))
                        )
                        .collect::<Vec<_>>();
                }

                let mut i = 0usize;
                while i < units.len() {
                    if !units[i].simplify() {
                        units.remove(i);
                        continue;
                    }
                    i += 1;
                }

                if units.len() == 1 {
                    *self = units.remove(0);
                }
            }
            Self::Unit(..) => {}
        }

        true
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
                    if !matches!(**numerator, Self::Unit(..)) {
                        result += &format!("({})", numerator.format(full_unit, plural));
                    } else {
                        result += &numerator.format(full_unit, plural);
                    }

                    result.push('/');

                    if !matches!(**denominator, Self::Unit(..)) {
                        result += &format!("({})", denominator.format(full_unit, plural));
                    } else {
                        result += &denominator.format(full_unit, plural);
                    }
                    result
                }
                Self::Unit(str, power) => {
                    let mut result = str.to_string();
                    if *power != 1.0 { result += &format!("^{power}"); }
                    result
                }
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

                    if units.len() > 2 {
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
                Self::Unit(str, power) => format!("{}{}", format_unit(str, plural), format_unit_power(*power)),
            }
        }
    }
}

fn format_unit_power(pow: f64) -> String {
    if pow == 1.0 {
        String::new()
    } else if pow == 2.0 {
        " squared".to_string()
    } else if pow == 3.0 {
        " cubed".to_string()
    } else {
        format!(" ^ {pow}")
    }
}

impl From<&str> for Unit {
    fn from(value: &str) -> Self { Self::new(value, 1.0) }
}

impl std::fmt::Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format(false, false))
    }
}

// Stores prefix with its power (e.g. k => * 10^3)
pub const PREFIXES: [(char, i32); 14] = [
    ('n', -9), ('m', -3), ('c', -2), ('d', -1),
    ('\0', 0),
    ('h', 2), ('k', 3), ('M', 6), ('G', 9), ('T', 12), ('P', 15), ('E', 18), ('Z', 21), ('Y', 24),
];

pub fn prefix_to_string(prefix: char) -> Option<&'static str> {
    match prefix {
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
        Unit::Unit(src, power) => {
            let Unit::Unit(dst, dst_power) = dst_unit else { error!(UnitsNotMatching: range.clone()); };
            convert_units((src, *power), (dst, *dst_power), n, currencies, range)
        }
    }
}
