/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::f64::consts::{E, PI, TAU};

use crate::{
    astgen::ast::AstNode,
    common::ErrorType,
    Currencies,
    Engine,
};
use crate::engine::CalculationResult;

use self::units::Unit;

pub mod units;
pub mod currencies;

// These files are generated in build.rs during build time
mod default_currencies;
mod unit_conversion;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Variable(pub f64, pub Option<Unit>);

const STANDARD_VARIABLES: [&str; 4] = ["pi", "e", "tau", "ans"];
const VAR_PI: &Variable = &Variable(PI, None);
const VAR_E: &Variable = &Variable(E, None);
const VAR_TAU: &Variable = &Variable(TAU, None);

#[derive(Default, Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Function(pub(crate) Vec<String>, pub(crate) Vec<AstNode>);

#[derive(Debug)]
pub(crate) enum ArgCount {
    Single(usize),
    Multiple(&'static [usize]),
}

impl ArgCount {
    pub fn is_valid_count(&self, count: usize) -> bool {
        match self {
            Self::Single(n) => count == *n,
            Self::Multiple(options) => options.contains(&count),
        }
    }
}

const STANDARD_FUNCTIONS: [(&str, ArgCount); 17] = [
    ("sin", ArgCount::Single(1)), ("asin", ArgCount::Single(1)),
    ("cos", ArgCount::Single(1)), ("acos", ArgCount::Single(1)),
    ("tan", ArgCount::Single(1)), ("atan", ArgCount::Single(1)),
    ("ln", ArgCount::Single(1)), ("log", ArgCount::Single(2)), // log arg2 to base arg1
    ("sqrt", ArgCount::Single(1)), ("cbrt", ArgCount::Single(1)), ("root", ArgCount::Single(2)), // root with "index" arg1 of arg2
    ("abs", ArgCount::Single(1)),
    ("floor", ArgCount::Single(1)), ("ceil", ArgCount::Single(1)),
    ("clamp", ArgCount::Single(3)), ("map", ArgCount::Single(5)), // map arg1 from range arg2..arg3 to range arg4..arg5
    ("round", ArgCount::Multiple(&[1, 2])),
];

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Environment {
    ans: Variable,
    variables: Vec<(String, Variable)>,
    functions: Vec<(String, Function)>,
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
    }
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            ans: Variable(0.0, None),
            variables: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub(crate) fn clear(&mut self) {
        self.ans = Variable(0.0, None);
        self.variables.clear();
        self.functions.clear();
    }

    pub(crate) fn is_valid_variable(&self, var: &str) -> bool {
        if STANDARD_VARIABLES.contains(&var) { true } else {
            for (name, _) in &self.variables {
                if var == name { return true; }
            }
            false
        }
    }

    pub(crate) fn is_standard_variable(&self, var: &str) -> bool {
        STANDARD_VARIABLES.contains(&var)
    }

    pub(crate) fn resolve_variable(&self, var: &str) -> Result<&Variable, ErrorType> {
        match var {
            "pi" => Ok(VAR_PI),
            "e" => Ok(VAR_E),
            "tau" => Ok(VAR_TAU),
            "ans" => Ok(&self.ans),
            _ => {
                for (name, variable) in &self.variables {
                    if name == var { return Ok(variable); }
                }
                Err(ErrorType::UnknownVariable(var.to_owned()))
            }
        }
    }

    pub(crate) fn set_variable(&mut self, var: &str, value: Variable) -> Result<(), ErrorType> {
        if var == "ans" {
            self.ans = value;
            return Ok(());
        } else if self.is_standard_variable(var) {
            return Err(ErrorType::ReservedVariable(var.to_owned()));
        }

        for (i, (name, _)) in self.variables.iter().enumerate() {
            if name == var {
                self.variables[i].1 = value;
                return Ok(());
            }
        }

        self.variables.push((var.to_string(), value));
        Ok(())
    }

    pub(crate) fn set_ans_variable(&mut self, value: Variable) { self.ans = value; }

    pub(crate) fn remove_variable(&mut self, var: &str) -> Result<(), ErrorType> {
        if var == "ans" {
            self.ans = Variable(0.0, None);
            return Ok(());
        } else if self.is_standard_variable(var) {
            return Err(ErrorType::ReservedVariable(var.to_owned()));
        }

        for (i, (name, _)) in self.variables.iter().enumerate() {
            if name == var {
                self.variables.remove(i);
                break;
            }
        }

        Ok(())
    }

    pub(crate) fn is_valid_function(&self, name: &str) -> bool {
        for (f, _) in STANDARD_FUNCTIONS {
            if f == name { return true; }
        }
        for (f, _) in &self.functions {
            if f == name { return true; }
        }
        false
    }

    pub(crate) fn is_standard_function(&self, f: &str) -> bool {
        for (name, _) in STANDARD_FUNCTIONS {
            if name == f { return true; }
        }
        false
    }

    pub(crate) fn get_function(&self, f: &str) -> Option<&Function> {
        for (name, function) in &self.functions {
            if name == f { return Some(function); }
        }
        None
    }

    pub(crate) fn function_argument_count(&self, name: &str) -> Option<ArgCount> {
        for (f, arg_count) in STANDARD_FUNCTIONS {
            if f == name { return Some(arg_count); }
        }
        for (f, Function(args, _)) in &self.functions {
            if f == name { return Some(ArgCount::Single(args.len())); }
        }
        None
    }

    pub(crate) fn resolve_function(
        &self,
        f: &str,
        arg_results: &[CalculationResult],
    ) -> Result<(f64, Option<Unit>), ErrorType> {
        let args = arg_results.iter().map(|r| r.result).collect::<Vec<_>>();
        let as_radians = |i: usize| {
            if let Some(unit) = &arg_results[i].unit {
                if unit.0 == "°" {
                    return args[i].to_radians();
                }
            }
            args[i]
        };

        let unit_0 = &arg_results[0].unit;

        match f {
            "sin" => Ok((as_radians(0).sin(), None)),
            "asin" => {
                if args[0] < -1.0 || args[0] > 1.0 {
                    return Err(ErrorType::NotANumber);
                }
                Ok((args[0].asin(), Some(Unit::from("rad"))))
            }
            "cos" => Ok((as_radians(0).cos(), None)),
            "acos" => {
                if args[0] < -1.0 || args[0] > 1.0 {
                    return Err(ErrorType::NotANumber);
                }
                Ok((args[0].acos(), Some(Unit::from("rad"))))
            }
            "tan" => Ok((as_radians(0).tan(), None)),
            "atan" => Ok((args[0].atan(), Some(Unit::from("rad")))),
            "ln" => Ok((args[0].ln(), unit_0.clone())),
            "log" => Ok((if args[0] == 2.0 {
                args[1].log2()
            } else if args[0] == 10.0 {
                args[1].log10()
            } else {
                args[1].log(args[0])
            }, unit_0.clone())),
            "sqrt" => Ok((args[0].sqrt(), unit_0.clone())),
            "cbrt" => Ok((args[0].cbrt(), unit_0.clone())),
            "root" => Ok((args[1].powf(1.0 / args[0]), unit_0.clone())),
            "abs" => Ok((args[0].abs(), unit_0.clone())),
            "floor" => Ok((args[0].floor(), unit_0.clone())),
            "ceil" => Ok((args[0].ceil(), unit_0.clone())),
            "clamp" => {
                if args[1] > args[2] {
                    return Err(ErrorType::Arg1GreaterThanArg2);
                }
                Ok((args[0].clamp(args[1], args[2]), unit_0.clone()))
            }
            "map" => {
                let a1 = args[1];
                let b1 = args[2];
                let a2 = args[3];
                let b2 = args[4];
                Ok(((args[0] - a1) * (b2 - a2) / (b1 - a1) + a2, None))
            }
            "round" => {
                let result = if let Some(decimal_places) = args.get(1) {
                    if decimal_places.fract() != 0.0 {
                        return Err(ErrorType::ExpectedInteger(*decimal_places));
                    }
                    let multiplier = 10.0f64.powf(*decimal_places);
                    (args[0] * multiplier).round() / multiplier
                } else {
                    args[0].round()
                };
                Ok((result, unit_0.clone()))
            }
            _ => Err(ErrorType::UnknownFunction(f.to_owned())),
        }
    }

    pub(crate) fn resolve_custom_function(&self, f: &str, args: &[f64], currencies: &Currencies) -> Result<(f64, Option<Unit>), ErrorType> {
        for (name, func) in &self.functions {
            if name == f {
                return self.resolve_specific_function(func, args, currencies);
            }
        }

        Err(ErrorType::UnknownFunction(f.to_owned()))
    }

    pub fn resolve_specific_function(&self, f: &Function, args: &[f64], currencies: &Currencies) -> Result<(f64, Option<Unit>), ErrorType> {
        let mut temp_env = self.clone();
        for (i, arg) in args.iter().enumerate() {
            temp_env.set_variable(&f.0[i], Variable(*arg, None))?;
        }

        let value = Engine::evaluate(f.1.clone(), &temp_env, currencies);

        match value {
            Ok(res) => Ok((res.result, res.unit)),
            Err(e) => Err(e.error)
        }
    }

    pub(crate) fn set_function(&mut self, f: &str, value: Function) -> Result<(), ErrorType> {
        if self.is_standard_function(f) { return Err(ErrorType::ReservedFunction(f.to_owned())); }

        for (i, (name, _)) in self.functions.iter().enumerate() {
            if name == f {
                self.functions[i].1 = value;
                return Ok(());
            }
        }

        self.functions.push((f.to_string(), value));
        Ok(())
    }

    pub(crate) fn remove_function(&mut self, f: &str) -> Result<(), ErrorType> {
        if self.is_standard_function(f) { return Err(ErrorType::ReservedFunction(f.to_owned())); }

        for (i, (name, _)) in self.functions.iter().enumerate() {
            if name == f {
                self.functions.remove(i);
                return Ok(());
            }
        }

        Ok(())
    }
}
