use ::common::{ErrorType};
use std::f64::consts::{PI, E, TAU};
use astgen::ast::AstNode;
use evaluate;

pub mod units;

#[derive(Debug, Clone)]
pub struct Variable(pub f64, pub Option<String>);

const STANDARD_VARIABLES: [&str; 4] = ["pi", "e", "tau", "ans"];
const VAR_PI: &Variable = &Variable(PI, None);
const VAR_E: &Variable = &Variable(E, None);
const VAR_TAU: &Variable = &Variable(TAU, None);

#[derive(Debug, Clone)]
pub struct Function(pub Vec<String>, pub Vec<AstNode>);

const STANDARD_FUNCTIONS: [(&str, usize); 14] = [
    ("sin", 1), ("asin", 1),
    ("cos", 1), ("acos", 1),
    ("tan", 1), ("atan", 1),
    ("ln", 1), ("log", 2), // log arg2 to base arg1
    ("sqrt", 1),
    ("abs", 1),
    ("floor", 1), ("ceil", 1),
    ("clamp", 3), ("map", 5), // map arg1 from range arg2..arg3 to range arg4..arg5
];

#[derive(Clone)]
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

    pub fn clear(&mut self) {
        self.ans = Variable(0.0, None);
        self.variables.clear();
        self.functions.clear();
    }

    pub fn is_valid_variable(&self, var: &str) -> bool {
        if STANDARD_VARIABLES.contains(&var) { true } else {
            for (name, _) in &self.variables {
                if var == name { return true; }
            }
            false
        }
    }

    pub fn is_standard_variable(&self, var: &str) -> bool {
        STANDARD_VARIABLES.contains(&var)
    }

    pub fn resolve_variable(&self, var: &str) -> Result<&Variable, ErrorType> {
        match var {
            "pi" => Ok(VAR_PI),
            "e" => Ok(VAR_E),
            "tau" => Ok(VAR_TAU),
            "ans" => Ok(&self.ans),
            _ => {
                for (name, variable) in &self.variables {
                    if name == var { return Ok(variable); }
                }
                Err(ErrorType::UnknownVariable)
            }
        }
    }

    pub fn set_variable(&mut self, var: &str, value: Variable) -> Result<(), ErrorType> {
        if var == "ans" {
            self.ans = value;
            return Ok(());
        } else if self.is_standard_variable(var) {
            return Err(ErrorType::ReservedVariable);
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

    pub fn remove_variable(&mut self, var: &str) -> Result<(), ErrorType> {
        if var == "ans" {
            self.ans = Variable(0.0, None);
            return Ok(());
        } else if self.is_standard_variable(var) {
            return Err(ErrorType::ReservedVariable);
        }

        for (i, (name, _)) in self.variables.iter().enumerate() {
            if name == var {
                self.variables.remove(i);
                break;
            }
        }

        Ok(())
    }

    pub fn is_valid_function(&self, name: &str) -> bool {
        for (f, _) in STANDARD_FUNCTIONS {
            if f == name { return true; }
        }
        for (f, _) in &self.functions {
            if f == name { return true; }
        }
        false
    }

    pub fn is_standard_function(&self, f: &str) -> bool {
        for (name, _) in STANDARD_FUNCTIONS {
            if name == f { return true; }
        }
        false
    }

    pub fn function_argument_count(&self, name: &str) -> Option<usize> {
        for (f, arg_count) in STANDARD_FUNCTIONS {
            if f == name { return Some(arg_count); }
        }
        for (f, Function(args, _)) in &self.functions {
            if f == name { return Some(args.len()); }
        }
        None
    }

    pub fn resolve_function(&self, f: &str, args: &[f64]) -> Result<f64, ErrorType> {
        match f {
            "sin" => Ok(args[0].to_radians().sin()),
            "asin" => {
                if args[0] < -1.0 || args[0] > 1.0 {
                    return Err(ErrorType::NotANumber);
                }
                Ok(args[0].asin().to_degrees())
            }
            "cos" => Ok(args[0].to_radians().cos()),
            "acos" => {
                if args[0] < -1.0 || args[0] > 1.0 {
                    return Err(ErrorType::NotANumber);
                }
                Ok(args[0].acos().to_degrees())
            }
            "tan" => Ok(args[0].to_radians().tan()),
            "atan" => Ok(args[0].atan().to_degrees()),
            "ln" => Ok(args[0].ln()),
            "log" => Ok(if args[0] == 2.0 {
                args[1].log2()
            } else if args[0] == 10.0 {
                args[1].log10()
            } else {
                args[1].log(args[0])
            }),
            "sqrt" => Ok(args[0].sqrt()),
            "abs" => Ok(args[0].abs()),
            "floor" => Ok(args[0].floor()),
            "ceil" => Ok(args[0].ceil()),
            "clamp" => {
                if args[1] > args[2] {
                    return Err(ErrorType::Arg1GreaterThanArg2);
                }
                Ok(args[0].clamp(args[1], args[2]))
            }
            "map" => {
                let a1 = args[1];
                let b1 = args[2];
                let a2 = args[3];
                let b2 = args[4];
                Ok((args[0] - a1) * (b2 - a2) / (b1 - a1) + a2)
            }
            _ => Err(ErrorType::UnknownFunction),
        }
    }

    pub fn resolve_custom_function(&self, f: &str, args: &[f64]) -> Result<(f64, Option<String>), ErrorType> {
        for (name, Function(function_args, ast)) in &self.functions {
            if name == f {
                let mut temp_env = Environment::new();
                temp_env.ans = self.ans.clone();
                temp_env.variables = self.variables.clone();

                for i in 0..args.len() {
                    temp_env.set_variable(&function_args[i], Variable(args[i], None))?;
                }

                let value = evaluate(ast.clone(), &mut temp_env);
                for name in function_args { temp_env.remove_variable(name)?; }

                return match value {
                    Ok(res) => Ok((res.result, res.unit)),
                    Err(e) => Err(e.error)
                };
            }
        }

        Err(ErrorType::UnknownFunction)
    }

    pub fn set_function(&mut self, f: &str, value: Function) -> Result<(), ErrorType> {
        if self.is_standard_function(f) { return Err(ErrorType::ReservedFunction); }

        for (i, (name, _)) in self.functions.iter().enumerate() {
            if name == f {
                self.functions[i].1 = value;
                return Ok(());
            }
        }

        self.functions.push((f.to_string(), value));
        Ok(())
    }

    pub fn remove_function(&mut self, f: &str) -> Result<(), ErrorType> {
        if self.is_standard_function(f) { return Err(ErrorType::ReservedFunction); }

        for (i, (name, _)) in self.functions.iter().enumerate() {
            if name == f {
                self.functions.remove(i);
                return Ok(());
            }
        }

        Ok(())
    }
}
