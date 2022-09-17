/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::{Display, Formatter};
use std::mem::{take, replace};
use crate::{
    astgen::ast::{Operator, AstNode, AstNodeData},
    match_ast_node,
    environment::{Environment, Variable, units::Unit},
    common::*,
    Currencies,
};

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Format { Decimal, Hex, Binary }

impl Display for Format {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Format::Decimal => write!(f, "decimal"),
            Format::Hex => write!(f, "hex"),
            Format::Binary => write!(f, "binary"),
        }
    }
}

impl Format {
    pub fn format(&self, n: f64) -> String {
        match self {
            Format::Decimal => round_dp(n, 10),
            Format::Hex => format!("{:#X}", n as i64),
            Format::Binary => format!("{:#b}", n as i64),
        }
    }
}

pub struct CalculationResult {
    pub result: f64,
    pub unit: Option<Unit>,
    pub is_long_unit: bool,
    pub format: Format,
}

impl CalculationResult {
    pub fn new(result: f64, unit: Option<Unit>, is_long_unit: bool, format: Format) -> CalculationResult {
        CalculationResult { result, unit, is_long_unit, format }
    }
}


pub struct Engine<'a> {
    ast: &'a mut Vec<AstNode>,
    env: &'a Environment,
    currencies: &'a Currencies,
}

impl<'a> Engine<'a> {
    pub fn evaluate(mut ast: Vec<AstNode>, env: &Environment, currencies: &Currencies) -> Result<CalculationResult> {
        if ast.len() == 1 && matches!(&ast[0].data, AstNodeData::Literal(_)) {
            ast[0].apply_modifiers()?;
            let result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
            let unit = take(&mut ast[0].unit);
            return Ok(CalculationResult::new(result, unit, true, ast[0].format));
        }

        let mut engine = Engine::new(&mut ast, env, currencies);
        engine.eval_functions()?;
        engine.eval_variables()?;
        engine.eval_groups()?;
        engine.eval_operators(&[Operator::Exponentiation, Operator::BitwiseAnd, Operator::BitwiseOr])?;
        engine.eval_operators(&[Operator::Multiply, Operator::Divide])?;
        engine.eval_operators(&[Operator::Plus, Operator::Minus])?;
        engine.eval_operators(&[Operator::Of, Operator::In])?;

        ast[0].apply_modifiers()?;
        let mut result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
        let format = ast[0].format;
        if format != Format::Decimal { result = result.trunc(); }

        Ok(CalculationResult::new(result, take(&mut ast[0].unit), false, format))
    }

    // TODO: - Units
    /// Solves a linear equation
    pub fn solve(
        lhs: Vec<AstNode>,
        rhs: Vec<AstNode>,
        is_question_mark_in_lhs: bool,
        env: &Environment,
        currencies: &Currencies,
    ) -> Result<CalculationResult> {
        let (unknown_side, result_side) = if is_question_mark_in_lhs {
            (lhs, rhs)
        } else {
            (rhs, lhs)
        };
        let target_result = Self::evaluate(result_side, env, currencies)?;
        let target_value = target_result.result;

        if unknown_side.len() == 1 {
            return Ok(target_result);
        }

        /// Validates that the question mark or its enclosing groups are not surrounded by a
        /// power sign (^).
        ///
        /// **Returns:** An error if it was surrounded by a power sign or a bool indicating whether a
        /// question mark was found.
        fn validate_unknown_side(ast: &[AstNode], is_surrounded_by_exponentiation: bool) -> Result<bool> {
            for i in 0..ast.len() {
                if !matches!(ast[i].data, AstNodeData::QuestionMark | AstNodeData::Group(_)) {
                    continue;
                }

                fn map_fn(node: &AstNode) -> bool {
                    matches!(node.data, AstNodeData::Operator(Operator::Exponentiation))
                }

                let prev_node = ast.get(i.saturating_sub(1)).map(map_fn);
                let next_node = ast.get(i + 1).map(map_fn);

                let has_power_sign = prev_node.unwrap_or(false) || next_node.unwrap_or(false);

                match ast[i].data {
                    AstNodeData::QuestionMark => {
                        if has_power_sign || is_surrounded_by_exponentiation {
                            // TODO: Better error range (power sign)
                            return Err(ErrorType::ForbiddenExponentiation.with(ast[i].range.clone()));
                        }

                        return Ok(true);
                    }
                    AstNodeData::Group(ref ast) => {
                        if validate_unknown_side(ast, has_power_sign || is_surrounded_by_exponentiation)? {
                            return Ok(true);
                        }
                    }
                    _ => {}
                }
            }

            Ok(false)
        }

        fn replace_question_mark(mut ast: Vec<AstNode>, value: f64) -> Vec<AstNode> {
            fn replace(ast: &mut [AstNode], value: f64) {
                for i in 0..ast.len() {
                    match ast[i].data {
                        AstNodeData::QuestionMark => {
                            ast[i] = AstNode::from(&ast[i], AstNodeData::Literal(value));
                            return;
                        }
                        AstNodeData::Group(ref mut ast) => replace(ast, value),
                        _ => {}
                    }
                }
            }

            replace(&mut ast, value);
            ast
        }

        if !validate_unknown_side(&unknown_side, false)? {
            return Err(ErrorType::ExpectedQuestionMark.with(
                unknown_side.first().unwrap().range.start..unknown_side.last().unwrap().range.end
            ));
        }

        // NOTE: General equation (if ? is a variable): f(?) = a
        //       => f(?) - a = 0    => y = f(?) - a
        //       With this, the result value of ? is the x-coordinate of the point where this graph
        //       intersects the x-axis.

        // Get two points from the linear function
        const X1: f64 = 1.0;
        const X2: f64 = 2.0;

        let first_ast = replace_question_mark(unknown_side.clone(), X1);
        let y1_result = Self::evaluate(first_ast, env, currencies)?;
        let y1 = y1_result.result - target_value;

        let second_ast = replace_question_mark(unknown_side, X2);
        let y2 = Self::evaluate(second_ast, env, currencies)?.result - target_value;

        // Calculate the variables for formula y = mx + c
        let m = (y2 - y1) / (X2 - X1);
        let c = y1 - m * X1;

        // y = mx + c           | -c
        // y - c = mx           | / m
        // (y - c) / m = x      with y = 0
        // x = -c / m
        let result = -c / m;

        let format = if target_result.format != Format::Decimal {
            target_result.format
        } else {
            y1_result.format
        };

        Ok(CalculationResult::new(result, None, false, format))
    }

    fn new(ast: &'a mut Vec<AstNode>, env: &'a Environment, currencies: &'a Currencies) -> Engine<'a> {
        Engine { ast, env, currencies }
    }

    fn eval_functions(&mut self) -> Result<()> {
        for node in self.ast.iter_mut() {
            let (func_name, arg_asts) = match node.data {
                AstNodeData::FunctionInvocation(ref name, ref args) => (name, args),
                _ => continue,
            };

            let mut args = Vec::new();
            for ast in arg_asts {
                args.push(Self::evaluate(ast.clone(), self.env, self.currencies)?.result);
            }
            let (result, unit) = match self.env.resolve_function(func_name, &args) {
                Ok(res) => (res, None),
                Err(ty) => {
                    if ty == ErrorType::UnknownFunction {
                        match self.env.resolve_custom_function(func_name, &args, self.currencies) {
                            Ok(res) => res,
                            Err(ty) => return Err(ty.with(node.range.clone())),
                        }
                    } else {
                        return Err(ty.with(node.range.clone()));
                    }
                }
            };
            let mut new_node = AstNode::from(node, AstNodeData::Literal(result));
            if unit.is_some() { new_node.unit = unit; }
            let _ = replace(node, new_node);
        }

        Ok(())
    }

    fn eval_variables(&mut self) -> Result<()> {
        for node in self.ast.iter_mut() {
            let var_name = match node.data {
                AstNodeData::VariableReference(ref name) => name.as_str(),
                _ => continue,
            };

            let Variable(number, unit) = match self.env.resolve_variable(var_name) {
                Ok(var) => var,
                Err(ty) => return Err(ty.with(node.range.clone())),
            };
            let mut new_node = AstNode::from(node, AstNodeData::Literal(*number));
            if unit.is_some() { new_node.unit = unit.clone(); }
            let _ = replace(node, new_node);
        }

        Ok(())
    }

    fn eval_groups(&mut self) -> Result<()> {
        for node in self.ast.iter_mut() {
            let group_ast = match &node.data {
                AstNodeData::Group(ast) => ast,
                _ => continue,
            };

            let group_result = Self::evaluate(group_ast.clone(), self.env, self.currencies)?;
            // Construct Literal node with the evaluated result
            let new_node = AstNode::from(node, AstNodeData::Literal(group_result.result));
            let _ = replace(node, new_node);
        }

        Ok(())
    }

    fn eval_operators(&mut self, operators: &[Operator]) -> Result<()> {
        let mut i = 0usize;
        while i < self.ast.len() - 1 {
            // there has got to be a better way to do this...
            let (lhs, operator, rhs) =
                if let [lhs, operator, rhs] = &mut self.ast[i..=i + 2] {
                    (lhs, operator, rhs)
                } else {
                    return Err(ErrorType::InvalidAst.with(0..0));
                };

            let op = match_ast_node!(AstNodeData::Operator(op), op, operator);

            if operators.contains(&op) {
                lhs.apply(operator, rhs, self.currencies)?;
                // remove operator and rhs
                self.ast.remove(i + 1);
                self.ast.remove(i + 1);
            } else {
                i += 2;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Result;
    use crate::{parse, tokenize, ParserResult};
    use crate::environment::units::format as format_unit;

    macro_rules! eval {
        ($str:expr) => {
            Engine::evaluate(
                if let ParserResult::Calculation(ast) = parse(&tokenize($str)?, &Environment::new())? { ast }
                else { panic!("Expected ParserResult::Calculation"); },
                &Environment::new(),
                &Currencies::none(),
            )
        }
    }

    macro_rules! expect {
        ($str:expr, $res:expr) => {
            assert_eq!(eval!($str)?.result, $res)
        }
    }

    macro_rules! expect_error {
        ($str:expr, $error:ident) => {
            match eval!($str) {
                Err(e) => assert!(matches!(e.error, ErrorType::$error)),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn only_one() -> Result<()> {
        expect!("3", 3.0);
        Ok(())
    }

    #[test]
    fn plus_and_minus() -> Result<()> {
        expect!("3 + 5 - -2", 10.0);
        Ok(())
    }

    #[test]
    fn multiply_and_divide() -> Result<()> {
        expect!("3 * 2 / 3", 2.0);
        Ok(())
    }

    #[test]
    fn extended_operators() -> Result<()> {
        expect!("3 ^ 3 | 2 & 10", 10.0);
        Ok(())
    }

    #[test]
    fn modifiers() -> Result<()> {
        expect!("4!%", 0.24);
        expect!("!5", 2.0);
        Ok(())
    }

    #[test]
    fn operator_order() -> Result<()> {
        expect!("2% of 3 ^ 2 + 3 + 4 * 2", 0.4);
        Ok(())
    }

    #[test]
    fn groups() -> Result<()> {
        expect!("2 * (2 + 2)", 8.0);
        expect!("2 * (2 + (1 + 1))", 8.0);
        Ok(())
    }

    #[test]
    fn variables() -> Result<()> {
        expect!("pi", std::f64::consts::PI);
        expect!("2 * pi", std::f64::consts::PI * 2.0);
        Ok(())
    }

    #[test]
    fn functions() -> Result<()> {
        expect!("sin(30)", 30.0f64.to_radians().sin());
        expect!("sin(15 * 2)", 30.0f64.to_radians().sin());
        Ok(())
    }

    #[test]
    fn units() -> Result<()> {
        let res = eval!("3 + 3m")?;
        assert_eq!(res.unit.unwrap().to_string(), "m");
        assert_eq!(res.result, 6.0);
        Ok(())
    }

    #[test]
    fn print_full_unit() -> Result<()> {
        let res = eval!("1min")?;
        assert!(res.is_long_unit);
        assert_eq!(format_unit(&res.unit.unwrap(), false), " Minute");
        let res = eval!("3m")?;
        assert_eq!(format_unit(&res.unit.unwrap(), true), " Meters");
        let res = eval!("3km")?;
        assert_eq!(format_unit(&res.unit.unwrap(), true), " Kilometers");
        let res = eval!("3km/h")?;
        assert_eq!(format_unit(&res.unit.unwrap(), true), " Kilometers per Hour");
        Ok(())
    }

    #[test]
    fn unit_conversions() -> Result<()> {
        expect!("3min in h", 0.05);
        expect!("3m in km", 0.003);
        Ok(())
    }

    #[test]
    fn convert_complex_units() -> Result<()> {
        expect!("60km/h in km/min", 1.0);
        expect!("60km/h in m/h", 60_000.0);
        Ok(())
    }


    #[test]
    fn divide_by_zero() -> Result<()> {
        expect_error!("3 / 0", DivideByZero);
        Ok(())
    }
}
