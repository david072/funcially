/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::{Display, Formatter};
use std::mem::{replace, take};

use crate::{
    astgen::ast::{AstNode, AstNodeData, Operator},
    astgen::tokenizer::TokenType,
    common::*,
    Currencies,
    environment::{Environment, units::{convert as convert_units, Unit}, Variable},
    match_ast_node,
};

#[derive(PartialEq, Eq, Debug, Copy, Clone, serde::Serialize, serde::Deserialize)]
pub enum Format { Decimal, Hex, Binary, Scientific }

impl Format {
    pub fn format(&self, n: f64, use_thousands_separator: bool) -> String {
        let mut res = match self {
            Format::Decimal => round_dp(n, 10),
            Format::Hex => format!("{:#X}", n as i64),
            Format::Binary => format!("{:#b}", n as i64),
            Format::Scientific => format!("{:#e}", n),
        };
        if *self != Format::Scientific && use_thousands_separator {
            if *self == Format::Decimal {
                Self::add_thousands_separator(&mut res, 3);
            } else {
                let mut temp = res[2..].to_string();
                Self::add_thousands_separator(
                    &mut temp,
                    4,
                );
                res.replace_range(2.., &temp);
            }
        }
        res
    }

    fn add_thousands_separator(str: &mut String, packet_size: usize) {
        if str.is_empty() { return; }

        let mut char_counter = 0usize;
        let str_len = str.len();
        let mut str_i = str.len() - 1;
        let decimal_point_index = str.find('.');

        let has_sign = matches!(str.chars().next().unwrap(), '+' | '-');

        for i in 0..str.len() {
            if let Some(dp_i) = decimal_point_index {
                if i < str_len - dp_i {
                    str_i = str_i.saturating_sub(1);
                    continue;
                }
            }

            char_counter += 1;
            if char_counter == packet_size && i != str_len - 1 && (!has_sign || i != str_len - 2) {
                str.insert(str_i, '_');
                char_counter = 0;
            }
            str_i = str_i.saturating_sub(1);
        }
    }
}

impl Display for Format {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

impl From<TokenType> for Format {
    fn from(ty: TokenType) -> Self {
        match ty {
            TokenType::Decimal => Format::Decimal,
            TokenType::Hex => Format::Hex,
            TokenType::Binary => Format::Binary,
            TokenType::Scientific => Format::Scientific,
            _ => panic!("Invalid token"),
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
        // extended operators
        engine.eval_operators(&[
            Operator::Exponentiation, Operator::BitwiseAnd, Operator::BitwiseOr,
            Operator::BitShiftLeft, Operator::BitShiftRight, Operator::Modulo
        ])?;
        engine.eval_operators(&[Operator::Multiply, Operator::Divide])?;
        engine.eval_operators(&[Operator::Plus, Operator::Minus])?;
        engine.eval_operators(&[Operator::Of, Operator::In])?;

        ast[0].apply_modifiers()?;
        let mut result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
        let format = ast[0].format;
        if format != Format::Decimal { result = result.trunc(); }

        Ok(CalculationResult::new(result, take(&mut ast[0].unit), false, format))
    }

    /// Solves a linear equation
    pub fn solve(
        lhs: Vec<AstNode>,
        rhs: Vec<AstNode>,
        is_question_mark_in_lhs: bool,
        env: &Environment,
        currencies: &Currencies,
    ) -> Result<CalculationResult> {
        if lhs.is_empty() || rhs.is_empty() {
            return Err(ErrorType::InvalidAst.with(0..1));
        }

        let rhs_range = rhs.first().unwrap().range.start..rhs.last().unwrap().range.end;
        let (unknown_side, result_side) = if is_question_mark_in_lhs {
            (lhs, rhs)
        } else {
            (rhs, lhs)
        };
        let target_result = Self::evaluate(result_side, env, currencies)?;
        let mut target_value = target_result.result;

        if unknown_side.len() == 1 && matches!(unknown_side[0].data, AstNodeData::Literal(_)) {
            return Ok(target_result);
        }

        /// Validates that the question mark or its enclosing groups are not surrounded by a
        /// power sign (^).
        ///
        /// **Returns:** An error if it was surrounded by a power sign or an option, indicating
        /// whether a question mark was found, and if so, its unit.
        fn validate_and_get_unit(
            ast: &[AstNode],
            env: &Environment,
            is_surrounded_by_exponentiation: bool,
            // The name of a variable that "contains" the question mark (e.g. function arg name)
            question_mark_variable_name: Option<&str>,
        ) -> Result<Option<Option<Unit>>> {
            for i in 0..ast.len() {
                fn map_fn(node: &AstNode) -> bool {
                    matches!(node.data, AstNodeData::Operator(Operator::Exponentiation))
                }

                let prev_node = ast.get(i.saturating_sub(1)).map(map_fn);
                let next_node = ast.get(i + 1).map(map_fn);

                let has_power_sign = prev_node.unwrap_or(false) || next_node.unwrap_or(false);
                let is_surrounded_by_exponentiation = has_power_sign || is_surrounded_by_exponentiation;

                match &ast[i].data {
                    AstNodeData::QuestionMark => {
                        if is_surrounded_by_exponentiation {
                            // TODO: Better error range (power sign)
                            return Err(ErrorType::ForbiddenExponentiation.with(ast[i].range.clone()));
                        }

                        let unit = ast[i].unit.clone();
                        return Ok(Some(unit));
                    }
                    AstNodeData::VariableReference(name) => {
                        if question_mark_variable_name.is_none() ||
                            question_mark_variable_name.unwrap() != name { continue; }

                        if is_surrounded_by_exponentiation {
                            // TODO: Better error range (power sign)
                            return Err(ErrorType::ForbiddenExponentiation.with(ast[i].range.clone()));
                        }

                        let unit = ast[i].unit.clone();
                        return Ok(Some(unit));
                    }
                    AstNodeData::Group(ast) => {
                        if let Some(unit) = validate_and_get_unit(ast, env, is_surrounded_by_exponentiation, None)? {
                            return Ok(Some(unit));
                        }
                    }
                    AstNodeData::FunctionInvocation(name, args) => {
                        let f = env.get_function(name).unwrap();

                        let mut question_mark_arg_name: Option<&str> = None;
                        let mut question_mark_range: Option<std::ops::Range<usize>> = None;

                        for (i, arg) in args.iter().enumerate() {
                            if validate_and_get_unit(arg, env, is_surrounded_by_exponentiation, None)?.is_some() {
                                let range = arg.first().unwrap().range.start..arg.last().unwrap().range.end;
                                if question_mark_arg_name.is_some() {
                                    return Err(ErrorType::UnexpectedQuestionMark.with(range));
                                }

                                question_mark_arg_name = Some(&f.0[i]);
                                question_mark_range = Some(range);
                            }
                        }

                        if question_mark_arg_name.is_some() {
                            match validate_and_get_unit(
                                &f.1,
                                env,
                                is_surrounded_by_exponentiation,
                                question_mark_arg_name,
                            ) {
                                Ok(val) => if let Some(unit) = val {
                                    return Ok(Some(unit));
                                }
                                Err(mut e) => {
                                    // TODO: Maybe somehow show the corresponding variable
                                    //  reference in the function source in addition to this.
                                    let range = question_mark_range.unwrap();
                                    e.start = range.start;
                                    e.end = range.end;
                                    return Err(e);
                                }
                            }
                        } else {
                            return Ok(None);
                        }
                    }
                    _ => {}
                }
            }

            Ok(None)
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
                        AstNodeData::FunctionInvocation(_, ref mut args) => {
                            for arg in args { replace(arg, value); }
                        }
                        _ => {}
                    }
                }
            }

            replace(&mut ast, value);
            ast
        }

        let question_mark_unit = match validate_and_get_unit(&unknown_side, env, false, None)? {
            Some(unit) => unit,
            None => return Err(ErrorType::ExpectedQuestionMark.with(
                unknown_side.first().unwrap().range.start..unknown_side.last().unwrap().range.end
            )),
        };

        // NOTE: General equation (if ? is a variable): f(?) = a
        //       => f(?) - a = 0    => y = f(?) - a
        //       With this, the result value of ? is the x-coordinate of the point where this graph
        //       intersects the x-axis.

        // Get two points from the linear function
        const X1: f64 = 1.0;
        const X2: f64 = 2.0;

        let first_ast = replace_question_mark(unknown_side.clone(), X1);

        let y1_result = Self::evaluate(first_ast, env, currencies)?;
        if y1_result.unit.is_some() && target_result.unit.is_none() {
            return Err(ErrorType::WrongUnit(y1_result.unit.unwrap().to_string()).with(rhs_range));
        } else if y1_result.unit.is_none() && target_result.unit.is_some() {
            return Err(ErrorType::WrongUnit("none".to_string()).with(rhs_range));
        } else if y1_result.unit.is_some() && target_result.unit.is_some() {
            let y1_unit = y1_result.unit.clone().unwrap();

            target_value = match convert_units(
                &target_result.unit.unwrap(),
                &y1_unit,
                target_value,
                currencies,
                &rhs_range,
            ) {
                Ok(n) => n,
                Err(_) => return Err(ErrorType::WrongUnit(y1_unit.to_string()).with(rhs_range)),
            };
        }

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

        Ok(CalculationResult::new(result, question_mark_unit, false, format))
    }

    pub fn equals(lhs: &CalculationResult, rhs: &CalculationResult, currencies: &Currencies) -> bool {
        let lhs_unit = &lhs.unit;
        let rhs_unit = &rhs.unit;

        if (lhs_unit.is_some() && rhs_unit.is_none()) || (lhs_unit.is_none() && rhs_unit.is_some()) {
            false
        } else if lhs_unit.is_some() && rhs_unit.is_some() {
            let range: std::ops::Range<usize> = 0..1; // this doesn't matter since we discard the error
            match convert_units(
                rhs_unit.as_ref().unwrap(),
                lhs_unit.as_ref().unwrap(),
                rhs.result,
                currencies,
                &range,
            ) {
                Ok(rhs) => lhs.result == rhs,
                Err(_) => false,
            }
        } else {
            lhs.result == rhs.result
        }
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
                args.push(Self::evaluate(ast.clone(), self.env, self.currencies)?);
            }
            let (result, unit) = match self.env.resolve_function(func_name, &args) {
                Ok(res) => (res.0, res.1),
                Err(ty) => {
                    if matches!(ty, ErrorType::UnknownFunction(_)) {
                        let args = args.iter()
                            .map(|r| r.result)
                            .collect::<Vec<_>>();
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
            new_node.unit = unit;
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
            new_node.unit = unit.clone();
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
            let mut new_node = AstNode::from(node, AstNodeData::Literal(group_result.result));
            new_node.unit = group_result.unit;
            let _ = replace(node, new_node);
        }

        Ok(())
    }

    fn eval_operators(&mut self, operators: &[Operator]) -> Result<()> {
        let mut i = 0usize;
        while i < self.ast.len() - 1 {
            let [lhs, operator, rhs] = &mut self.ast[i..=i + 2] else { unreachable!() };
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
    use crate::{parse, ParserResult, tokenize};
    use crate::common::Result;

    use super::*;

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
        expect!("sin(30°)", 30.0f64.to_radians().sin());
        expect!("sin(15 * 2)", 30.0f64.sin());
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
        assert_eq!(res.unit.unwrap().format(false), "Minute");
        let res = eval!("3m")?;
        assert_eq!(res.unit.unwrap().format(true), "Meters");
        let res = eval!("3km")?;
        assert_eq!(res.unit.unwrap().format(true), "Kilometers");
        let res = eval!("3km/h")?;
        assert_eq!(res.unit.unwrap().format(true), "Kilometers per Hour");
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
