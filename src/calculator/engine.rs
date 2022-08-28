/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::mem::{take, replace};
use astgen::ast::Operator;
use ::{match_ast_node, Format};
use ::environment::{Environment, Variable};
use crate::astgen::ast::{AstNode, AstNodeData};
use crate::common::*;
use ::environment::units::format as format_unit;

pub struct CalculationResult {
    pub result: f64,
    pub unit: Option<String>,
    pub format: Format,
}

impl CalculationResult {
    pub fn new(result: f64, unit: Option<String>, format: Format) -> CalculationResult {
        CalculationResult { result, unit, format }
    }
}

pub fn evaluate(mut ast: Vec<AstNode>, env: &Environment) -> Result<CalculationResult> {
    if ast.len() == 1 && matches!(&ast[0].data, AstNodeData::Literal(_)) {
        ast[0].apply_modifiers()?;
        let result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
        let unit = take(&mut ast[0].unit)
            .map(|x| format_unit(&x, result != 1.0));
        return Ok(CalculationResult::new(result, unit, ast[0].format));
    }

    eval_functions(&mut ast, env)?;
    eval_variables(&mut ast, env)?;
    eval_groups(&mut ast, env)?;
    eval_operators(&mut ast, &[Operator::Exponentiation, Operator::BitwiseAnd, Operator::BitwiseOr])?;
    eval_operators(&mut ast, &[Operator::Multiply, Operator::Divide])?;
    eval_operators(&mut ast, &[Operator::Plus, Operator::Minus])?;
    eval_operators(&mut ast, &[Operator::Of, Operator::In])?;

    ast[0].apply_modifiers()?;
    let mut result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
    let format = ast[0].format;
    if format != Format::Decimal { result = result.trunc(); }

    Ok(CalculationResult::new(result, take(&mut ast[0].unit), format))
}

fn eval_functions(ast: &mut Vec<AstNode>, env: &Environment) -> Result<()> {
    for node in ast {
        let (func_name, arg_asts) = match node.data {
            AstNodeData::FunctionInvocation(ref name, ref args) => (name, args),
            _ => continue,
        };

        let mut args = Vec::new();
        for ast in arg_asts {
            args.push(evaluate(ast.clone(), env)?.result);
        }
        let result = match env.resolve_function(func_name, &args) {
            Ok(res) => res,
            Err(ty) => return Err(ty.with(node.range.clone())),
        };
        let new_node = AstNode::from(node, AstNodeData::Literal(result));
        let _ = replace(node, new_node);
    }
    Ok(())
}

fn eval_variables(ast: &mut Vec<AstNode>, env: &Environment) -> Result<()> {
    for node in ast {
        let var_name = match node.data {
            AstNodeData::VariableReference(ref name) => name.as_str(),
            _ => continue,
        };

        let Variable(number, unit) = match env.resolve_variable(var_name) {
            Ok(var) => var,
            Err(ty) => return Err(ty.with(node.range.clone())),
        };
        let mut new_node = AstNode::from(node, AstNodeData::Literal(*number));
        new_node.unit = unit.clone();
        let _ = replace(node, new_node);
    }

    Ok(())
}

fn eval_groups(ast: &mut Vec<AstNode>, env: &Environment) -> Result<()> {
    for node in ast {
        let group_ast = match &node.data {
            AstNodeData::Group(ast) => ast,
            _ => continue,
        };

        let group_result = evaluate(group_ast.clone(), env)?;
        // Construct Literal node with the evaluated result
        let new_node = AstNode::from(node, AstNodeData::Literal(group_result.result));
        let _ = replace(node, new_node);
    }

    Ok(())
}

fn eval_operators(ast: &mut Vec<AstNode>, operators: &[Operator]) -> Result<()> {
    let mut i = 0usize;
    while i < ast.len() - 1 {
        // there has got to be a better way to do this...
        let (lhs, operator, rhs) =
            if let [lhs, operator, rhs] = &mut ast[i..=i + 2] {
                (lhs, operator, rhs)
            } else {
                return Err(ErrorType::InvalidAst.with(0..0));
            };

        let op = match_ast_node!(AstNodeData::Operator(op), op, operator);

        if operators.contains(&op) {
            lhs.apply(operator, rhs)?;
            // remove operator and rhs
            ast.remove(i + 1);
            ast.remove(i + 1);
        } else {
            i += 2;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::common::Result;
    use ::parse;
    use ::tokenize;
    use ::ParserResult;

    macro_rules! eval {
        ($str:expr) => {
            evaluate(
                if let ParserResult::Calculation(ast) = parse(&tokenize($str)?, &Environment::new())? { ast }
                else { panic!("Expected ParserResult::Calculation"); },
                &Environment::new()
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
        assert_eq!(res.unit.unwrap(), "m");
        assert_eq!(res.result, 6.0);
        Ok(())
    }

    #[test]
    fn print_full_unit() -> Result<()> {
        let res = eval!("1min")?;
        assert_eq!(res.unit.unwrap(), " Minute");
        let res = eval!("3m")?;
        assert_eq!(res.unit.unwrap(), " Meters");
        let res = eval!("3km")?;
        assert_eq!(res.unit.unwrap(), " Kilometers");
        Ok(())
    }

    #[test]
    fn unit_conversions() -> Result<()> {
        expect!("3min in h", 0.05);
        expect!("3m in km", 0.003);
        Ok(())
    }

    #[test]
    fn divide_by_zero() -> Result<()> {
        expect_error!("3 / 0", DivideByZero);
        Ok(())
    }
}