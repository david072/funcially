/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::{Display, Formatter};
use std::mem::{replace, take};
use std::ops::Range;

use crate::{astgen::ast::{AstNode, AstNodeData, Operator}, astgen::tokenizer::TokenType, common::*, Context, Currencies, environment::{Environment, units::convert as convert_units, Variable}, error, match_ast_node, Settings};
use crate::astgen::ast::BooleanOperator;
use crate::astgen::objects::CalculatorObject;
use crate::environment::units::Unit;

#[derive(PartialEq, Eq, Debug, Copy, Clone, serde::Serialize, serde::Deserialize)]
pub enum Format { Decimal, Hex, Binary, Scientific }

const DECIMAL_PLACES: i32 = 10;

impl Format {
    pub fn format(&self, n: f64, use_thousands_separator: bool) -> String {
        let mut res = match self {
            Format::Decimal => round_dp(n, DECIMAL_PLACES),
            Format::Hex => format!("{:#X}", n as i64),
            Format::Binary => format!("{:#b}", n as i64),
            Format::Scientific => Self::format_scientific(n),
        };
        if *self != Format::Scientific && use_thousands_separator && !n.is_infinite() {
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

    fn format_scientific(mut n: f64) -> String {
        let is_negative = n.is_sign_negative();
        if is_negative { n *= -1.0; }

        let n_str = round_dp(n, DECIMAL_PLACES);
        let mut n_str = n_str.trim_matches('0').chars().collect::<Vec<_>>();

        fn count_decimal_places(chars: &[char]) -> isize {
            if chars.iter().any(|c| *c == '.') {
                chars.rsplit(|c| *c == '.').next().unwrap().len() as isize
            } else {
                0
            }
        }

        let decimal_places = count_decimal_places(&n_str);

        n_str.retain(|c| *c != '.');

        // trim '0'
        if let Some(first) = n_str.iter().position(|c| *c != '0') {
            let Some(last) = n_str.iter().rposition(|c| *c != '0') else { unreachable!() };
            n_str = n_str[first..last + 1].to_vec();
        } else {
            // We have removed everything. This only happens if "n == 0".
            return "0e0".to_string();
        }

        n_str.insert(1, '.');

        let decimal_places_after_move = count_decimal_places(&n_str);
        let exponent = decimal_places_after_move - decimal_places;

        format!("{}{}e{exponent}", if is_negative { "-" } else { "" }, n_str.into_iter().collect::<String>())
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
        write!(f, "{}", format!("{self:?}").to_lowercase())
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

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct NumberValue {
    pub number: f64,
    pub(crate) unit: Option<Unit>,
    is_long_unit: bool,
    pub format: Format,
}

impl NumberValue {
    pub fn new(number: f64) -> Self {
        Self {
            number,
            unit: None,
            is_long_unit: false,
            format: Format::Decimal,
        }
    }

    pub fn is_long_unit(&self) -> bool {
        self.is_long_unit || self.number.is_infinite()
    }

    pub fn unit_string(&self) -> String {
        self.unit.as_ref()
            .map(|unit| unit.format(self.is_long_unit(), self.number != 1.0))
            .unwrap_or_default()
    }
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum Value {
    Number(NumberValue),
    Object(CalculatorObject),
}

impl Value {
    pub fn number(number: f64, unit: Option<Unit>, is_long_unit: bool, format: Format) -> Value {
        Value::Number(NumberValue { number, unit, is_long_unit, format })
    }

    pub const fn only_number(number: f64) -> Value {
        Value::Number(NumberValue {
            number,
            unit: None,
            is_long_unit: false,
            format: Format::Decimal,
        })
    }

    pub fn format(&self, settings: &Settings, use_thousands_separator: bool) -> String {
        match self {
            Value::Number(number) => {
                let mut result = number.format.format(number.number, use_thousands_separator);
                if !matches!(number.unit, Some(Unit::Unit(..))) || number.is_long_unit() { result.push(' '); }
                result + &number.unit_string()
            }
            Value::Object(object) => object.to_string(settings),
        }
    }

    pub fn to_ast_node_from(&self, src: &AstNode) -> AstNode {
        match self {
            Value::Number(NumberValue { number, unit, .. }) => {
                let mut new_node = AstNode::from(src, AstNodeData::Literal(*number));
                if new_node.unit.is_none() { new_node.unit = unit.clone(); }
                new_node
            }
            Value::Object(object) => AstNode::from(src, AstNodeData::Object(object.clone())),
        }
    }

    pub fn to_number(&self) -> Option<&NumberValue> {
        match self {
            Value::Number(result) => Some(result),
            _ => None,
        }
    }

    pub fn to_object(&self) -> Option<&CalculatorObject> {
        match self {
            Value::Object(result) => Some(result),
            _ => None,
        }
    }
}

pub struct Engine<'a> {
    ast: &'a mut Vec<AstNode>,
    context: Context<'a>,
}

impl<'a> Engine<'a> {
    pub(crate) fn evaluate_to_number(ast: Vec<AstNode>, context: Context) -> Result<NumberValue> {
        let _full_range = full_range(&ast);
        let result = Engine::evaluate(ast, context)?;
        let Some(result) = result.to_number() else {
            return Err(ErrorType::ExpectedNumber.with(_full_range));
        };
        Ok(result.clone())
    }

    pub(crate) fn evaluate(mut ast: Vec<AstNode>, context: Context) -> Result<Value> {
        if ast.len() == 1 {
            if matches!(ast[0].data, AstNodeData::Literal(_)) {
                ast[0].apply_modifiers()?;
                let result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
                let unit = take(&mut ast[0].unit);
                return Ok(Value::number(result, unit, true, ast[0].format));
            } else if let AstNodeData::Object(object) = &ast[0].data {
                return Ok(Value::Object(object.clone()));
            }
        }

        let mut engine = Engine::new(&mut ast, context);
        engine.eval_variables()?;
        engine.eval_functions()?;
        engine.eval_groups()?;
        // extended operators
        engine.eval_operators(&[
            Operator::Exponentiation, Operator::BitwiseAnd, Operator::BitwiseOr, Operator::Xor,
            Operator::BitShiftLeft, Operator::BitShiftRight, Operator::Modulo
        ])?;
        engine.eval_operators(&[Operator::Multiply, Operator::Divide])?;
        engine.eval_operators(&[Operator::Plus, Operator::Minus])?;
        engine.eval_operators(&[Operator::Of, Operator::In])?;

        if matches!(ast[0].data, AstNodeData::Literal(_)) {
            ast[0].apply_modifiers()?;
            let mut result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
            let format = ast[0].format;
            if format != Format::Decimal && format != Format::Scientific { result = result.trunc(); }

            Ok(Value::number(result, take(&mut ast[0].unit), false, format))
        } else if let AstNodeData::Object(object) = &ast[0].data {
            Ok(Value::Object(object.clone()))
        } else {
            // We should never get here!
            Err(ErrorType::InvalidAst.with(ast[0].range.clone()))
        }
    }

    /// Solves a linear equation
    pub(crate) fn solve(
        lhs: Vec<AstNode>,
        rhs: Vec<AstNode>,
        is_question_mark_in_lhs: bool,
        context: Context,
    ) -> Result<Value> {
        if lhs.is_empty() || rhs.is_empty() {
            return Err(ErrorType::InvalidAst.with(0..1));
        }

        let rhs_range = full_range(&rhs);
        let (unknown_side, result_side) = if is_question_mark_in_lhs {
            (lhs, rhs)
        } else {
            (rhs, lhs)
        };
        let target_result = Self::evaluate_to_number(result_side, context)?;
        let mut target_value = target_result.number;

        if unknown_side.len() == 1 && matches!(unknown_side[0].data, AstNodeData::Literal(_)) {
            return Ok(Value::Number(target_result));
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
            let mut i = 0usize;
            while i < ast.len() {
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
                    AstNodeData::Group(ast) => {
                        if let Some(unit) = validate_and_get_unit(ast, env, is_surrounded_by_exponentiation, None)? {
                            return Ok(Some(unit));
                        }
                    }
                    AstNodeData::Identifier(name)
                    if ast.len() > i + 2 &&
                        ast[i + 1].data == AstNodeData::Operator(Operator::Call) &&
                        matches!(ast[i + 2].data, AstNodeData::Arguments(_)) => 'blk: {
                        let AstNodeData::Arguments(args) = &ast[i + 2].data else { break 'blk; };
                        i += 2;

                        let f = env.get_function(name).unwrap();

                        let mut question_mark_arg_name: Option<&str> = None;
                        let mut question_mark_range: Option<Range<usize>> = None;
                        let mut question_mark_unit: Option<Unit> = None;

                        for (i, arg) in args.iter().enumerate() {
                            if validate_and_get_unit(arg, env, is_surrounded_by_exponentiation, None)?.is_some() {
                                let range = full_range(arg);
                                if question_mark_arg_name.is_some() {
                                    return Err(ErrorType::UnexpectedQuestionMark.with(range));
                                }

                                question_mark_arg_name = Some(&f.0[i].0);
                                question_mark_unit = f.0[i].1.clone();
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
                                Ok(val) => if let Some(unit) = val.or(Some(question_mark_unit)) {
                                    return Ok(Some(unit));
                                }
                                Err(mut e) => {
                                    // TODO: Maybe somehow show the corresponding variable
                                    //  reference in the function source in addition to this.
                                    let range = question_mark_range.unwrap();
                                    e.ranges.first_mut().unwrap().start = range.start;
                                    e.ranges.first_mut().unwrap().end = range.end;
                                    return Err(e);
                                }
                            }
                        } else {
                            return Ok(None);
                        }
                    }
                    AstNodeData::Identifier(name) => {
                        if question_mark_variable_name.is_none() ||
                            question_mark_variable_name.unwrap() != name { continue; }

                        if is_surrounded_by_exponentiation {
                            // TODO: Better error range (power sign)
                            return Err(ErrorType::ForbiddenExponentiation.with(ast[i].range.clone()));
                        }

                        let unit = ast[i].unit.clone();
                        return Ok(Some(unit));
                    }
                    _ => {}
                }

                i += 1;
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
                        AstNodeData::Identifier(_) => 'blk: {
                            if ast.len() <= i + 2 { break 'blk; }
                            if ast[i + 1].data != AstNodeData::Operator(Operator::Call) { break 'blk; }

                            let AstNodeData::Arguments(args) = &mut ast[i + 2].data else { break 'blk; };

                            for arg in args { replace(arg, value); }
                        }
                        _ => {}
                    }
                }
            }

            replace(&mut ast, value);
            ast
        }

        let question_mark_unit = match validate_and_get_unit(&unknown_side, context.env, false, None)? {
            Some(unit) => unit,
            None => return Err(ErrorType::ExpectedQuestionMark.with(full_range(&unknown_side))),
        };

        // NOTE: General equation (if ? is a variable): f(?) = a
        //       => f(?) - a = 0    => y = f(?) - a
        //       With this, the result value of ? is the x-coordinate of the point where this graph
        //       intersects the x-axis.

        // Get two points from the linear function
        const X1: f64 = 1.0;
        const X2: f64 = 2.0;

        let first_ast = replace_question_mark(unknown_side.clone(), X1);

        let y1_result = Self::evaluate_to_number(first_ast, context)?;

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
                context.currencies,
                &rhs_range,
            ) {
                Ok(n) => n,
                Err(_) => return Err(ErrorType::WrongUnit(y1_unit.to_string()).with(rhs_range)),
            };
        }

        let y1 = y1_result.number - target_value;

        let second_ast = replace_question_mark(unknown_side, X2);
        let y2 = Self::evaluate_to_number(second_ast, context)?.number - target_value;

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

        Ok(Value::number(result, question_mark_unit, false, format))
    }

    pub fn check_boolean_operator(lhs: &Value, rhs: &Value, operator: BooleanOperator, currencies: &Currencies) -> bool {
        use crate::common::math::round;

        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => {
                let lhs_number = round(lhs.number, DECIMAL_PLACES);
                let rhs_number = round(rhs.number, DECIMAL_PLACES);
                let lhs_unit = &lhs.unit;
                let rhs_unit = &rhs.unit;

                if (lhs_unit.is_some() && rhs_unit.is_none()) || (lhs_unit.is_none() && rhs_unit.is_some()) {
                    false
                } else if lhs_unit.is_some() && rhs_unit.is_some() {
                    let range: Range<usize> = 0..1; // this doesn't matter since we discard the error
                    match convert_units(
                        rhs_unit.as_ref().unwrap(),
                        lhs_unit.as_ref().unwrap(),
                        rhs_number,
                        currencies,
                        &range,
                    ) {
                        Ok(mut rhs) => {
                            rhs = round(rhs, DECIMAL_PLACES);
                            operator.check(lhs_number, rhs)
                        }
                        Err(_) => false,
                    }
                } else {
                    operator.check(lhs_number, rhs_number)
                }
            }
            (Value::Object(lhs), Value::Object(rhs)) => operator.check(lhs, rhs),
            _ => false,
        }
    }

    fn new(ast: &'a mut Vec<AstNode>, context: Context<'a>) -> Engine<'a> {
        Engine { ast, context }
    }

    fn eval_functions(&mut self) -> Result<()> {
        let mut i = 0usize;

        while i < self.ast.len() - 1 {
            let Some([receiver, call_op, args_node]) = self.ast.get_mut(i..=i + 2) else {
                i += 2;
                continue;
            };

            if call_op.data != AstNodeData::Operator(Operator::Call) {
                i += 2;
                continue;
            }
            let AstNodeData::Arguments(arg_asts) = &args_node.data else {
                i += 2;
                continue;
            };

            let new_node: AstNode;
            if let AstNodeData::Identifier(func_name) = &receiver.data {
                // TODO: Make this generic!?
                let mut first_arg: Option<NumberValue> = None;
                if func_name == "abs" && arg_asts.len() == 1 {
                    match Self::evaluate(arg_asts[0].clone(), self.context)? {
                        Value::Number(number) => first_arg = Some(number),
                        Value::Object(CalculatorObject::Vector(vector)) => {
                            let result = vector.length();
                            let new_node = AstNode::from(receiver, AstNodeData::Literal(result));
                            let _ = replace(receiver, new_node);
                            self.ast.remove(i + 1);
                            self.ast.remove(i + 1);
                            continue;
                        }
                        _ => {}
                    }
                }

                let mut args = if let Some(arg) = first_arg { vec![arg] } else { vec![] };
                for ast in arg_asts {
                    args.push(Self::evaluate_to_number(ast.clone(), self.context)?);
                }

                new_node = match self.context.env.resolve_function(func_name, &args) {
                    Ok(res) => {
                        let mut new_node = AstNode::from(receiver, AstNodeData::Literal(res.0));
                        if new_node.unit.is_none() { new_node.unit = res.1; }
                        new_node
                    }
                    Err(ty) => match ty {
                        ErrorType::UnknownFunction(_) => {
                            let args = args.into_iter()
                                .zip(arg_asts.iter().map(|ast| full_range(ast)))
                                .collect::<Vec<_>>();
                            let res = self.context.env.resolve_custom_function(
                                func_name,
                                &args,
                                receiver.range.clone(),
                                self.context,
                            )?;
                            res.to_ast_node_from(receiver)
                        }
                        _ => {
                            return Err(ty.with(receiver.range.clone()));
                        }
                    }
                };
            } else if let AstNodeData::Object(object) = &receiver.data {
                if !object.is_callable() { error!(NotCallable: receiver.range.clone()); }
                let mut args = vec![];
                for ast in arg_asts {
                    args.push((Self::evaluate_to_number(ast.clone(), self.context)?, full_range(ast)));
                }

                new_node = object.call(
                    receiver.range.clone(),
                    &args,
                    args_node.range.clone(),
                )?;
            } else {
                i += 2;
                continue;
            }

            let _ = replace(receiver, new_node);
            self.ast.remove(i + 1);
            self.ast.remove(i + 1);
        }

        Ok(())
    }

    fn eval_variables(&mut self) -> Result<()> {
        for node in self.ast.iter_mut() {
            let var_name = match node.data {
                AstNodeData::Identifier(ref name) => name.as_str(),
                _ => continue,
            };

            let Variable(value) = self.context.env.resolve_variable(var_name)
                .map_err(|ty| ty.with(node.range.clone()))?;

            let new_node = value.to_ast_node_from(node);
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

            let group_result = Self::evaluate(group_ast.clone(), self.context)?;
            // Construct Literal node with the evaluated result
            let new_node = group_result.to_ast_node_from(node);
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
                if let AstNodeData::Object(object) = &lhs.data {
                    let new_lhs = object.apply(lhs.range.clone(), (op, operator.range.clone()), rhs, false)?;
                    let _ = replace(lhs, new_lhs);
                } else if let AstNodeData::Object(object) = &rhs.data {
                    let new_lhs = object.apply(rhs.range.clone(), (op, operator.range.clone()), lhs, true)?;
                    let _ = replace(lhs, new_lhs);
                } else {
                    lhs.apply(operator, rhs, self.context.currencies)?;
                }

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

pub fn full_range(ast: &[AstNode]) -> Range<usize> {
    ast.first().unwrap().range.start..ast.last().unwrap().range.end
}

#[cfg(test)]
mod tests {
    use chrono::NaiveDate;

    use crate::{Parser, ParserResult, tokenize};
    use crate::astgen::objects::DateObject;
    use crate::common::Result;

    use super::*;

    macro_rules! eval {
        ($str:expr) => {
            {
                static ENV: Environment = Environment::new();
                static CURR: Currencies = Currencies::none();
                static SET: Settings = Settings::default();
                static CONTEXT: Context = Context {
                    env: &ENV,
                    currencies: &CURR,
                    settings: &SET,
                };
                Engine::evaluate(
                    if let ParserResult::Calculation(ast) = Parser::parse(&tokenize($str)?, CONTEXT)? { ast }
                    else { panic!("Expected ParserResult::Calculation"); },
                    CONTEXT,
                ).and_then(|res| res.to_number().cloned().map(|v| Ok(v)).unwrap_or(Err(ErrorType::ExpectedNumber.with(0..1))))
            }
        }
    }

    macro_rules! eval_obj {
        ($str:expr) => {
            {
                static ENV: Environment = Environment::new();
                static CURR: Currencies = Currencies::none();
                static SET: Settings = Settings::default();
                static CONTEXT: Context = Context {
                    env: &ENV,
                    currencies: &CURR,
                    settings: &SET,
                };
                Engine::evaluate(
                    if let ParserResult::Calculation(ast) = Parser::parse(&tokenize($str)?, CONTEXT)? { ast }
                    else { panic!("Expected ParserResult::Calculation"); },
                    CONTEXT,
                ).and_then(|res| res.to_object().cloned().map(|v| Ok(v)).unwrap_or(Err(ErrorType::ExpectedNumber.with(0..1))))
            }
        }
    }

    macro_rules! expect {
        ($str:expr, $res:expr) => {
            assert_eq!(eval!($str)?.number, $res)
        }
    }

    macro_rules! expect_obj {
        ($str:expr, $res:expr) => {
            assert_eq!(eval_obj!($str)?, $res)
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
        assert_eq!(res.number, 6.0);
        Ok(())
    }

    #[test]
    fn print_full_unit() -> Result<()> {
        let res = eval!("1min")?;
        assert!(res.is_long_unit);
        assert_eq!(res.unit.unwrap().format(true, false), "Minute");
        let res = eval!("3m")?;
        assert_eq!(res.unit.unwrap().format(true, true), "Meters");
        let res = eval!("3km")?;
        assert_eq!(res.unit.unwrap().format(true, true), "Kilometers");
        let res = eval!("3km/h")?;
        assert_eq!(res.unit.unwrap().format(true, true), "Kilometers per Hour");
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

    #[test]
    fn date_object() -> Result<()> {
        expect_obj!("{date 01.01.2023}", CalculatorObject::Date(DateObject { date: NaiveDate::from_ymd_opt(2023, 1, 1).unwrap() }));
        expect_obj!("{date 01.01.2023} + 3d", CalculatorObject::Date(DateObject { date: NaiveDate::from_ymd_opt(2023, 1, 4).unwrap() }));
        expect!("{date 05.01.2023} - {date 01.01.2023}", 4.0);
        Ok(())
    }
}
