use astgen::ast::Operator;
use ::{match_ast_node, Format};
use Variables;
use crate::astgen::ast::{AstNode, AstNodeData};
use crate::common::*;

pub struct CalculationResult {
    pub result: f64,
    pub format: Format,
}

impl CalculationResult {
    pub fn new(result: f64, format: Format) -> CalculationResult {
        CalculationResult { result, format }
    }
}

pub fn evaluate(mut ast: Vec<AstNode>, variables: &Variables) -> Result<CalculationResult> {
    if ast.len() == 1 && matches!(&ast[0].data, AstNodeData::Literal(_)) {
        ast[0].apply_modifiers()?;
        let result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
        return Ok(CalculationResult::new(result, ast[0].format));
    }

    eval_variables(&mut ast, variables)?;
    eval_groups(&mut ast, variables)?;
    eval_operators(&mut ast, &[Operator::Exponentiation, Operator::BitwiseAnd, Operator::BitwiseOr])?;
    eval_operators(&mut ast, &[Operator::Multiply, Operator::Divide])?;
    eval_operators(&mut ast, &[Operator::Plus, Operator::Minus])?;
    eval_operators(&mut ast, &[Operator::Of])?;

    ast[0].apply_modifiers()?;
    let mut result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
    let format = ast[0].format;
    if format != Format::Decimal { result = result.trunc(); }

    Ok(CalculationResult::new(result, format))
}

fn eval_variables(ast: &mut Vec<AstNode>, variables: &Variables) -> Result<()> {
    for node in ast {
        let var_name = match node.data {
            AstNodeData::VariableReference(ref name) => name.as_str(),
            _ => continue,
        };

        let result = variables.resolve(var_name, &node.range)?;
        let new_node = AstNode::from(node, AstNodeData::Literal(result));
        let _ = std::mem::replace(node, new_node);
    }

    Ok(())
}

fn eval_groups(ast: &mut Vec<AstNode>, variables: &Variables) -> Result<()> {
    for node in ast {
        let group_ast = match &node.data {
            AstNodeData::Group(ast) => ast,
            _ => continue,
        };

        let group_result = evaluate(group_ast.clone(), variables)?;
        // Construct Literal node with the evaluated result
        let new_node = AstNode::from(node, AstNodeData::Literal(group_result.result));
        let _ = std::mem::replace(node, new_node);
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
    use crate::common::Result;
    use crate::parse;
    use crate::tokenize;

    macro_rules! eval {
        ($str:expr) => {
            evaluate(parse(&tokenize($str)?)?)?
        }
    }

    #[test]
    fn only_one() -> Result<()> {
        let result = eval!("3");
        assert_eq!(result.result, 3.0);
        Ok(())
    }

    #[test]
    fn plus_and_minus() -> Result<()> {
        let result = eval!("3 + 5 - -2");
        assert_eq!(result.result, 10.0);
        Ok(())
    }

    #[test]
    fn multiply_and_divide() -> Result<()> {
        let result = eval!("3 * 2 / 3");
        assert_eq!(result.result, 2.0);
        Ok(())
    }

    #[test]
    fn extended_operators() -> Result<()> {
        let result = eval!("3 ^ 3 | 2 & 10");
        assert_eq!(result.result, 10.0);
        Ok(())
    }

    #[test]
    fn modifiers() -> Result<()> {
        let result = eval!("4!%");
        assert_eq!(result.result, 0.24);
        let result = eval!("!5");
        assert_eq!(result.result, 2.0);
        Ok(())
    }

    #[test]
    fn operator_order() -> Result<()> {
        let result = eval!("2% of 3 ^ 2 + 3 + 4 * 2");
        assert_eq!(result.result, 0.4);
        Ok(())
    }
}