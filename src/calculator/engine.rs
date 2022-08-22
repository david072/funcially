use astgen::ast::Operator;
use match_ast_node;
use crate::astgen::ast::{AstNode, AstNodeData};
use crate::common::*;

pub fn evaluate(mut ast: Vec<AstNode>) -> Result<f64> {
    if ast.len() == 1 {
        let result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
        return Ok(result);
    }

    eval_operators(&mut ast, &[Operator::Exponentiation, Operator::BitwiseAnd, Operator::BitwiseOr])?;
    eval_operators(&mut ast, &[Operator::Multiply, Operator::Divide])?;
    eval_operators(&mut ast, &[Operator::Plus, Operator::Minus])?;

    assert_eq!(ast.len(), 1);
    let result = match_ast_node!(AstNodeData::Literal(res), res, ast[0]);
    Ok(result)
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
        assert_eq!(result, 3.0);
        Ok(())
    }

    #[test]
    fn plus_and_minus() -> Result<()> {
        let result = eval!("3 + 5 - -2");
        assert_eq!(result, 10.0);
        Ok(())
    }

    #[test]
    fn multiply_and_divide() -> Result<()> {
        let result = eval!("3 * 2 / 3");
        assert_eq!(result, 2.0);
        Ok(())
    }

    #[test]
    fn operator_order() -> Result<()> {
        let result = eval!("3 + 4 * 2");
        assert_eq!(result, 11.0);
        Ok(())
    }
}