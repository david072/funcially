use astgen::ast::Operator;
use match_ast_node;
use crate::astgen::ast::{AstNode};
use crate::common::*;

pub fn evaluate(mut ast: Vec<AstNode>) -> Result<f64> {
    eval_operators(&mut ast, &[Operator::Multiply, Operator::Divide])?;
    eval_operators(&mut ast, &[Operator::Plus, Operator::Minus])?;

    assert_eq!(ast.len(), 1);
    let result = match_ast_node!(AstNode::Literal(res), res, ast[0]);
    Ok(result)
}

fn eval_operators(ast: &mut Vec<AstNode>, operators: &[Operator]) -> Result<()> {
    let mut i = 0usize;
    while i < ast.len() - 1 {
        // there has got to be a better way to do this...
        let (lhs, operator, rhs) =
            if let [lhs, operator, rhs, ..] = &mut ast[i..=i + 2] {
                (lhs, operator, rhs)
            } else {
                return Err(ErrorType::InvalidAst.with(0..0));
            };

        let op = match_ast_node!(AstNode::Operator(op), op, operator);

        if operators.contains(op) {
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
