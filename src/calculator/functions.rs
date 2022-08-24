use ::common::Result;
use common::ErrorType;

const FUNCTIONS: [(&str, usize); 2] = [("sin", 1), ("cos", 1)];

pub fn get_arguments_count(str: &str) -> Option<usize> {
    for ref f in FUNCTIONS {
        if f.0 == str { return Some(f.1); }
    }
    None
}

pub fn is_valid_function(str: &str) -> bool {
    for ref f in FUNCTIONS {
        if f.0 == str { return true; }
    }
    false
}

pub fn resolve(f: &str, arguments: &[f64], ast_node_range: &std::ops::Range<usize>) -> Result<f64> {
    match f {
        "sin" => Ok((arguments[0].to_radians()).sin()),
        "cos" => Ok((arguments[0].to_radians()).cos()),
        _ => Err(ErrorType::UnknownFunction.with(ast_node_range.clone())),
    }
}
