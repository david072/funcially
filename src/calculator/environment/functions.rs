/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use ::common::Result;
use common::ErrorType;

const FUNCTIONS: [(&str, usize); 14] = [
    ("sin", 1), ("asin", 1),
    ("cos", 1), ("acos", 1),
    ("tan", 1), ("atan", 1),
    ("ln", 1), ("log", 2), // log arg2 to base arg1
    ("sqrt", 1),
    ("abs", 1),
    ("floor", 1), ("ceil", 1),
    ("clamp", 3), ("map", 5), // map arg1 from range arg2..arg3 to range arg4..arg5
];

pub fn get_arguments_count(str: &str) -> Option<usize> {
    let str = str.to_lowercase();
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

pub fn resolve(f: &str, args: &[f64], ast_node_range: &std::ops::Range<usize>) -> Result<f64> {
    match f {
        "sin" => Ok(args[0].to_radians().sin()),
        "asin" => {
            if args[0] < -1.0 || args[0] > 1.0 {
                return Err(ErrorType::NotANumber.with(ast_node_range.clone()));
            }
            Ok(args[0].asin().to_degrees())
        },
        "cos" => Ok(args[0].to_radians().cos()),
        "acos" => {
            if args[0] < -1.0 || args[0] > 1.0 {
                return Err(ErrorType::NotANumber.with(ast_node_range.clone()));
            }
            Ok(args[0].acos().to_degrees())
        },
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
                return Err(ErrorType::InvalidArguments.with(ast_node_range.clone()));
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
        _ => Err(ErrorType::UnknownFunction.with(ast_node_range.clone())),
    }
}
