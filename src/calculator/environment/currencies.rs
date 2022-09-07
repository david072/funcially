/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;
use common::Result;
use environment::default_currencies;

pub fn is_currency(str: &str) -> bool {
    default_currencies::is_currency(str)
}

pub fn convert(src_curr: &str, dst_curr: &str, n: f64, range: &Range<usize>) -> Result<f64> {
    if src_curr == dst_curr { return Ok(n); }

    default_currencies::convert(src_curr, dst_curr, n, range)
}