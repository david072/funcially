/*
 * Copyright (c) 2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#![allow(dead_code)]

use std::alloc::{alloc, dealloc, Layout};
use std::cell::RefCell;
use std::ffi::{CStr, CString};
use std::mem;
use std::ops::Range;
use std::os::raw::c_char;
use std::rc::Rc;
use std::sync::Arc;

use funcially_core::{
    Calculator, CalculatorResult, ContextData, Currencies, Environment, Result as CalcResult,
    ResultData, Settings, Verbosity,
};

struct AllocatableContextData {
    env: Environment,
    settings: Settings,
}

struct AllocatableCalculator {
    context: AllocatableContextData,
    verbosity: Verbosity,
}

impl AllocatableCalculator {
    pub fn new() -> Self {
        Self {
            context: AllocatableContextData {
                env: Environment::new(),
                settings: Settings::default(),
            },
            verbosity: Verbosity::None,
        }
    }

    pub fn from_calculator(calc: &Calculator) -> Self {
        Self {
            context: AllocatableContextData {
                env: calc.clone_env(),
                settings: (*calc.context().borrow()).clone().settings,
            },
            verbosity: calc.verbosity,
        }
    }

    pub fn to_calculator(&self) -> Calculator {
        Calculator {
            context: Rc::new(RefCell::new(ContextData {
                env: self.context.env.clone(),
                currencies: Arc::new(Currencies::new_load_only()),
                settings: self.context.settings,
            })),
            verbosity: self.verbosity,
        }
    }
}

struct CalculatorWrapper(Calculator, *mut AllocatableCalculator);

impl CalculatorWrapper {
    pub unsafe fn load(ptr: usize) -> Self {
        let ptr = ptr as *mut AllocatableCalculator;
        let alloc_calc = &mut *ptr;
        Self(alloc_calc.to_calculator(), ptr)
    }

    pub unsafe fn store(&self) {
        *self.1 = AllocatableCalculator::from_calculator(&self.0);
    }
}

impl Drop for CalculatorWrapper {
    fn drop(&mut self) {
        unsafe { self.store() };
    }
}

#[repr(C)]
pub struct FfiVec<T> {
    array: *mut T,
    len: usize,
}

impl<T> FfiVec<T> {
    fn empty() -> Self {
        Self {
            array: 0 as *mut T,
            len: 0,
        }
    }

    fn into_vec(self) -> Vec<T> {
        unsafe { Vec::from_raw_parts(self.array, self.len, self.len) }
    }
}

impl<T> From<Vec<T>> for FfiVec<T> {
    fn from(mut value: Vec<T>) -> Self {
        let res = Self {
            array: value.as_mut_ptr(),
            len: value.len(),
        };
        mem::forget(value);
        res
    }
}

#[repr(C)]
pub struct FfiCalculatorResult {
    data: FfiResultData,
    color_segments: FfiVec<common_c::ColorSegment>,
}

#[repr(C)]
pub struct FfiResultData {
    str_value: *const c_char,
    line_range_start: usize,
    line_range_end: usize,
    is_error: bool,
    error_ranges: FfiVec<common_c::SourceRange>,
}

#[no_mangle]
pub unsafe extern "C" fn create_calculator() -> usize {
    let alloc_calc = AllocatableCalculator::from_calculator(&Calculator::default());
    allocate(alloc_calc) as usize
}

#[no_mangle]
pub unsafe extern "C" fn reset_calculator(calculator: usize) {
    let mut calculator = CalculatorWrapper::load(calculator);
    calculator.0.reset();
}

#[no_mangle]
pub unsafe extern "C" fn calculate(
    calculator: usize,
    input: *const c_char,
) -> FfiVec<FfiCalculatorResult> {
    let mut calc = CalculatorWrapper::load(calculator);
    let input = CStr::from_ptr(input).to_str().unwrap();
    calc.0
        .calculate(input)
        .iter()
        .map(|res| FfiCalculatorResult {
            data: result_to_ffi(res, &calc.0.context.borrow().settings),
            color_segments: res
                .color_segments
                .iter()
                .map(common_c::ColorSegment::from_core_color_segment)
                .collect::<Vec<common_c::ColorSegment>>()
                .into(),
        })
        .collect::<Vec<FfiCalculatorResult>>()
        .into()
}

fn result_to_ffi(result: &CalculatorResult, settings: &Settings) -> FfiResultData {
    let str = calculator_result_to_string(&result.data, settings, false);
    let cstr = CString::new(str).unwrap().into_raw();
    let line_range = line_range_from_calculator_result(&result);

    FfiResultData {
        str_value: cstr,
        line_range_start: line_range.start,
        line_range_end: line_range.end,
        is_error: result.data.is_err(),
        error_ranges: if let Err(e) = &result.data {
            e.ranges
                .iter()
                .map(common_c::SourceRange::from_core_source_range)
                .collect::<Vec<_>>()
                .into()
        } else {
            FfiVec::empty()
        },
    }
}

fn calculator_result_to_string(
    data: &CalcResult<(ResultData, Range<usize>)>,
    calculator_settings: &Settings,
    use_thousands_separator: bool,
) -> String {
    match data {
        Ok((data, _)) => match data {
            ResultData::Value(number) => {
                number.format(calculator_settings, use_thousands_separator)
            }
            ResultData::Boolean(b) => (if *b { "True" } else { "False" }).to_string(),
            _ => String::new(),
        },
        Err(e) => format!("{}", e.error),
    }
}

fn line_range_from_calculator_result(res: &CalculatorResult) -> Range<usize> {
    match &res.data {
        Ok((_, range)) => range.clone(),
        Err(e) => {
            let lines = e.ranges.iter().flat_map(|r| vec![r.start_line, r.end_line]);
            let start = lines.clone().min().unwrap();
            let end = lines.max().unwrap();
            start..end
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn get_settings(calculator: usize) -> common_c::Settings {
    let calc = CalculatorWrapper::load(calculator);
    let ctx = calc.0.context.borrow();
    common_c::Settings::from_core_settings(ctx.settings)
}

#[no_mangle]
pub unsafe extern "C" fn set_settings(calculator: usize, settings: common_c::Settings) {
    let calc = CalculatorWrapper::load(calculator);
    (calc.0.context.borrow_mut()).settings = settings.to_core_settings();
}

#[no_mangle]
pub unsafe extern "C" fn free_settings(settings: common_c::Settings) {
    settings.free();
}

#[no_mangle]
pub unsafe extern "C" fn free_results(results: FfiVec<FfiCalculatorResult>) {
    drop(results.into_vec());
}

#[no_mangle]
pub unsafe extern "C" fn free_calculator(ptr: usize) {
    dealloc(ptr as *mut u8, Layout::new::<AllocatableCalculator>());
}

unsafe fn allocate<T>(val: T) -> *mut T {
    let ptr = alloc(Layout::new::<T>()) as *mut T;
    *ptr = val;
    ptr
}

mod common_c {
    use std::ffi::CString;
    use std::os::raw::c_char;
    use std::str::FromStr;

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct SourceRange {
        pub start_line: usize,
        pub start_char: usize,
        pub end_line: usize,
        pub end_char: usize,
    }

    impl SourceRange {
        pub(crate) fn from_core_source_range(r: &funcially_core::SourceRange) -> Self {
            Self {
                start_line: r.start_line,
                start_char: r.start_char,
                end_line: r.end_line,
                end_char: r.end_char,
            }
        }
    }

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct ColorSegment {
        pub range: SourceRange,
        pub color: Color,
    }

    impl ColorSegment {
        pub(crate) fn from_core_color_segment(seg: &funcially_core::ColorSegment) -> Self {
            Self {
                range: SourceRange::from_core_source_range(&seg.range),
                color: Color { color: seg.color.0 },
            }
        }
    }

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct Color {
        pub color: [u8; 4],
    }

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct Settings {
        pub date: DateSettings,
    }

    impl Settings {
        pub(crate) fn from_core_settings(settings: funcially_core::Settings) -> Self {
            Self {
                date: DateSettings::from_core_settings(settings.date),
            }
        }

        pub(crate) unsafe fn to_core_settings(&self) -> funcially_core::Settings {
            funcially_core::Settings {
                date: self.date.to_core_settings(),
            }
        }

        pub(crate) unsafe fn free(&self) {
            self.date.free();
        }
    }

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct DateSettings {
        pub format: *const c_char,
        pub delimiter: c_char,
    }

    impl DateSettings {
        pub(crate) fn from_core_settings(settings: funcially_core::DateSettings) -> Self {
            Self {
                format: CString::new(format!("{}", settings.format))
                    .unwrap()
                    .into_raw(),
                delimiter: settings.delimiter as c_char,
            }
        }

        pub(crate) unsafe fn to_core_settings(&self) -> funcially_core::DateSettings {
            funcially_core::DateSettings {
                format: funcially_core::DateFormat::from_str(
                    CString::from_raw(self.format as *mut c_char)
                        .to_str()
                        .unwrap(),
                )
                .unwrap(),
                delimiter: self.delimiter as u8 as char,
            }
        }

        pub(crate) unsafe fn free(&self) {
            drop(CString::from_raw(self.format as *mut c_char));
        }
    }
}
