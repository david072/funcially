#![allow(dead_code, unused_imports)]

use std::alloc::{alloc, dealloc, Layout};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::mem;
use std::ops::Range;
use std::os::raw::c_char;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use funcially_core::{
    Calculator, CalculatorResult, ContextData, Currencies, Environment, Result as CalcResult,
    ResultData, Settings, Value, Variable, Verbosity,
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
pub struct FfiResultData {
    str_value: *const c_char,
    line_range_start: usize,
    line_range_end: usize,
    is_error: bool,
}

#[no_mangle]
pub unsafe extern "C" fn create_calculator() -> usize {
    let alloc_calc = AllocatableCalculator::from_calculator(&Calculator::default());
    allocate(alloc_calc) as usize
}

#[no_mangle]
pub unsafe extern "C" fn calculate(
    calculator: usize,
    input: *const c_char,
) -> FfiVec<FfiResultData> {
    let mut calc = CalculatorWrapper::load(calculator);
    let input = CStr::from_ptr(input).to_str().unwrap();
    calc.0
        .calculate(input)
        .iter()
        .map(|res| result_to_ffi(res, &calc.0.context.borrow().settings))
        .collect::<Vec<FfiResultData>>()
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
        is_error: result.data.is_ok(),
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
pub unsafe extern "C" fn free_results(results: FfiVec<FfiResultData>) {
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
