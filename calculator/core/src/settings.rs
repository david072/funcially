/*
 * Copyright (c) 2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

pub enum AccessError {
    InvalidPath(&'static [&'static str]),
    Error(Box<dyn Error>),
}

impl Display for AccessError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidPath(options) => write!(f, "Invalid setting. Options: {options:?}"),
            Self::Error(e) => write!(f, "Error: {e}"),
        }
    }
}

macro_rules! settable {
    ($name:ident {
        $($field:ident: $field_ty:ty),+
    }) => {
        #[derive(Clone, Copy, serde::Serialize, serde::Deserialize)]
        pub struct $name {
            $(pub $field: $field_ty),+
        }

        impl $name {
            pub fn set(&mut self, path: &[&str], value: &str) -> Result<(), AccessError> {
                if path.is_empty() { return Err(AccessError::InvalidPath(&[$(stringify!($field)),+])); }
                match path[0] {
                    $(
                        stringify!($field) => self.$field.set(&path[1..], value),
                    )+
                    _ => return Err(AccessError::InvalidPath(&[$(stringify!($field)),+])),
                }
            }

            pub fn get(&self, path: &[&str]) -> Result<String, AccessError> {
                if path.is_empty() { return Err(AccessError::InvalidPath(&[$(stringify!($field)),+])); }
                match path[0] {
                    $(
                        stringify!($field) => self.$field.get(&path[1..]),
                    )+
                    _ => Err(AccessError::InvalidPath(&[$(stringify!($field)),+])),
                }
            }
        }
    };
    ($name:ident {
        $($field:ident: $field_ty:ty,)*
        $([end] $end_field:ident: $end_field_ty:ty,)+
    }) => {
        #[derive(Clone, Copy, serde::Serialize, serde::Deserialize)]
        pub struct $name {
            $(pub $field: $field_ty,)*
            $(pub $end_field: $end_field_ty),+
        }

        impl $name {
            pub fn set(&mut self, path: &[&str], value: &str) -> Result<(), AccessError> {
                const OPTIONS: &[&str] = &[$(stringify!($field)),* $(stringify!($end_field)),+];
                if path.is_empty() { return Err(AccessError::InvalidPath(OPTIONS)); }
                match path[0] {
                    $(
                        stringify!($field) => return self.$field.set(&path[1..], value),
                    )*
                    $(
                        stringify!($end_field) => {
                            if path.len() > 1 { return Err(AccessError::InvalidPath(OPTIONS)); }
                            match value.parse::<$end_field_ty>() {
                                Ok(v) => self.$end_field = v,
                                Err(e) => return Err(AccessError::Error(Box::new(e))),
                            }
                        }
                    )+
                    _ => return Err(AccessError::InvalidPath(OPTIONS)),
                }
                Ok(())
            }

            pub fn get(&self, path: &[&str]) -> Result<String, AccessError> {
                const OPTIONS: &[&str] = &[$(stringify!($field)),* $(stringify!($end_field)),+];
                if path.is_empty() { return Err(AccessError::InvalidPath(OPTIONS)); }
                match path[0] {
                    $(
                        stringify!($field) => self.$field.get(&path[1..]),
                    )*
                    $(
                        stringify!($end_field) => {
                            if path.len() > 1 { return Err(AccessError::InvalidPath(OPTIONS)); }
                            return Ok(self.$end_field.to_string())
                        }
                    )+
                    _ => Err(AccessError::InvalidPath(OPTIONS)),
                }
            }
        }
    };
}

#[derive(Debug)]
pub struct ParseDateFormatError(&'static [&'static str]);

impl Error for ParseDateFormatError {}

impl Display for ParseDateFormatError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid input. Options: {:?}", self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum DateFormat {
    Dmy,
    Mdy,
    Ymd,
}

impl Display for DateFormat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dmy => write!(f, "DMY"),
            Self::Mdy => write!(f, "MDY"),
            Self::Ymd => write!(f, "YMD"),
        }
    }
}

impl FromStr for DateFormat {
    type Err = ParseDateFormatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "dmy" => Ok(Self::Dmy),
            "mdy" => Ok(Self::Mdy),
            "ymd" => Ok(Self::Ymd),
            _ => Err(ParseDateFormatError(&["dmy", "mdy", "ymd"])),
        }
    }
}

impl DateFormat {
    pub const fn default() -> Self {
        Self::Dmy
    }

    pub fn year_index(&self) -> usize {
        match self {
            Self::Dmy => 2,
            Self::Mdy => 2,
            Self::Ymd => 0,
        }
    }

    pub fn month_index(&self) -> usize {
        match self {
            Self::Dmy => 1,
            Self::Mdy => 0,
            Self::Ymd => 1,
        }
    }

    pub fn day_index(&self) -> usize {
        match self {
            Self::Dmy => 0,
            Self::Mdy => 1,
            Self::Ymd => 2,
        }
    }
}

settable!(
    DateSettings {
        [end] format: DateFormat,
        [end] delimiter: char,
    }
);

impl DateSettings {
    pub const fn default() -> Self {
        Self {
            format: DateFormat::default(),
            delimiter: '.',
        }
    }
}

settable!(Settings { date: DateSettings });

impl Settings {
    pub const fn default() -> Self {
        Self {
            date: DateSettings::default(),
        }
    }
}
