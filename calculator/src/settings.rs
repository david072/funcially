/*
 * Copyright (c) 2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::{Display, Formatter};
use std::str::FromStr;
use std::error::Error;

pub enum SetError {
    InvalidPath(&'static [&'static str]),
    Error(Box<dyn Error>),
}

macro_rules! settable {
    ($name:ident {
        $($field:ident: $field_ty:ty,)+
    }) => {
        #[derive(Clone, serde::Serialize, serde::Deserialize)]
        pub struct $name {
            $(pub $field: $field_ty),+
        }

        impl $name {
            pub fn set(&mut self, path: &[&str], value: &str) -> Result<(), SetError> {
                if path.is_empty() { return Err(SetError::InvalidPath(&[$(stringify!($field)),+])); }
                match path[0] {
                    $(
                        stringify!($field) => self.$field.set(&path[1..], value),
                    )+
                    _ => return Err(SetError::InvalidPath(&[$(stringify!($field)),+])),
                }
            }
        }
    };
    ($name:ident {
        $($field:ident: $field_ty:ty,)*
        $([end] $end_field:ident: $end_field_ty:ty,)+
    }) => {
        #[derive(Clone, serde::Serialize, serde::Deserialize)]
        pub struct $name {
            $(pub $field: $field_ty,)*
            $(pub $end_field: $end_field_ty),+
        }

        impl $name {
            pub fn set(&mut self, path: &[&str], value: &str) -> Result<(), SetError> {
                const OPTIONS: &[&str] = &[$(stringify!($field)),* $(stringify!($end_field)),+];
                if path.is_empty() { return Err(SetError::InvalidPath(OPTIONS)); }
                match path[0] {
                    $(
                        stringify!($field) => self.$field.set(&path[1..], value),
                    )*
                    $(
                        stringify!($end_field) => {
                            if path.len() > 1 { return Err(SetError::InvalidPath(OPTIONS)); }
                            match value.parse::<$end_field_ty>() {
                                Ok(v) => self.$end_field = v,
                                Err(e) => return Err(SetError::Error(Box::new(e))),
                            }
                        }
                    )+
                    _ => return Err(SetError::InvalidPath(OPTIONS)),
                }
                Ok(())
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
            _ =>  Err(ParseDateFormatError(&["dmy", "mdy", "ymd"]))
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

settable!(
    Settings {
        date: DateSettings,
    }
);

impl Settings {
    pub const fn default() -> Self {
        Self {
            date: DateSettings::default(),
        }
    }
}
