/*
 * Copyright (c) 2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::fmt::{Display, Formatter};

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

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct DateSettings {
    pub format: DateFormat,
    pub delimiter: char,
}

impl DateSettings {
    pub const fn default() -> Self {
        Self {
            format: DateFormat::default(),
            delimiter: '.',
        }
    }
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Settings {
    pub date: DateSettings,
}

impl Settings {
    pub const fn default() -> Self {
        Self {
            date: DateSettings::default(),
        }
    }
}
