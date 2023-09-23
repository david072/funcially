/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use crate::astgen::tokenizer::{Token, TokenType};
use crate::common::SourceRange;

use self::TokenType::*;

const IDENTIFIER_COLOR: Color = Color::from_rgb(0xAD, 0xD8, 0xE6);

const BRACKET_COLORS: [Color; 5] = [
    Color::from_rgb(0xD2, 0x0F, 0x39),
    Color::from_rgb(0xfe, 0x64, 0x0b),
    Color::from_rgb(0xdf, 0x8e, 0x1d),
    Color::from_rgb(0x40, 0xa0, 0x2b),
    Color::from_rgb(0x20, 0x9f, 0xb5),
];

#[derive(Debug, Clone)]
pub struct ColorSegment {
    pub range: SourceRange,
    pub color: Color,
}

impl ColorSegment {
    pub fn new(range: SourceRange, color: Color) -> Self {
        Self { range, color }
    }

    pub fn all(tokens: &[Token]) -> Vec<ColorSegment> {
        let mut result = Vec::new();

        let mut bracket_colors = Vec::<Color>::new();

        let mut nesting = 0usize;

        fn bracket_color(nesting: usize) -> Color {
            let percent = (((nesting / BRACKET_COLORS.len()).saturating_sub(1) * 20) % 100) as u32;
            BRACKET_COLORS[nesting % BRACKET_COLORS.len()].lighten(percent)
        }

        for token in tokens {
            match token.ty {
                OpenBracket | OpenSquareBracket | OpenCurlyBracket => {
                    let color = bracket_color(nesting);
                    result.push(ColorSegment::new(token.range, color));
                    bracket_colors.push(color);
                    nesting += 1;
                }
                CloseBracket | CloseSquareBracket | CloseCurlyBracket => {
                    let color = bracket_colors.pop().unwrap_or(Color::WHITE);
                    result.push(ColorSegment::new(token.range, color));

                    nesting = nesting.saturating_sub(1);
                }
                _ => result.push(Self::from(token)),
            }
        }

        result
    }

    fn from(token: &Token) -> Self {
        let ty = &token.ty;
        let color = if ty.is_literal() || matches!(ty, QuestionMark | Dot) {
            Color::KHAKI
        } else if ty.is_operator() {
            Color::GOLD
        } else if ty.is_boolean_operator() {
            Color::WHITE
        } else if ty.is_format() || matches!(*ty, Identifier | ObjectArgs) {
            IDENTIFIER_COLOR
        } else if ty.is_keyword() {
            Color::PINK
        } else {
            match token.ty {
                Whitespace | Newline => Color::TRANSPARENT,
                OpenBracket
                | OpenSquareBracket
                | OpenCurlyBracket
                | CloseBracket
                | CloseSquareBracket
                | CloseCurlyBracket
                | ExclamationMark
                | PercentSign
                | Comma
                | LineContinuation
                | Colon
                | Semicolon
                | EqualsSign
                | DefinitionSign
                | PostfixDefinitionSign => Color::WHITE,
                _ => unreachable!(),
            }
        };

        ColorSegment::new(token.range, color)
    }
}

/// [egui](https://github.com/emilk/egui/blob/master/crates/epaint/src/color.rs)'s Color32
#[derive(Debug, Clone, Copy)]
pub struct Color(pub [u8; 4]);

impl Color {
    pub const TRANSPARENT: Color = Color::from_rgba_premultiplied(0, 0, 0, 0);
    pub const BLACK: Color = Color::from_rgb(0, 0, 0);
    pub const DARK_GRAY: Color = Color::from_rgb(96, 96, 96);
    pub const GRAY: Color = Color::from_rgb(160, 160, 160);
    pub const LIGHT_GRAY: Color = Color::from_rgb(220, 220, 220);
    pub const WHITE: Color = Color::from_rgb(255, 255, 255);

    pub const BROWN: Color = Color::from_rgb(165, 42, 42);
    pub const DARK_RED: Color = Color::from_rgb(0x8B, 0, 0);
    pub const RED: Color = Color::from_rgb(255, 0, 0);
    pub const LIGHT_RED: Color = Color::from_rgb(255, 128, 128);

    pub const YELLOW: Color = Color::from_rgb(255, 255, 0);
    pub const LIGHT_YELLOW: Color = Color::from_rgb(255, 255, 0xE0);
    pub const KHAKI: Color = Color::from_rgb(240, 230, 140);

    pub const DARK_GREEN: Color = Color::from_rgb(0, 0x64, 0);
    pub const GREEN: Color = Color::from_rgb(0, 255, 0);
    pub const LIGHT_GREEN: Color = Color::from_rgb(0x90, 0xEE, 0x90);

    pub const DARK_BLUE: Color = Color::from_rgb(0, 0, 0x8B);
    pub const BLUE: Color = Color::from_rgb(0, 0, 255);
    pub const LIGHT_BLUE: Color = Color::from_rgb(0xAD, 0xD8, 0xE6);

    pub const GOLD: Color = Color::from_rgb(255, 215, 0);

    pub const PINK: Color = Color::from_rgb(175, 165, 216);

    #[inline(always)]
    pub const fn from_rgb(r: u8, g: u8, b: u8) -> Color {
        Color([r, g, b, 255])
    }

    #[inline(always)]
    pub const fn from_rgba_premultiplied(r: u8, g: u8, b: u8, a: u8) -> Color {
        Color([r, g, b, a])
    }

    pub fn lighten(&self, percent: u32) -> Color {
        let f = |i: usize| (self.0[i] as u32 * (100 + percent) / 100).min(255) as u8;
        Color::from_rgba_premultiplied(f(0), f(1), f(2), self.0[3])
    }
}
