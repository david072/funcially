/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;
use rand::{Rng, SeedableRng, rngs::SmallRng};
use crate::astgen::tokenizer::{Token, TokenType};
use self::TokenType::*;

const IDENTIFIER_COLOR: Color = Color::from_rgb(0xAD, 0xD8, 0xE6);
const INITIAL_RNG_SEED: u64 = 420;

#[derive(Debug, Clone)]
pub struct ColorSegment {
    pub range: Range<usize>,
    pub color: Color,
}

impl ColorSegment {
    pub fn new(range: Range<usize>, color: Color) -> Self {
        Self { range, color }
    }

    pub fn all(tokens: &[Token]) -> Vec<ColorSegment> {
        let mut result = Vec::new();

        let mut last_token: Option<(TokenType, Range<usize>)> = None;
        let mut bracket_colors = Vec::<Color>::new();

        let mut rng = SmallRng::seed_from_u64(INITIAL_RNG_SEED);
        let mut nesting = 0usize;

        for token in tokens {
            match token.ty {
                OpenBracket | OpenCurlyBracket => {
                    let r = rng.gen::<u8>();
                    let g = rng.gen::<u8>();
                    let b = rng.gen::<u8>();
                    let color = Color::from_rgb(r, g, b);

                    result.push(ColorSegment::new(token.range(), color));
                    bracket_colors.push(color);
                    nesting += 1;
                }
                CloseBracket | CloseCurlyBracket => {
                    let color = bracket_colors.pop().unwrap_or(Color::WHITE);
                    result.push(ColorSegment::new(token.range(), color));

                    if nesting > 0 {
                        nesting -= 1;
                        if nesting == 0 {
                            rng = SmallRng::seed_from_u64(INITIAL_RNG_SEED);
                        }
                    }
                }
                Divide if last_token.is_some() &&
                    last_token.as_ref().unwrap().0 == Identifier &&
                    last_token.unwrap().1.end == token.range().start => {
                    result.push(
                        ColorSegment::new(token.range(), IDENTIFIER_COLOR)
                    );
                }
                _ => result.push(Self::from(token)),
            }

            last_token = Some((token.ty, token.range()));
        }

        result
    }

    fn from(token: &Token) -> Self {
        let ty = &token.ty;
        let color = if ty.is_literal() || *ty == QuestionMark {
            Color::KHAKI
        } else if ty.is_operator() {
            if *ty == EqualsSign {
                Color::WHITE
            } else {
                Color::GOLD
            }
        } else if ty.is_format() || *ty == Identifier {
            IDENTIFIER_COLOR
        } else {
            match token.ty {
                Whitespace => Color::TRANSPARENT,
                OpenBracket | CloseBracket | ExclamationMark | PercentSign |
                Comma | EqualsSign | DefinitionSign => Color::WHITE,
                _ => unreachable!(),
            }
        };

        ColorSegment {
            range: token.range(),
            color,
        }
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

    #[inline(always)]
    pub const fn from_rgb(r: u8, g: u8, b: u8) -> Color {
        Color([r, g, b, 255])
    }

    #[inline(always)]
    pub const fn from_rgba_premultiplied(r: u8, g: u8, b: u8, a: u8) -> Color {
        Color([r, g, b, a])
    }
}
