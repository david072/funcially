/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;
use eframe::egui::Color32;
use rand::{Rng, SeedableRng, rngs::SmallRng};
use crate::astgen::tokenizer::{Token, TokenType};
use self::TokenType::*;

const IDENTIFIER_COLOR: Color32 = Color32::LIGHT_BLUE;
const INITIAL_RNG_SEED: u64 = 420;

#[derive(Debug, Clone)]
pub struct ColorSegment {
    pub range: Range<usize>,
    pub color: Color32,
}

impl ColorSegment {
    pub fn new(range: Range<usize>, color: Color32) -> Self {
        Self { range, color }
    }

    pub fn all(tokens: &[Token]) -> Vec<ColorSegment> {
        let mut result = Vec::new();

        let mut last_token: Option<(TokenType, Range<usize>)> = None;
        let mut bracket_colors = Vec::<Color32>::new();

        let mut rng = SmallRng::seed_from_u64(INITIAL_RNG_SEED);
        let mut nesting = 0usize;

        for token in tokens {
            match token.ty {
                OpenBracket => {
                    let r = rng.gen::<u8>();
                    let g = rng.gen::<u8>();
                    let b = rng.gen::<u8>();
                    let color = Color32::from_rgb(r, g, b);

                    result.push(ColorSegment::new(token.range.clone(), color));
                    bracket_colors.push(color);
                    nesting += 1;
                }
                CloseBracket => {
                    let color = bracket_colors.pop().unwrap_or(Color32::WHITE);
                    result.push(ColorSegment::new(token.range.clone(), color));

                    nesting -= 1;
                    if nesting == 0 {
                        rng = SmallRng::seed_from_u64(INITIAL_RNG_SEED);
                    }
                }
                Divide if last_token.is_some() &&
                    last_token.as_ref().unwrap().0 == Identifier &&
                    last_token.unwrap().1.end == token.range.start => {
                    result.push(
                        ColorSegment::new(token.range.clone(), IDENTIFIER_COLOR)
                    );
                }
                _ => result.push(Self::from(token)),
            }

            last_token = Some((token.ty, token.range.clone()));
        }

        result
    }

    fn from(token: &Token) -> Self {
        let color = match token.ty {
            Whitespace => Color32::TRANSPARENT,
            DecimalLiteral | HexLiteral | BinaryLiteral | QuestionMark => Color32::KHAKI,
            OpenBracket | CloseBracket | ExclamationMark | PercentSign |
            Comma | EqualsSign | DefinitionSign => Color32::WHITE,
            Plus | Minus | Multiply | Divide | Exponentiation | BitwiseAnd | BitwiseOr | Of | In => Color32::GOLD,
            Decimal | Hex | Binary | Identifier => IDENTIFIER_COLOR,
        };
        ColorSegment {
            range: token.range.clone(),
            color,
        }
    }
}
