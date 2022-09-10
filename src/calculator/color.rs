/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;
use eframe::egui::Color32;
use rand::Rng;
use crate::astgen::tokenizer::{Token, TokenType};
use self::TokenType::*;

#[derive(Debug, Clone)]
pub struct ColorSegment {
    pub range: Range<usize>,
    pub color: Color32,
}

impl ColorSegment {
    pub fn new(range: Range<usize>, color: Color32) -> Self {
        Self { range, color }
    }

    pub fn convert(tokens: &[Token]) -> Vec<ColorSegment> {
        let mut result = Vec::new();

        let mut bracket_colors = Vec::<Color32>::new();

        for token in tokens {
            if token.ty == OpenBracket {
                let color = random_color();
                result.push(ColorSegment::new(token.range.clone(), color));
                bracket_colors.push(color);
                continue;
            } else if token.ty == CloseBracket {
                let color = bracket_colors.pop().unwrap_or(Color32::WHITE);
                result.push(ColorSegment::new(token.range.clone(), color));
                continue;
            }

            result.push(Self::from(token));
        }

        result
    }

    fn from(token: &Token) -> Self {
        let color = match token.ty {
            Whitespace => Color32::TRANSPARENT,
            DecimalLiteral | HexLiteral | BinaryLiteral => Color32::KHAKI,
            OpenBracket | CloseBracket | ExclamationMark | PercentSign |
            Comma | EqualsSign | DefinitionSign => Color32::WHITE,
            Plus | Minus | Multiply | Divide | Exponentiation | BitwiseAnd | BitwiseOr | Of | In => Color32::GOLD,
            Decimal | Hex | Binary | Identifier => Color32::LIGHT_BLUE,
        };
        ColorSegment {
            range: token.range.clone(),
            color,
        }
    }
}

fn random_color() -> Color32 {
    let mut rng = rand::thread_rng();
    let r = rng.gen::<u8>();
    let g = rng.gen::<u8>();
    let b = rng.gen::<u8>();
    Color32::from_rgb(r, g, b)
}