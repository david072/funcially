/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;

use eframe::egui::{Color32, FontId, text, TextFormat};

use calculator::ColorSegment;

pub fn section(range: Range<usize>, font_id: FontId, color: Color32) -> text::LayoutSection {
    text::LayoutSection {
        leading_space: 0.0,
        byte_range: range,
        format: TextFormat {
            font_id,
            color,
            ..Default::default()
        },
    }
}

pub fn layout_segments(
    font_id: FontId,
    color_segments: &[ColorSegment],
    job: &mut text::LayoutJob,
    string: &str,
    end: &mut usize,
    offset: usize,
) -> bool {
    for segment in color_segments {
        let range_start = segment.range.start + offset;
        let range_end = segment.range.end + offset;

        // Handle errors in the data caused by the text being edited. It will be
        // updated imminently after this function, we just can't crash here
        if range_end > string.len() { return false; }
        if range_start >= range_end { return false; }
        if !string.is_char_boundary(range_start) ||
            !string.is_char_boundary(range_end) {
            return false;
        }
        if *end > range_start { return false; }

        if range_start != *end {
            job.sections.push(section(*end..range_start, font_id.clone(), Color32::GRAY));
        }

        let color = Color32::from_rgba_premultiplied(
            segment.color.0[0],
            segment.color.0[1],
            segment.color.0[2],
            segment.color.0[3],
        );
        job.sections.push(section(range_start..range_end, font_id.clone(), color));
        *end = range_end;
    }

    true
}
