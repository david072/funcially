/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use std::ops::Range;

use eframe::egui::{Color32, Context, Event, FontId, Id, Key, Modifiers, text, TextFormat, Ui};
use eframe::egui::text::{CCursor, CCursorRange};
use eframe::egui::text_edit::TextEditState;

use calculator::ColorSegment;

#[derive(Debug, Default)]
pub struct SearchState {
    pub open: bool,
    pub should_have_focus: bool,
    pub text: String,
    pub old_text: String,
    pub match_case: bool,
    pub occurrences: Vec<Range<usize>>,
    pub selected_range: Option<usize>,
}

impl SearchState {
    pub fn update(&mut self, searched_text: &str) {
        if self.text != self.old_text {
            self.old_text = self.text.clone();
            if self.text.trim().is_empty() {
                self.selected_range = None;
            }
            return;
        }

        let text = self.text.trim();
        if text.is_empty() { return; }

        let (searched_text, text) = if self.match_case {
            (searched_text.to_string(), text.to_string())
        } else {
            (searched_text.to_lowercase(), text.to_lowercase())
        };
        self.occurrences = searched_text.match_indices(&text)
            .map(|(i, str)| i..i + str.len())
            .collect::<Vec<_>>();
    }

    pub fn set_range_in_text_edit_state(&self, ctx: &Context, id: &str) {
        if self.selected_range.is_none() { return; }

        if let Some(mut state) = TextEditState::load(ctx, Id::new(id)) {
            let Some(range) = self.occurrences.get(self.selected_range.unwrap()) else { return; };
            state.set_ccursor_range(Some(CCursorRange::two(
                CCursor::new(range.start),
                CCursor::new(range.end),
            )));
            state.store(ctx, Id::new(id));
        }
    }

    pub fn increment_selected_range(&mut self) {
        if self.occurrences.is_empty() { return; }
        self.selected_range = Some(
            self.selected_range.map(|i| (i + 1) % self.occurrences.len()).unwrap_or_default()
        );
    }

    pub fn decrement_selected_range(&mut self) {
        if self.occurrences.is_empty() { return; }
        self.selected_range = Some(
            self.selected_range.map(|i| {
                if i == 0 {
                    self.occurrences.len() - 1
                } else {
                    i - 1
                }
            }).unwrap_or_default()
        );
    }

    pub fn selected_range_if_open(&self) -> Option<Range<usize>> {
        if !self.open || self.occurrences.is_empty() { return None; }
        self.selected_range.map(|i| self.occurrences[i].clone())
    }
}

pub fn is_key_pressed(ui: &Ui, k: Key) -> bool {
    ui.input().events.iter().any(|event| {
        if let Event::Key { key, pressed, .. } = event {
            if *key == k && *pressed {
                return true;
            }
        }

        false
    })
}

pub fn is_key_pressed_fn<Checker: FnMut(&Key, bool, &Modifiers) -> bool>(ui: &Ui, mut checker: Checker) -> bool {
    ui.input().events.iter().any(|event| {
        if let Event::Key { key, pressed, modifiers, .. } = event {
            if checker(key, *pressed, modifiers) {
                return true;
            }
        }

        false
    })
}

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
