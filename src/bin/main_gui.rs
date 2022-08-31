/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

extern crate eframe;
extern crate clipboard;
extern crate calculator;

use std::collections::HashMap;
use eframe::{egui, Frame};
use egui::*;
use calculator::{calculate, Environment, CalculatorResultData, Format, round_dp, Verbosity, Segment};
use std::ops::Range;
use std::sync::Arc;
use clipboard::{ClipboardProvider, ClipboardContext};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const FONT_SIZE: f32 = 16.0;
const FONT_ID: FontId = FontId::monospace(FONT_SIZE);
const TEXT_EDIT_MARGIN: Vec2 = Vec2::new(4.0, 2.0);

fn main() {
    let options = eframe::NativeOptions {
        resizable: false,
        initial_window_size: Some(Vec2::new(500.0, 400.0)),
        ..Default::default()
    };
    eframe::run_native(
        "Calculator",
        options,
        Box::new(|_cc| Box::new(App::default())),
    );
}

struct App {
    environment: Environment,

    source_old: String,
    source: String,

    output_lines: Vec<String>,
    color_segments: HashMap<usize, Vec<Segment>>,
    error_ranges: HashMap<usize, Range<usize>>,

    first_frame: bool,
    default_bottom_text: String,
    bottom_text: Option<String>,
}

impl Default for App {
    fn default() -> App {
        App {
            environment: Environment::new(),
            source_old: String::new(),
            source: String::new(),
            output_lines: Vec::new(),
            color_segments: HashMap::new(),
            error_ranges: HashMap::new(),
            first_frame: true,
            default_bottom_text: format!("v{}", VERSION),
            bottom_text: None,
        }
    }
}

impl App {
    fn calculate(&mut self, line: usize, str: &str) -> String {
        let str = str.to_string();
        let str = str.trim();
        if str.is_empty() { return String::new(); }

        let result = calculate(str, &mut self.environment, Verbosity::None);
        match result {
            Ok(res) => {
                self.color_segments.insert(line, res.color_segments);
                self.error_ranges.remove(&line);

                match res.data {
                    CalculatorResultData::Number { result, unit, format } => {
                        let unit = unit.unwrap_or_default();
                        match format {
                            Format::Decimal => format!("{}{}", round_dp(result, 10), unit),
                            Format::Binary => format!("= {:#b}{}", result as i64, unit),
                            Format::Hex => format!("= {:#X}{}", result as i64, unit),
                        }
                    }
                    CalculatorResultData::Boolean(b) => (if b { "True" } else { "False" }).to_string(),
                    CalculatorResultData::Nothing => String::new(),
                }
            }
            Err(e) => {
                self.color_segments.remove(&line);
                self.error_ranges.insert(line, e.start..e.end);
                format!("{}", e.error)
            }
        }
    }

    fn update_output_lines(&mut self, galley: Arc<Galley>) {
        if self.source == self.source_old { return; }

        self.color_segments.clear();
        // Since we re-calculate everything from the beginning,
        // we need to start with a fresh environment
        self.environment.clear();
        self.output_lines.clear();
        self.source_old = self.source.clone();

        if galley.rows.is_empty() { return; }

        let mut line = String::new();
        let mut line_index = 0usize;

        for (i, row) in galley.rows.iter().enumerate() {
            line += row.glyphs.iter().map(|g| g.chr).collect::<String>().as_str();
            if !row.ends_with_newline {
                if i != galley.rows.len() - 1 {
                    self.output_lines.push(String::new());
                }
                continue;
            } else {
                if !line.trim().starts_with('#') {
                    let actual_line = if let Some(index) = line.find('#') {
                        &line[0..index]
                    } else { &line };
                    let res = self.calculate(line_index, actual_line);
                    self.output_lines.push(res);
                } else {
                    self.output_lines.push(String::new());
                }

                line.clear();
                line_index += 1;
            }
        }

        if !line.is_empty() && !line.trim().starts_with('#') {
            let actual_line = if let Some(index) = line.find('#') {
                &line[0..index]
            } else { &line };
            let res = self.calculate(line_index, actual_line);
            self.output_lines.push(res);
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &Context, _frame: &mut Frame) {
        self.bottom_text = None;

        CentralPanel::default().show(ctx, |ui| {
            let rows = ((ui.available_height() - TEXT_EDIT_MARGIN.y) / FONT_SIZE) as usize;

            ScrollArea::vertical().show(ui, |ui| {
                ui.horizontal(|ui| {
                    let color_segments = &mut self.color_segments;
                    let error_ranges = &mut self.error_ranges;

                    let output = TextEdit::multiline(&mut self.source)
                        .lock_focus(true)
                        .hint_text("Calculate something")
                        .frame(false)
                        .font(FontSelection::from(FONT_ID))
                        .desired_rows(rows)
                        .layouter(&mut |ui: &Ui, string: &str, wrap_width: f32| {
                            input_layouter(ui, string, wrap_width, color_segments, error_ranges)
                        })
                        .show(ui);

                    if self.first_frame {
                        self.first_frame = false;
                        ui.ctx().memory().request_focus(output.response.id);
                    }

                    self.update_output_lines(output.galley);

                    vertical_spacer(ui);

                    ui.vertical(|ui| {
                        ui.add_space(2.0);
                        ui.spacing_mut().item_spacing.y = 0.0;

                        for line in &self.output_lines {
                            if line.is_empty() {
                                ui.add_space(FONT_SIZE);
                            } else {
                                output_text(ui, FONT_ID, line, &mut self.bottom_text);
                            }
                        }
                    });
                });
            });

            let text = if let Some(text) = &self.bottom_text { text } else { &self.default_bottom_text };
            ui.label(RichText::new(text).font(FontId::proportional(14.0)));
        });
    }
}

fn output_text(ui: &mut Ui, font_id: FontId, str: &str, bottom_text: &mut Option<String>) -> Response {
    let text: WidgetText = str.into();
    let valign = ui.layout().vertical_align();

    let mut text_job = text.into_text_job(
        ui.style(), FontSelection::FontId(font_id), valign,
    );
    text_job.job.wrap.max_width = f32::INFINITY;
    text_job.job.halign = Align::RIGHT;
    let galley = text_job.into_galley(&*ui.fonts());

    let width = if ui.available_width() > galley.size().x { ui.available_width() } else { galley.size().x };
    let height = galley.size().y;

    let (rect, response) = ui.allocate_exact_size(
        vec2(width, height), Sense::click(),
    );

    let max = rect.right_bottom();
    let min = pos2(max.x - galley.size().x, max.y - galley.size().y);
    let bg_rect = Rect::from_min_max(min, max).expand(2.0);

    let mut show_copied_text = false;

    if ui.is_rect_visible(rect) {
        let mut text_color = Color32::GREEN;
        if response.hovered() {
            let hover_pos = response.hover_pos().unwrap();
            if bg_rect.contains(hover_pos) {
                text_color = Color32::BLACK;
                ui.painter().rect(
                    bg_rect,
                    0.5 * rect.height(),
                    Color32::GREEN,
                    Stroke::none(),
                );
                *bottom_text = Some("Click to copy".into());
            }
        }

        ui.painter().galley_with_color(rect.right_top(), galley.galley, text_color);

        if response.clicked() {
            let hover_pos = response.hover_pos().unwrap();
            if bg_rect.contains(hover_pos) {
                set_clipboard_contents(str.to_owned());
                show_copied_text = true;
            }
        }
    }

    if ui.ctx()
        .animate_bool_with_time(response.id, show_copied_text, 2.0) != 0.0 {
        *bottom_text = Some("Copied!".into());
    }

    response
}

fn set_clipboard_contents(str: String) {
    let mut ctx: ClipboardContext = ClipboardProvider::new().unwrap();
    ctx.set_contents(str).unwrap();
}

fn vertical_spacer(ui: &mut Ui) -> Response {
    let width = 5f32;
    let height = ui.available_height();

    let (rect, response) = ui.allocate_exact_size(
        Vec2::new(width, height),
        Sense::hover(),
    );

    if ui.is_rect_visible(rect) {
        let visuals = ui.style().noninteractive();
        let rect = rect.expand(visuals.expansion);
        ui.painter().vline(rect.left(), rect.y_range(), visuals.bg_stroke);
    }

    response
}

fn input_layouter(
    ui: &Ui,
    string: &str,
    wrap_width: f32,
    color_segments: &HashMap<usize, Vec<Segment>>,
    error_ranges: &HashMap<usize, Range<usize>>,
) -> Arc<Galley> {
    let layout_section = |range: Range<usize>, color: Color32| {
        text::LayoutSection {
            leading_space: 0.0,
            byte_range: range,
            format: TextFormat {
                font_id: FONT_ID,
                color,
                ..Default::default()
            },
        }
    };

    let mut job = text::LayoutJob {
        text: string.into(),
        ..Default::default()
    };

    if error_ranges.is_empty() && color_segments.is_empty() {
        job.sections.push(layout_section(0..string.len(), Color32::GRAY));
    } else {
        let mut end = 0usize;
        let mut offset = 0usize;
        'outer: for (i, line) in string.lines().enumerate() {
            if let Some(range) = error_ranges.get(&i) {
                let range_start = range.start + offset;
                let range_end = range.end + offset;

                if range_end > string.len() { break; }
                if range_start >= range_end { break 'outer; }

                // There is an error in the data because the text was edited. It will be updated
                // by the code further down imminently, we just have to try to not crash here
                if !string.is_char_boundary(range_start) || !string.is_char_boundary(range_end) {
                    break 'outer;
                }

                job.sections.push(layout_section(end..range_start, Color32::GRAY));
                job.sections.push(layout_section(range_start..range_end, Color32::RED));
                end = range_end;
            } else if let Some(segments) = color_segments.get(&i) {
                for segment in segments {
                    let range_start = segment.range.start + offset;
                    let range_end = segment.range.end + offset;

                    if range_end > string.len() { break 'outer; }
                    if range_start >= range_end { break 'outer; }

                    // There is an error in the data because the text was edited. It will be updated
                    // by the code further down imminently, we just have to try to not crash here
                    if !string.is_char_boundary(range_start) || !string.is_char_boundary(range_end) {
                        break 'outer;
                    }

                    if end > range_start { break 'outer; }
                    if range_start != end {
                        job.sections.push(layout_section(end..range_start, Color32::GRAY));
                    }

                    job.sections.push(layout_section(range_start..range_end, segment.color));
                    end = range_end;
                }
            }

            offset += line.len() + 1;
        }

        if end < string.len() {
            job.sections.push(layout_section(end..string.len(), Color32::GRAY));
        }
    }

    job.wrap.max_width = wrap_width;
    ui.fonts().layout_job(job)
}