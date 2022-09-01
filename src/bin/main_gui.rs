/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

extern crate eframe;
extern crate clipboard;
extern crate calculator;

use eframe::{egui, Frame, Theme};
use egui::*;
use calculator::{
    calculate, Environment, CalculatorResultData, Format, round_dp, Verbosity, ColorSegment,
};
use std::ops::Range;
use std::sync::Arc;
use clipboard::{ClipboardProvider, ClipboardContext};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const FONT_SIZE: f32 = 16.0;
const FONT_ID: FontId = FontId::monospace(FONT_SIZE);
const TEXT_EDIT_MARGIN: Vec2 = Vec2::new(4.0, 2.0);
const ERROR_COLOR: Color32 = Color32::RED;

fn main() {
    let options = eframe::NativeOptions {
        initial_window_size: Some(Vec2::new(500.0, 400.0)),
        default_theme: Theme::Dark,
        ..Default::default()
    };
    eframe::run_native(
        "Calculator",
        options,
        Box::new(|_cc| Box::new(App::default())),
    );
}

#[derive(Debug)]
enum Line {
    Empty,
    Line {
        output_text: String,
        color_segments: Vec<ColorSegment>,
        function: Option<(String, usize)>,
        is_error: bool,
        show_in_plot: bool,
    },
}

struct App {
    environment: Environment,

    source_old: String,
    source: String,
    lines: Vec<Line>,

    first_frame: bool,
    is_plot_open: bool,
    default_bottom_text: String,
    bottom_text: Option<String>,
}

impl Default for App {
    fn default() -> App {
        App {
            environment: Environment::new(),
            source_old: String::new(),
            source: String::new(),
            lines: Vec::new(),
            first_frame: true,
            is_plot_open: false,
            default_bottom_text: format!("v{}", VERSION),
            bottom_text: None,
        }
    }
}

impl App {
    fn calculate(&mut self, str: &str) -> Line {
        let str = str.to_string();
        let str = str.trim();
        if str.is_empty() { return Line::Empty; }

        let result = calculate(str, &mut self.environment, Verbosity::None);

        let mut function: Option<(String, usize)> = None;
        let mut color_segments: Vec<ColorSegment> = Vec::new();
        let mut is_error: bool = false;

        let output_text = match result {
            Ok(res) => {
                color_segments = res.color_segments;
                match res.data {
                    CalculatorResultData::Number { result, unit, format } => {
                        let unit = unit.unwrap_or_default();
                        match format {
                            Format::Decimal => format!("{}{}", round_dp(result, 10), unit),
                            Format::Binary => format!("{:#b}{}", result as i64, unit),
                            Format::Hex => format!("{:#X}{}", result as i64, unit),
                        }
                    }
                    CalculatorResultData::Boolean(b) => (if b { "True" } else { "False" }).to_string(),
                    CalculatorResultData::Function(name, arg_count) => {
                        function = Some((name, arg_count));
                        String::new()
                    }
                    CalculatorResultData::Nothing => String::new(),
                }
            }
            Err(e) => {
                is_error = true;
                color_segments.push(ColorSegment::new(e.start..e.end, ERROR_COLOR));
                format!("{}", e.error)
            }
        };

        Line::Line {
            output_text,
            function,
            color_segments,
            is_error,
            show_in_plot: false,
        }
    }

    fn update_lines(&mut self, galley: Arc<Galley>) {
        if self.source == self.source_old { return; }

        self.source_old = self.source.clone();
        // Since we re-calculate everything from the beginning,
        // we need to start with a fresh environment
        self.environment.clear();

        let functions = self.lines.iter()
            .filter(|l| {
                match l {
                    Line::Line { show_in_plot, .. } => *show_in_plot,
                    _ => false,
                }
            })
            .map(|l| {
                if let Line::Line { function: Some((name, _)), .. } = l {
                    name.clone()
                } else { unreachable!() }
            })
            .collect::<Vec<_>>();
        self.lines.clear();

        if galley.rows.is_empty() { return; }

        let mut line = String::new();
        for (i, row) in galley.rows.iter().enumerate() {
            line += row.glyphs.iter().map(|g| g.chr).collect::<String>().as_str();

            if !row.ends_with_newline {
                if i != galley.rows.len() - 1 {
                    self.lines.push(Line::Empty);
                }
                continue;
            } else {
                if !line.trim().starts_with('#') {
                    let actual_line = if let Some(index) = line.find('#') {
                        &line[0..index]
                    } else { &line };

                    let mut res = self.calculate(actual_line);
                    if let Line::Line { function: Some((name, _)), show_in_plot, .. } = &mut res {
                        if functions.contains(name) {
                            *show_in_plot = true;
                        }
                    }
                    self.lines.push(res);
                } else {
                    self.lines.push(Line::Empty);
                }

                line.clear();
            }
        }

        if !line.is_empty() && !line.trim().starts_with('#') {
            let actual_line = if let Some(index) = line.find('#') {
                &line[0..index]
            } else { &line };

            let mut res = self.calculate(actual_line);
            if let Line::Line { function: Some((name, _)), show_in_plot, .. } = &mut res {
                if functions.contains(name) {
                    *show_in_plot = true;
                }
            }
            self.lines.push(res);
        }
    }

    fn plot_panel(&mut self, ctx: &Context) {
        SidePanel::right("plot_panel").resizable(true).show(ctx, |ui| {
            plot::Plot::new("calculator_plot")
                .data_aspect(1.0)
                .coordinates_formatter(
                    plot::Corner::LeftBottom, plot::CoordinatesFormatter::default(),
                )
                .show(ui, |plot_ui| {
                    for line in &self.lines {
                        if let Line::Line { function, show_in_plot, .. } = line {
                            if !show_in_plot { continue; }
                            if let Some(function) = function {
                                if function.1 != 1 { continue; }

                                let env = self.environment.clone();
                                let name = function.0.clone();

                                plot_ui.line(plot::Line::new(
                                    plot::PlotPoints::from_explicit_callback(move |x| {
                                        let res = env.resolve_custom_function(&name, &[x]).unwrap();
                                        res.0
                                    }, .., 512)
                                ).name(&function.0));
                            }
                        }
                    }
                });
        });
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &Context, _frame: &mut Frame) {
        self.bottom_text = None;

        TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            menu::bar(ui, |ui| {
                if ui.button(if self.is_plot_open { "Close Plot" } else { "Open Plot" }).clicked() {
                    self.is_plot_open = !self.is_plot_open;
                }
            })
        });

        if self.is_plot_open { self.plot_panel(ctx); }

        CentralPanel::default().show(ctx, |ui| {
            let input_width = ui.available_width() * (2.0 / 3.0);
            let rows = ((ui.available_height() - TEXT_EDIT_MARGIN.y) / FONT_SIZE) as usize;

            ScrollArea::vertical().show(ui, |ui| {
                ui.horizontal(|ui| {
                    let lines = &mut self.lines;
                    let output = TextEdit::multiline(&mut self.source)
                        .lock_focus(true)
                        .hint_text("Calculate something")
                        .frame(false)
                        .desired_width(input_width)
                        .font(FontSelection::from(FONT_ID))
                        .desired_rows(rows)
                        .layouter(&mut input_layouter(lines))
                        .show(ui);

                    if self.first_frame {
                        self.first_frame = false;
                        ui.ctx().memory().request_focus(output.response.id);
                    }

                    self.update_lines(output.galley);

                    vertical_spacer(ui);

                    ui.vertical(|ui| {
                        ui.add_space(2.0);
                        let prev_item_spacing = ui.spacing().item_spacing;
                        ui.spacing_mut().item_spacing.y = 0.0;

                        for line in &mut self.lines {
                            if let Line::Line {
                                output_text: text,
                                function,
                                is_error,
                                show_in_plot,
                                ..
                            } = line {
                                if !*is_error {
                                    if let Some((_, arg_count)) = function {
                                        if *arg_count == 1 {
                                            ui.with_layout(Layout::right_to_left(Align::TOP), |ui| {
                                                ui.checkbox(show_in_plot, "Show in Plot");
                                                ui.add_space(-2.0);
                                            });
                                            continue;
                                        }
                                    }
                                }

                                output_text(ui, FONT_ID, text, &mut self.bottom_text);
                            } else {
                                ui.add_space(FONT_SIZE);
                            }
                        }

                        ui.spacing_mut().item_spacing = prev_item_spacing;
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

    let font_width = (&*ui.fonts()).glyph_width(&font_id, '0');
    let mut text_job = text.into_text_job(
        ui.style(), FontSelection::FontId(font_id), valign,
    );
    text_job.job.wrap.max_width = f32::INFINITY;
    text_job.job.halign = Align::RIGHT;
    let galley = text_job.into_galley(&*ui.fonts());

    let width = f32::max(ui.available_width(), galley.size().x);
    let height = galley.size().y;

    // rect spanning the entire available width
    let (length_rect, length_rect_response) = ui.allocate_exact_size(
        vec2(ui.available_width(), 0.0), Sense::hover(),
    );

    let (rect, response) = ui.allocate_exact_size(
        vec2(width, height), Sense::click(),
    );

    let max = pos2(length_rect.right_top().x, rect.right_bottom().y);
    let min = pos2(
        f32::max(max.x - galley.size().x, length_rect.left_top().x),
        max.y - galley.size().y,
    );
    let bg_rect = Rect::from_min_max(min, max).expand(2.0);

    let mut is_right = ui.ctx().data()
        .get_persisted(length_rect_response.id)
        .unwrap_or(true);
    let mut show_copied_text = false;

    if ui.is_rect_visible(rect) {
        let mut text_color = Color32::GREEN;
        if let Some(hover_pos) = response.hover_pos() {
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

        let pos = if galley.size().x <= ui.available_width() {
            rect.right_top()
        } else {
            let time = (galley.galley.text().len() as f32 - (ui.available_width() / font_width)) * 2.0;
            let how_right = ui.ctx().animate_bool_with_time(
                length_rect_response.id, is_right, time,
            );
            if how_right == 1.0 || how_right == 0.0 {
                is_right = !is_right;
            }

            pos2(
                lerp(length_rect.right_top().x - 5.0..=rect.right_top().x + 5.0, how_right),
                rect.right_top().y,
            )
        };

        ui.painter().with_clip_rect(bg_rect).galley_with_color(pos, galley.galley, text_color);

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

    ui.ctx().data().insert_persisted(length_rect_response.id, is_right);
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

fn input_layouter(lines: &[Line]) -> impl FnMut(&Ui, &str, f32) -> Arc<Galley> + '_ {
    move |ui, string, wrap_width| {
        let section = |range: Range<usize>, color: Color32| {
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

        if !lines.is_empty() {
            let mut end = 0usize;
            let mut offset = 0usize;
            'outer: for (i, line) in string.lines().enumerate() {
                if i >= lines.len() { break; }
                if let Line::Line { color_segments, .. } = &lines[i] {
                    for segment in color_segments {
                        let range_start = segment.range.start + offset;
                        let range_end = segment.range.end + offset;

                        // Handle errors in the data caused by the text being edited. It will be
                        // updated imminently after this function, we just can't crash here
                        if range_end > string.len() { break 'outer; }
                        if range_start >= range_end { break 'outer; }
                        if !string.is_char_boundary(range_start) ||
                            !string.is_char_boundary(range_end) {
                            break 'outer;
                        }
                        if end > range_start { break 'outer; }

                        if range_start != end {
                            job.sections.push(section(end..range_start, Color32::GRAY));
                        }

                        job.sections.push(section(range_start..range_end, segment.color));
                        end = range_end;
                    }
                }

                offset += line.len() + 1;
            }

            if end != string.len() {
                job.sections.push(section(end..string.len(), Color32::GRAY));
            }
        } else {
            job.sections.push(section(0..string.len(), Color32::GRAY));
        }

        job.wrap.max_width = wrap_width;
        ui.fonts().layout_job(job)
    }
}
