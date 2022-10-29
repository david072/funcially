/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

// FIXME: Figure out why copying does not work on web

#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use std::ops::Range;
use std::sync::Arc;

use eframe::{CreationContext, egui, Frame, Storage};
use egui::*;
use egui::text_edit::CursorRange;

use calculator::{Calculator, Color, colorize_text, ColorSegment, Function as CalcFn, ResultData, Verbosity};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const FONT_SIZE: f32 = 16.0;
const FONT_ID: FontId = FontId::monospace(FONT_SIZE);
const FOOTER_FONT_SIZE: f32 = 14.0;
const TEXT_EDIT_MARGIN: Vec2 = Vec2::new(4.0, 2.0);
const ERROR_COLOR: Color = Color::RED;

#[cfg(not(target_arch = "wasm32"))]
fn main() {
    let icon = if cfg!(windows) {
        if cfg!(debug_assertions) {
            image::open("./gui/assets/app_icon_256.ico").ok().map(|i| i.to_rgba8())
        } else {
            match std::env::current_exe() {
                Ok(mut path) => {
                    path.pop();
                    path = path.join("app_icon_256.ico");

                    image::open(path).ok().map(|i| i.to_rgba8())
                }
                Err(_) => None,
            }
        }
    } else { None };

    let options = eframe::NativeOptions {
        initial_window_size: Some(Vec2::new(500.0, 400.0)),
        icon_data: {
            if let Some(icon) = icon {
                let (icon_width, icon_height) = icon.dimensions();
                Some(eframe::IconData {
                    rgba: icon.into_raw(),
                    width: icon_width,
                    height: icon_height,
                })
            } else { None }
        },
        ..Default::default()
    };
    eframe::run_native(
        "Axioma",
        options,
        Box::new(|cc| Box::new(App::new(cc))),
    );
}

#[cfg(target_arch = "wasm32")]
fn main() {
    // Make sure panics are logged using `console.error`
    console_error_panic_hook::set_once();
    // Redirect tracing to console.log, ...
    tracing_wasm::set_as_global_default();

    let web_options = eframe::WebOptions::default();
    eframe::start_web(
        "the_canvas_id",
        web_options,
        Box::new(|cc| Box::new(App::new(cc))),
    ).expect("Failed to start eframe");
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct Function(String, usize, #[serde(skip)] CalcFn);

#[derive(Debug, serde::Serialize, serde::Deserialize)]
enum Line {
    Empty,
    Line {
        #[serde(skip)]
        output_text: String,
        #[serde(skip)]
        color_segments: Vec<ColorSegment>,
        /// `name`, `argument count`, `Function`.
        ///
        /// Store the function to be able to show redefinitions as well.
        function: Option<Function>,
        show_in_plot: bool,
        #[serde(skip)]
        is_error: bool,
    },
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default)]
struct App<'a> {
    #[serde(skip)]
    calculator: Calculator<'a>,

    source: String,
    #[serde(skip)]
    source_old: String,
    lines: Vec<Line>,
    line_numbers_text: String,

    is_plot_open: bool,
    is_help_open: bool,
    #[cfg(target_arch = "wasm32")]
    is_download_open: bool,
    is_settings_open: bool,

    is_debug_info_open: bool,
    debug_information: Option<String>,

    use_thousands_separator: bool,

    #[serde(skip)]
    first_frame: bool,
    #[serde(skip)]
    input_text_paragraph: usize,
    #[serde(skip)]
    bottom_text: String,
    #[serde(skip)]
    cached_help_window_color_segments: Vec<Vec<ColorSegment>>,
}

impl Default for App<'_> {
    fn default() -> Self {
        App {
            calculator: Calculator::default(),
            source_old: String::new(),
            source: String::new(),
            lines: Vec::new(),
            line_numbers_text: "1".to_string(),
            first_frame: true,
            is_plot_open: false,
            is_help_open: false,
            #[cfg(target_arch = "wasm32")]
            is_download_open: false,
            is_settings_open: false,
            is_debug_info_open: false,
            debug_information: None,
            use_thousands_separator: false,
            input_text_paragraph: 0,
            bottom_text: format!("v{}", VERSION),
            cached_help_window_color_segments: Vec::new(),
        }
    }
}

impl App<'_> {
    fn new(cc: &CreationContext<'_>) -> Self {
        cc.egui_ctx.set_visuals(Visuals::dark());

        if let Some(storage) = cc.storage {
            return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        }

        App::default()
    }

    fn calculate(&mut self, str: &str) -> Line {
        if str.trim().is_empty() { return Line::Empty; }

        let result = self.calculator.calculate(str);

        let mut function: Option<Function> = None;
        let mut color_segments: Vec<ColorSegment> = Vec::new();
        let mut is_error: bool = false;

        let output_text = match result {
            Ok(res) => {
                color_segments = res.color_segments;
                match res.data {
                    ResultData::Number { result, unit, format } => {
                        format!("{}{}", format.format(result, self.use_thousands_separator), unit.unwrap_or_default())
                    }
                    ResultData::Boolean(b) => (if b { "True" } else { "False" }).to_string(),
                    ResultData::Function { name, arg_count, function: f } => {
                        function = Some(Function(name, arg_count, f));
                        String::new()
                    }
                    ResultData::Nothing => String::new(),
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

    fn get_debug_info_for_current_line(&mut self) {
        for (i, line) in self.source.lines().enumerate() {
            if i != self.input_text_paragraph { continue; }

            self.debug_information = match self.calculator.get_debug_info(line, Verbosity::Ast) {
                Ok(info) => Some(info),
                Err(e) => Some(format!("Error generating debug information: {}, {}..{}", e.error, e.start, e.end))
            };
            break;
        }
    }

    fn update_lines(&mut self, galley: Arc<Galley>) {
        if self.source == self.source_old { return; }

        self.source_old = self.source.clone();
        // Since we re-calculate everything from the beginning,
        // we need to start with a fresh environment
        self.calculator.reset();

        let functions = self.lines.iter()
            .filter(|l| {
                match l {
                    Line::Line { show_in_plot, .. } => *show_in_plot,
                    _ => false,
                }
            })
            .map(|l| {
                if let Line::Line { function: Some(Function(name, ..)), .. } = l {
                    name.clone()
                } else { unreachable!() }
            })
            .collect::<Vec<_>>();
        self.lines.clear();
        self.line_numbers_text.clear();

        if galley.rows.is_empty() {
            self.line_numbers_text = "1".to_string();
            return;
        }

        let mut line = String::new();
        let mut line_index = 1usize;
        let mut did_add_line_index = false;
        for (i, row) in galley.rows.iter().enumerate() {
            line += row.glyphs.iter().map(|g| g.chr).collect::<String>().as_str();

            if !row.ends_with_newline {
                if !did_add_line_index {
                    self.line_numbers_text += &line_index.to_string();
                    self.line_numbers_text.push('\n');
                    did_add_line_index = true;
                    line_index += 1;
                }

                if i != galley.rows.len() - 1 {
                    self.lines.push(Line::Empty);
                }
                continue;
            } else {
                if !did_add_line_index {
                    self.line_numbers_text += &line_index.to_string();
                    line_index += 1;
                }
                self.line_numbers_text.push('\n');
                did_add_line_index = false;

                if !line.starts_with('#') {
                    let actual_line = if let Some(index) = line.find('#') {
                        &line[0..index]
                    } else { &line };

                    let mut res = self.calculate(actual_line);
                    if let Line::Line { function: Some(Function(name, ..)), show_in_plot, .. } = &mut res {
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

        if !line.is_empty() && !line.starts_with('#') {
            let actual_line = if let Some(index) = line.find('#') {
                &line[0..index]
            } else { &line };

            let mut res = self.calculate(actual_line);
            if let Line::Line { function: Some(Function(name, ..)), show_in_plot, .. } = &mut res {
                if functions.contains(name) {
                    *show_in_plot = true;
                }
            }
            self.lines.push(res);
        }

        if self.line_numbers_text.is_empty() {
            self.line_numbers_text = "1".to_string();
        }
    }

    fn plot_panel(&mut self, ctx: &Context) {
        SidePanel::right("plot_panel").resizable(true).show(ctx, |ui| {
            plot::Plot::new("calculator_plot")
                .data_aspect(1.0)
                .coordinates_formatter(
                    plot::Corner::LeftBottom, plot::CoordinatesFormatter::default(),
                )
                .legend(plot::Legend::default().position(plot::Corner::RightBottom))
                .show(ui, |plot_ui| {
                    for line in &self.lines {
                        if let Line::Line { function, show_in_plot, .. } = line {
                            if !show_in_plot { continue; }
                            if let Some(function) = function {
                                if function.1 != 1 { continue; }

                                let env = self.calculator.clone_env();
                                let f = function.2.clone();
                                let currencies = self.calculator.currencies.clone();

                                plot_ui.line(plot::Line::new(
                                    plot::PlotPoints::from_explicit_callback(move |x| {
                                        match env.resolve_specific_function(&f, &[x], &currencies) {
                                            Ok(v) => v.0,
                                            Err(_) => f64::NAN,
                                        }
                                    }, .., 512)
                                ).name(&function.0));
                            }
                        }
                    }
                });
        });
    }

    fn help_window(&mut self, ctx: &Context) {
        let is_help_open = &mut self.is_help_open;
        let color_segments = &mut self.cached_help_window_color_segments;
        Window::new("Help")
            .open(is_help_open)
            .vscroll(true)
            .hscroll(true)
            .show(ctx, |ui| {
                build_help(ui, color_segments);
            });
    }

    #[cfg(target_arch = "wasm32")]
    fn download_window(&mut self, ctx: &Context) {
        Window::new("Download")
            .open(&mut self.is_download_open)
            .collapsible(false)
            .show(ctx, |ui| {
                ui.heading("Desktop App");
                ui.horizontal(|ui| {
                    ui.spacing_mut().item_spacing.x = 0.0;
                    ui.label("If you're on desktop, you can download the ");
                    ui.hyperlink_to("desktop app", "https://github.com/david072/axioma/releases");
                    ui.label(".");
                });
                ui.separator();

                ui.heading("Web App");
                ui.label("You can make this website available offline through Chrome.");
                ui.add_space(4.0);
                ui.heading("Chrome:");
                ui.label("Desktop: Click the download button to the right of the address bar and follow the instructions.");
                ui.add_space(2.0);
                ui.label("Mobile: Click the three dots to the right of the address bar and click the 'Install app' button.");
                ui.add_space(4.0);
                ui.heading("Safari (iPad / iPhone):");
                ui.label("Click the share button, either at the bottom of the screen or next to the address bar, \
                    click the 'Add to Home Screen' button and follow the instructions.");
                ui.add_space(2.0);
            });
    }

    fn settings_window(&mut self, ctx: &Context) {
        Window::new("Settings")
            .open(&mut self.is_settings_open)
            .vscroll(true)
            .show(ctx, |ui| {
                if ui.checkbox(&mut self.use_thousands_separator, "Use thousands separator").clicked() {
                    // Make update_lines() refresh on the next frame, since now source and source_old are not the same
                    self.source_old.clear();
                }
                CollapsingHeader::new("Debug").default_open(true).show(ui, |ui| {
                    let mut debug_on_hover = ui.ctx().debug_on_hover();
                    ui.checkbox(&mut debug_on_hover, "Debug On Hover");
                    ui.ctx().set_debug_on_hover(debug_on_hover);

                    let mut tesselation_options = ui.ctx().options().tessellation_options;
                    ui.checkbox(&mut tesselation_options.debug_paint_clip_rects, "Paint clip rectangles");
                    ui.checkbox(&mut tesselation_options.debug_paint_text_rects, "Paint text bounds");
                    *ui.ctx().tessellation_options() = tesselation_options;
                });
                ui.hyperlink_to("Source code", "https://github.com/david072/axioma");
            });
    }

    fn show_debug_information(&mut self, ctx: &Context) {
        let debug_information = &mut self.debug_information;

        Window::new("Debug Information")
            .open(&mut self.is_debug_info_open)
            .vscroll(true)
            .show(ctx, |ui| {
                if let Some(debug_information) = debug_information {
                    if ui.button("📋").clicked() {
                        ui.output().copied_text = debug_information.clone();
                    }

                    TextEdit::multiline(debug_information)
                        .interactive(false)
                        .show(ui);
                }
            });
    }

    fn handle_shortcuts(&mut self, ui: &Ui, cursor_range: CursorRange) {
        let mut copied_text = String::new();
        for event in &ui.input().events {
            if let Event::Key { key, pressed, modifiers } = event {
                if !*pressed { continue; }
                match key {
                    Key::Num0 if modifiers.command && modifiers.shift => {
                        let start_line = cursor_range.primary.pcursor.paragraph;
                        let end_line = cursor_range.secondary.pcursor.paragraph;

                        let has_uncommented_line = self.source.lines()
                            .skip(start_line)
                            .take(if end_line == 0 { 1 } else { end_line })
                            .any(|l| !l.trim_start().starts_with('#'));

                        // If there is an uncommented line, we even the lines out by commenting
                        // uncommented lines too.
                        // Otherwise, we uncomment, since all lines are commented.

                        let mut new_source = String::new();
                        for (i, line) in self.source.lines().enumerate() {
                            if i < start_line || i > end_line {
                                new_source += line;
                                new_source.push('\n');
                                continue;
                            }

                            let trimmed = line.trim_start();
                            let offset = line.len() - trimmed.len();

                            if has_uncommented_line {
                                if !line.trim_start().starts_with('#') {
                                    for _ in 0..offset { new_source.push(' '); }
                                    new_source.push('#');
                                    new_source += &line[offset..];
                                    new_source.push('\n');
                                } else {
                                    new_source += line;
                                    new_source.push('\n');
                                }
                            } else {
                                for _ in 0..offset { new_source.push(' '); }
                                new_source += line.chars()
                                    .skip(offset + 1)
                                    .collect::<String>().as_str();
                                new_source.push('\n');
                            }
                        }

                        self.source = new_source;
                    }
                    Key::B if modifiers.command => {
                        // Check that we have a range spanning only one line
                        let primary = &cursor_range.primary.pcursor;
                        let secondary = &cursor_range.secondary.pcursor;

                        if (*primary == *secondary) || (primary.paragraph != secondary.paragraph) {
                            continue;
                        }

                        let mut new_source = String::new();
                        for (i, line) in self.source.lines().enumerate() {
                            if i != primary.paragraph {
                                new_source += line;
                                new_source.push('\n');
                                continue;
                            }

                            let start = std::cmp::min(primary.offset, secondary.offset);
                            let end = std::cmp::max(primary.offset, secondary.offset);

                            let mut line = line.to_string();
                            line.insert(start, '(');
                            line.insert(end + 1, ')');
                            new_source += line.as_str();
                            new_source.push('\n');
                        }

                        self.source = new_source;
                    }
                    Key::C if modifiers.command && modifiers.shift => {
                        let line = cursor_range.primary.rcursor.row;
                        if let Some(Line::Line { output_text, .. }) = self.lines.get(line) {
                            copied_text = output_text.to_owned();
                            // Taking the ui.output() lock here leads to a deadlock, so we have to
                            // do it after the loop.
                        }
                    }
                    Key::L if modifiers.command && modifiers.alt => {
                        let mut new_source = String::new();

                        let line_count = self.source.lines().count();
                        for (i, line) in self.source.lines().enumerate() {
                            if !line.is_empty() {
                                match self.calculator.format(line) {
                                    Ok(fmt) => new_source += &fmt,
                                    Err(_) => new_source += line,
                                }
                            }

                            if i != line_count - 1 {
                                new_source.push('\n');
                            }
                        }

                        self.source = new_source;
                    }
                    _ => {}
                }
            }
        }

        ui.output().copied_text = copied_text;
    }
}

impl eframe::App for App<'_> {
    fn update(&mut self, ctx: &Context, _frame: &mut Frame) {
        if !self.cached_help_window_color_segments.is_empty() && !self.is_help_open {
            self.cached_help_window_color_segments.clear();
        }
        if !self.is_debug_info_open { self.debug_information = None; }

        TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            menu::bar(ui, |ui| {
                if ui.button(if self.is_plot_open { "Close Plot" } else { "Open Plot" }).clicked() {
                    self.is_plot_open = !self.is_plot_open;
                }
                if ui.button("Help").clicked() {
                    self.is_help_open = !self.is_help_open;
                }
                #[cfg(target_arch = "wasm32")]
                if ui.button("Download").clicked() {
                    self.is_download_open = !self.is_download_open;
                }
                if ui.button("Settings").clicked() {
                    self.is_settings_open = !self.is_settings_open;
                }

                ui.menu_button("Debug", |ui| {
                    if ui.button("Print Debug Information for current line").clicked() {
                        self.get_debug_info_for_current_line();
                        self.is_debug_info_open = true;
                    }
                });
            })
        });

        // We wait for the second frame to have the lines updated if they've been loaded on startup
        if !self.first_frame && self.is_plot_open { self.plot_panel(ctx); }

        if self.is_help_open { self.help_window(ctx); }
        #[cfg(target_arch = "wasm32")]
        if self.is_download_open { self.download_window(ctx); }
        if self.is_settings_open { self.settings_window(ctx); }
        if self.is_debug_info_open { self.show_debug_information(ctx); }

        CentralPanel::default().show(ctx, |ui| {
            let rows = ((ui.available_height() - TEXT_EDIT_MARGIN.y - FOOTER_FONT_SIZE) / FONT_SIZE) as usize;

            ScrollArea::vertical().show(ui, |ui| {
                ui.with_layout(Layout::left_to_right(Align::TOP), |ui| {
                    let char_width = ui.fonts().glyph_width(&FONT_ID, '0') + 2.0;

                    let longest_row_chars = self.line_numbers_text.lines()
                        .map(str::len)
                        .max()
                        .unwrap_or_default() as f32;

                    TextEdit::multiline(&mut self.line_numbers_text)
                        .frame(false)
                        .font(FontSelection::from(FONT_ID))
                        .interactive(false)
                        .desired_width(longest_row_chars * char_width)
                        .desired_rows(rows)
                        .margin(vec2(0.0, 2.0))
                        .show(ui);

                    let input_width = ui.available_width() * (2.0 / 3.0);

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
                    if let Some(range) = output.cursor_range {
                        self.input_text_paragraph = range.primary.pcursor.paragraph;
                    }

                    if self.first_frame {
                        self.first_frame = false;
                        ui.ctx().memory().request_focus(output.response.id);
                    }

                    self.update_lines(output.galley);

                    if let Some(range) = output.cursor_range {
                        self.handle_shortcuts(ui, range);
                    }

                    vertical_spacer(ui);

                    ui.vertical(|ui| {
                        ui.add_space(2.0);
                        ui.spacing_mut().item_spacing.y = 0.0;

                        // Spacer to put scroll wheel at the right side of the window
                        ui.allocate_exact_size(
                            vec2(ui.available_width(), 0.0), Sense::hover());

                        for (i, line) in self.lines.iter_mut().enumerate() {
                            if let Line::Line {
                                output_text: text,
                                function,
                                is_error,
                                show_in_plot,
                                ..
                            } = line {
                                if !*is_error {
                                    if let Some(Function(_, arg_count, _)) = function {
                                        if *arg_count == 1 {
                                            ui.with_layout(Layout::right_to_left(Align::TOP), |ui| {
                                                ui.checkbox(show_in_plot, "Show in Plot");
                                                ui.add_space(-2.0);
                                            });
                                            continue;
                                        }
                                    }
                                }

                                output_text(ui, text, i + 1);
                            } else {
                                ui.add_space(FONT_SIZE);
                            }
                        }
                    });
                });
            });

            ui.label(RichText::new(&self.bottom_text).font(FontId::proportional(FOOTER_FONT_SIZE)));
        });
    }

    fn save(&mut self, storage: &mut dyn Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }
}

fn output_text(ui: &mut Ui, str: &str, index: usize) -> Response {
    let text: WidgetText = str.into();
    let valign = ui.layout().vertical_align();

    let mut text_job = text.into_text_job(
        ui.style(), FontSelection::FontId(FONT_ID), valign,
    );
    text_job.job.wrap.max_width = f32::INFINITY;
    text_job.job.halign = Align::RIGHT;
    let galley = text_job.into_galley(&*ui.fonts());

    let glyph_width = ui.fonts().glyph_width(&FONT_ID, '0');
    let index = index.to_string();
    let index_str_width = index.len() as f32 * glyph_width;

    let (full_rect, response) = ui.allocate_exact_size(
        vec2(ui.available_width() - index_str_width, galley.size().y), Sense::click());

    let index_rect = Rect::from_min_max(
        full_rect.left_top(),
        pos2(full_rect.left_bottom().x + index_str_width, full_rect.left_bottom().y),
    ).expand2(vec2(3.0, 0.0));

    let text_max_rect = Rect::from_min_max(
        index_rect.right_top(),
        full_rect.right_bottom(),
    );

    let bg_rect = Rect::from_min_max(
        pos2(text_max_rect.right_top().x - galley.size().x, text_max_rect.right_top().y),
        text_max_rect.right_bottom(),
    ).expand(1.5);

    if ui.is_rect_visible(full_rect) {
        if !str.is_empty() {
            ui.painter().text(
                index_rect.left_top(),
                Align2::LEFT_TOP,
                index,
                FONT_ID,
                Color32::GRAY,
            );
        }

        let mut text_color = Color32::GREEN;
        if let Some(hover_pos) = response.hover_pos() {
            if bg_rect.contains(hover_pos) {
                text_color = Color32::BLACK;
                ui.painter()
                    .with_clip_rect(Rect::from_min_max(
                        pos2(
                            f32::max(bg_rect.left_top().x, text_max_rect.left_top().x),
                            bg_rect.left_top().y,
                        ),
                        bg_rect.right_bottom(),
                    ))
                    .rect(
                        bg_rect,
                        0.5 * full_rect.height(),
                        Color32::GREEN,
                        Stroke::none(),
                    );
            }
        }

        let galley_length = galley.size().x;
        ui.painter()
            .with_clip_rect(text_max_rect)
            .galley_with_color(text_max_rect.right_top(), galley.galley, text_color);

        #[cfg(not(target_arch = "wasm32"))]
            let mut show_copied_text = false;
        #[cfg(target_arch = "wasm32")]
            let show_copied_text = false;

        #[cfg(not(target_arch = "wasm32"))]
        if response.clicked() && bg_rect.contains(response.hover_pos().unwrap()) {
            ui.output().copied_text = str.to_owned();
            show_copied_text = true;
        }

        if galley_length >= text_max_rect.width() && response.hovered() {
            show_tooltip_at(
                ui.ctx(),
                response.id.with("__out_tooltip"),
                Some(full_rect.right_bottom()),
                |ui| {
                    ui.label(str);
                    if ui.ctx().animate_bool_with_time(
                        response.id.with("__copied_text_anim"),
                        show_copied_text,
                        2.0,
                    ) != 0.0 {
                        ui.label("Copied!");
                    }
                });
        }
    }

    response
}

fn help_window_row(ui: &mut Ui, color_segments: &mut Vec<Vec<ColorSegment>>, input: &str, output: &str, color_index: usize) {
    if color_segments.len() == color_index {
        if let Some(segments) = colorize_text(input) {
            color_segments.push(segments);
        }
    }

    let color_segments = &color_segments[color_index];
    ui.horizontal(|ui| {
        let mut input = input.to_string();
        let mut output = output.to_string();

        TextEdit::singleline(&mut input)
            .interactive(false)
            .font(FontSelection::FontId(FONT_ID))
            .layouter(&mut |ui, string, wrap_width| {
                let mut job = text::LayoutJob {
                    text: string.into(),
                    ..Default::default()
                };

                let mut end = 0usize;
                layout_segments(color_segments, &mut job, string, &mut end, 0);

                job.wrap.max_width = wrap_width;
                ui.fonts().layout_job(job)
            })
            .show(ui);

        TextEdit::singleline(&mut output)
            .interactive(false)
            .font(FontSelection::FontId(FONT_ID))
            .text_color(Color32::GREEN)
            .show(ui);
    });
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

fn section(range: Range<usize>, color: Color32) -> text::LayoutSection {
    text::LayoutSection {
        leading_space: 0.0,
        byte_range: range,
        format: TextFormat {
            font_id: FONT_ID,
            color,
            ..Default::default()
        },
    }
}

fn input_layouter(lines: &[Line]) -> impl FnMut(&Ui, &str, f32) -> Arc<Galley> + '_ {
    move |ui, string, wrap_width| {
        let mut job = text::LayoutJob {
            text: string.into(),
            ..Default::default()
        };

        if !lines.is_empty() {
            let mut end = 0usize;
            let mut offset = 0usize;
            let mut i = 0usize;

            for line in string.lines() {
                if i >= lines.len() { break; }

                let trimmed_line = line.trim();
                // Skip empty lines
                if !trimmed_line.is_empty() && !trimmed_line.starts_with('#') {
                    // NOTE: We use `Line::Empty`s to add spacing if the line spans multiple rows.
                    //  We have to skip these lines here to get to the actual color segments.
                    while matches!(lines.get(i), Some(Line::Empty)) { i += 1; }

                    if let Some(Line::Line { color_segments, .. }) = &lines.get(i) {
                        if !layout_segments(color_segments, &mut job, string, &mut end, offset) {
                            break;
                        }
                    }
                }

                offset += line.len() + 1;
                i += 1;
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

fn layout_segments(
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
            job.sections.push(section(*end..range_start, Color32::GRAY));
        }

        let color = Color32::from_rgba_premultiplied(
            segment.color.0[0],
            segment.color.0[1],
            segment.color.0[2],
            segment.color.0[3],
        );
        job.sections.push(section(range_start..range_end, color));
        *end = range_end;
    }

    true
}

fn build_help(ui: &mut Ui, color_segments: &mut Vec<Vec<ColorSegment>>) {
    ui.horizontal(|ui| {
        ui.spacing_mut().item_spacing.x = 0.0;
        ui.label("This is a brief overview of the features. A more detailed documentation can be found ");
        ui.hyperlink_to("here", "https://github.com/david072/axioma/wiki");
        ui.label(".");
    });

    ui.heading("Numbers");
    ui.label("You can type in numbers in different formats:");
    help_window_row(ui, color_segments, "123", "123", 0);
    help_window_row(ui, color_segments, "10.5 + .5", "12", 1);
    help_window_row(ui, color_segments, "0xFF", "255", 2);
    help_window_row(ui, color_segments, "0b110", "6", 3);

    ui.separator();
    ui.heading("Operators");
    ui.label("Basic operators: +, -, *, /");
    help_window_row(ui, color_segments, "3 + 4 * 2", "11", 4);
    help_window_row(ui, color_segments, "8 / 2 - 2", "2", 5);

    ui.label("Extended operators: ^, &, |, <number>!, !<number>");
    help_window_row(ui, color_segments, "10 ^ 2 * 4", "400", 6);
    help_window_row(ui, color_segments, "5!", "120", 7);

    ui.label("The multiplication sign can be left out before variables, functions and groups.");

    ui.separator();
    ui.heading("Groups");
    help_window_row(ui, color_segments, "((3 + 3) / 2) * 3", "9", 8);

    ui.separator();
    ui.heading("Functions");
    ui.label("Builtin functions include 'sin', 'cos', 'log' or 'sqrt':");
    help_window_row(ui, color_segments, "sin(30)", "0.5", 9);
    help_window_row(ui, color_segments, "sqrt(20 + 5)", "5", 10);
    help_window_row(ui, color_segments, "2log(2, 8)", "6", 11);

    ui.label("You can also define your own custom functions:");
    help_window_row(ui, color_segments, "f(x) := x * 3", "", 12);
    help_window_row(ui, color_segments, "pow(a, b) := a ^ b", "", 13);

    ui.label("To remove a custom function, simply leave out the right side of ':='.");

    ui.separator();
    ui.heading("Variables");
    ui.label("Builtin variables include 'pi', 'e' and 'tau':");
    help_window_row(ui, color_segments, "pi", "3.1415926536", 14);

    ui.label("You can also define your own custom variables:");
    help_window_row(ui, color_segments, "x := 3 * 4", "", 15);

    ui.label("To remove a custom variable, simply leave out the right side of ':='.");

    ui.label("The 'ans' variable always contains the result of the last calculation.");
    help_window_row(ui, color_segments, "3 * 4", "12", 16);
    help_window_row(ui, color_segments, "ans", "12", 17);

    ui.separator();
    ui.heading("Percentages");
    ui.label("The '%'-Sign is a shorthand for 'n / 100':");
    help_window_row(ui, color_segments, "20%", "0.2", 18);

    ui.label("The 'of' operator can be used to take a percentage from a value:");
    help_window_row(ui, color_segments, "20% of 100", "20", 19);
    help_window_row(ui, color_segments, "(10 + 10)% of 10 ^ 2", "20", 20);

    ui.separator();
    ui.heading("Equality checks");
    ui.label("The '='-Sign can be used to make a line an equality check:");
    help_window_row(ui, color_segments, "3 + 4 = 7", "True", 21);
    help_window_row(ui, color_segments, "log(2, 8) = 5", "False", 22);

    ui.separator();
    ui.heading("Units");
    ui.label("Units are number suffixes with an optional unit prefix (e.g. 'm', 'k', etc.).");
    ui.label("Numbers with and without units can be mixed in a single calculation.");
    ui.label("If the line consists only of a number with a unit, the unit is written in its long form.");
    help_window_row(ui, color_segments, "10m", "10 Meters", 23);
    help_window_row(ui, color_segments, "(10 + 10)min", "20min", 24);
    help_window_row(ui, color_segments, "20h + 30min", "20.5h", 25);

    ui.label("The 'in' operator can be used to convert between units:");
    help_window_row(ui, color_segments, "180min in h", "3h", 26);

    ui.label("The 'in' operator can also be used to convert the result to a different representation.");
    ui.label("However, this only has an affect when it is the last thing in a line.");
    help_window_row(ui, color_segments, "0xFF in decimal", "255 (decimal is default)", 27);
    help_window_row(ui, color_segments, "255 in hex", "0xFF", 28);
    help_window_row(ui, color_segments, "0x6 in binary", "0b110", 29);
    help_window_row(ui, color_segments, "255km in hex", "0xFF Kilometers", 30);

    ui.separator();
    ui.heading("Solving equations");
    ui.label("The calculator can solve linear equations if you put a question mark in them.");
    help_window_row(ui, color_segments, "3 + ? = 30", "27", 31);
    help_window_row(ui, color_segments, "(3 + ?) * 2 = 30", "12", 32);
    ui.label("The question mark has the following position restrictions:");
    ui.label("- It or its enclosing groups cannot be surrounded by a power sign");
    ui.label("- It can only be an argument of custom functions that are themselves linear");

    ui.add_space(5.0);
    ui.label("If you prefix the question mark with a variable name, its value will be assigned to that variable:");
    help_window_row(ui, color_segments, "(3 + x?) * 2 = 30", "12", 33);
    help_window_row(ui, color_segments, "x", "12", 34);
}