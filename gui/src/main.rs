/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use std::ops::Range;
use std::sync::{Arc, Mutex};

use eframe::{CreationContext, Frame, Storage};
use eframe::egui;
use eframe::egui::panel::PanelState;
use eframe::egui::text_edit::{CursorRange, TextEditState};
use eframe::epaint::Shadow;
use eframe::epaint::text::cursor::Cursor;
use egui::*;

use calculator::{Calculator, Color, ColorSegment, DateFormat, Function as CalcFn, ResultData, Settings, Verbosity};

use crate::widgets::*;

mod widgets;

#[cfg(not(target_arch = "wasm32"))]
const GITHUB_TAGS_URL: &str = "https://api.github.com/repos/david072/funcially/tags";

const VERSION: &str = env!("CARGO_PKG_VERSION");
const FONT_SIZE: f32 = 14.0;
const FONT_ID: FontId = FontId::monospace(FONT_SIZE);
const FOOTER_FONT_SIZE: f32 = 14.0;
const TEXT_EDIT_MARGIN: Vec2 = Vec2::new(4.0, 2.0);
const ERROR_COLOR: Color = Color::RED;

const INPUT_TEXT_EDIT_ID: &str = "input-text-edit";
const PLOT_PANEL_ID: &str = "plot_panel";
const OUTPUT_PANEL_ID: &str = "output_panel";

const TOGGLE_COMMENTATION_SHORTCUT: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND.plus(Modifiers::ALT), Key::N);
const SURROUND_WITH_BRACKETS_SHORTCUT: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::B);
const COPY_RESULT_SHORTCUT: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND.plus(Modifiers::SHIFT), Key::C);
const FORMAT_SHORTCUT: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND.plus(Modifiers::ALT), Key::L);
const LINE_PICKER_SHORTCUT: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::G);
const SEARCH_SHORTCUT: KeyboardShortcut = KeyboardShortcut::new(Modifiers::COMMAND, Key::F);

#[cfg(feature = "experimental")]
fn app_key() -> String {
    eframe::APP_KEY.to_string() + "-experimental"
}

#[cfg(not(feature = "experimental"))]
fn app_key() -> String {
    eframe::APP_KEY.to_string()
}

fn settings_key() -> String {
    app_key() + "_calc-settings"
}

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
        "Funcially",
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
    wasm_bindgen_futures::spawn_local(async {
        eframe::start_web(
            "the_canvas_id",
            web_options,
            Box::new(|cc| Box::new(App::new(cc))),
        )
            .await
            .expect("Failed to start eframe");
    });
}

#[derive(Debug, serde::Deserialize)]
#[cfg(not(target_arch = "wasm32"))]
struct GitHubApiResponseItem {
    name: String,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Function(String, usize, #[serde(skip)] CalcFn);

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum Line {
    Empty,
    WrappedLine,
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

    #[serde(skip)]
    is_ui_enabled: bool,

    is_plot_open: bool,
    is_help_open: bool,
    #[cfg(target_arch = "wasm32")]
    is_download_open: bool,
    is_settings_open: bool,

    is_debug_info_open: bool,
    debug_information: Option<String>,

    use_thousands_separator: bool,

    #[serde(skip)]
    search_state: helpers::SearchState,

    #[serde(skip)]
    show_new_version_dialog: Arc<Mutex<bool>>,
    #[serde(skip)]
    first_frame: bool,
    #[serde(skip)]
    input_should_request_focus: bool,
    #[serde(skip)]
    input_text_cursor_range: CursorRange,
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
            input_should_request_focus: true,
            is_ui_enabled: true,
            is_plot_open: false,
            is_help_open: false,
            #[cfg(target_arch = "wasm32")]
            is_download_open: false,
            show_new_version_dialog: Arc::new(Mutex::new(false)),
            is_settings_open: false,
            is_debug_info_open: false,
            search_state: helpers::SearchState::default(),
            debug_information: None,
            use_thousands_separator: false,
            input_text_cursor_range: CursorRange::one(Cursor::default()),
            bottom_text: format!("v{VERSION}"),
            cached_help_window_color_segments: Vec::new(),
        }
    }
}

impl App<'_> {
    fn new(cc: &CreationContext<'_>) -> Self {
        cc.egui_ctx.set_visuals(Visuals::dark());

        if let Some(storage) = cc.storage {
            let settings: Settings = eframe::get_value(storage, &settings_key()).unwrap_or_else(Settings::default);
            let mut app: Self = eframe::get_value(storage, &app_key()).unwrap_or_default();
            app.calculator.settings = settings;
            return app;
        }

        App::default()
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn check_for_update(&self) {
        let show_new_version_dialog = self.show_new_version_dialog.clone();

        smol::spawn(async move {
            fn get() -> reqwest::Result<Vec<GitHubApiResponseItem>> {
                reqwest::blocking::Client::new()
                    .get(GITHUB_TAGS_URL)
                    .header("User-Agent", format!("funcially/{VERSION} desktop app")) // the API requires a user agent
                    .send()?.json()
            }

            let Ok(mut response) = get() else { return; };
            response.sort_by(|first, second| {
                match version_compare::compare(&first.name, &second.name) {
                    Ok(cmp) => match cmp {
                        version_compare::Cmp::Lt => std::cmp::Ordering::Less,
                        version_compare::Cmp::Eq => std::cmp::Ordering::Equal,
                        version_compare::Cmp::Gt => std::cmp::Ordering::Greater,
                        _ => unreachable!(),
                    }
                    Err(_) => {
                        eprintln!("Failed to compare versions {} and {}", first.name, second.name);
                        std::cmp::Ordering::Equal
                    }
                }
            });

            let GitHubApiResponseItem { name: newest } =
                response.remove(response.len() - 1);

            let result = version_compare::compare(newest, VERSION);
            if let Ok(version_compare::Cmp::Gt) = result {
                let Ok(mut show_dialog) = show_new_version_dialog.lock() else { return; };
                *show_dialog = true;
            }
        }).detach();
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
                    ResultData::Value(number) => number.format(&self.calculator.settings, self.use_thousands_separator),
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
        let input_text_paragraph = self.input_text_cursor_range.primary.pcursor.paragraph;
        for (i, line) in self.source.lines().enumerate() {
            if i != input_text_paragraph { continue; }

            if line.trim().starts_with('#') || line.is_empty() {
                break;
            }

            let mut line = line;
            if let Some(comment_start) = line.find('#') {
                line = &line[0..comment_start];
            }

            let debug_information = match self.calculator.get_debug_info(line, Verbosity::Ast) {
                Ok(info) => Some(info),
                Err(e) => Some(format!("Error generating debug information: {}, {}..{}", e.error, e.start, e.end))
            };

            self.debug_information = debug_information;
            break;
        }
    }

    fn update_lines(&mut self, galley: Arc<Galley>) {
        if self.source == self.source_old { return; }

        self.search_state.update(&self.source);

        self.source_old = self.source.clone();
        // Since we re-calculate everything from the beginning,
        // we need to start with a fresh environment
        self.calculator.reset();

        let mut functions = self.lines.iter()
            .filter(|l| {
                match l {
                    Line::Line { show_in_plot, .. } => *show_in_plot,
                    _ => false,
                }
            })
            .map(|l| {
                if let Line::Line { function: Some(Function(name, ..)), show_in_plot, .. } = l {
                    (name.clone(), *show_in_plot)
                } else { unreachable!() }
            })
            .collect::<Vec<_>>();
        self.lines.clear();
        self.line_numbers_text.clear();

        if galley.rows.is_empty() {
            self.line_numbers_text = "1".to_string();
            return;
        }

        let max_line_number_length = self.source.split('\n').count().to_string().len();

        let mut line = String::new();
        let mut line_index = 1usize;
        let mut did_add_line_index = false;
        let mut empty_lines = 0usize;
        for (i, row) in galley.rows.iter().enumerate() {
            line += row.glyphs.iter().map(|g| g.chr).collect::<String>().as_str();

            if !row.ends_with_newline {
                if !did_add_line_index {
                    self.line_numbers_text += &format!("{: >width$}", line_index.to_string(), width = max_line_number_length);
                    did_add_line_index = true;
                    line_index += 1;
                }
                self.line_numbers_text.push('\n');

                if i != galley.rows.len() - 1 { empty_lines += 1; }
                continue;
            } else {
                if !did_add_line_index {
                    self.line_numbers_text += &format!("{: >width$}", line_index.to_string(), width = max_line_number_length);
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
                        if let Some(i) = functions.iter().position(|(n, _)| n == name) {
                            *show_in_plot = functions[i].1;
                            functions.remove(i);
                        }
                    }
                    self.lines.push(res);
                    for _ in 0..empty_lines {
                        self.lines.push(Line::WrappedLine);
                    }
                    empty_lines = 0;
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
                if let Some(i) = functions.iter().position(|(n, _)| n == name) {
                    *show_in_plot = functions[i].1;
                    functions.remove(i);
                }
            }
            self.lines.push(res);
        }

        if self.line_numbers_text.is_empty() {
            self.line_numbers_text = "1".to_string();
        }
    }

    fn plot_panel(&mut self, ctx: &Context) {
        if FullScreenPlot::is_fullscreen(ctx) { return; }

        SidePanel::right(PLOT_PANEL_ID)
            .resizable(self.is_ui_enabled)
            .show(ctx, |ui| {
                ui.set_enabled(self.is_ui_enabled);

                let response = plot(ui, &self.lines, &self.calculator);
                ui.allocate_ui_at_rect(
                    response.response.rect.shrink(10.0),
                    |ui| {
                        ui.with_layout(Layout::right_to_left(Align::TOP), |ui| {
                            if ui.small_button("Fullscreen").clicked() {
                                FullScreenPlot::set_fullscreen(ui.ctx(), true);
                            }
                        });
                    },
                );
            });
    }

    fn help_window(&mut self, ctx: &Context) {
        let is_help_open = &mut self.is_help_open;
        Window::new("Help")
            .open(is_help_open)
            .vscroll(true)
            .hscroll(true)
            .enabled(self.is_ui_enabled)
            .show(ctx, build_help);
    }

    #[cfg(target_arch = "wasm32")]
    fn download_window(&mut self, ctx: &Context) {
        Window::new("Download")
            .open(&mut self.is_download_open)
            .collapsible(false)
            .resizable(false)
            .enabled(self.is_ui_enabled)
            .show(ctx, |ui| {
                ui.heading("Desktop App");
                ui.horizontal(|ui| {
                    ui.spacing_mut().item_spacing.x = 0.0;
                    ui.label("If you're on desktop, you can download the ");
                    ui.hyperlink_to("desktop app", "https://github.com/david072/funcially/releases");
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
            .resizable(false)
            .enabled(self.is_ui_enabled)
            .show(ctx, |ui| {
                let mut update = false;

                ui.heading("General");
                ui.add_space(10.0);
                update |= ui.checkbox(&mut self.use_thousands_separator, "Use thousands separator").clicked();

                ui.separator();
                ui.heading("Date format");
                ui.add_space(10.0);
                ComboBox::from_label("Format")
                    .selected_text(self.calculator.settings.date.format.to_string())
                    .show_ui(ui, |ui| {
                        let current_format = &mut self.calculator.settings.date.format;
                        update |= ui.selectable_value(current_format, DateFormat::Dmy, "DMY").clicked();
                        update |= ui.selectable_value(current_format, DateFormat::Mdy, "MDY").clicked();
                        update |= ui.selectable_value(current_format, DateFormat::Ymd, "YMD").clicked();
                    });

                ComboBox::from_label("Delimiter")
                    .selected_text(self.calculator.settings.date.delimiter.to_string())
                    .show_ui(ui, |ui| {
                        const DELIMITERS: &str = ".,'/-";
                        let current_del = &mut self.calculator.settings.date.delimiter;
                        for char in DELIMITERS.chars() {
                            update |= ui.selectable_value(current_del, char, char.to_string()).clicked();
                        }
                    });

                ui.separator();
                CollapsingHeader::new("Debug").default_open(true).show(ui, |ui| {
                    let mut debug_on_hover = ui.ctx().debug_on_hover();
                    ui.checkbox(&mut debug_on_hover, "Debug On Hover");
                    ui.ctx().set_debug_on_hover(debug_on_hover);

                    let mut tesselation_options = ui.ctx().options().tessellation_options;
                    ui.checkbox(&mut tesselation_options.debug_paint_clip_rects, "Paint clip rectangles");
                    ui.checkbox(&mut tesselation_options.debug_paint_text_rects, "Paint text bounds");
                    *ui.ctx().tessellation_options() = tesselation_options;
                });
                ui.hyperlink_to("Source code", "https://github.com/david072/funcially");

                if update {
                    // Make update_lines() refresh on the next frame, since now source and source_old are not the same
                    self.source_old.clear();
                }
            });
    }

    fn show_debug_information(&mut self, ctx: &Context) {
        let debug_information = &mut self.debug_information;

        Window::new("Debug Information")
            .open(&mut self.is_debug_info_open)
            .vscroll(true)
            .enabled(self.is_ui_enabled)
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

    /// Handles shortcuts that modify what's inside the textedit => needs a cursor range
    fn handle_text_edit_shortcuts(&mut self, ui: &mut Ui, cursor_range: CursorRange) {
        if ui.input_mut().consume_shortcut(&TOGGLE_COMMENTATION_SHORTCUT) {
            self.toggle_commentation(cursor_range);
        }
        if ui.input_mut().consume_shortcut(&SURROUND_WITH_BRACKETS_SHORTCUT) {
            self.surround_selection_with_brackets(cursor_range);
        }
        if ui.input_mut().consume_shortcut(&COPY_RESULT_SHORTCUT) {
            self.copy_result(ui, cursor_range);
        }
    }

    /// Handles shortcuts that are global => don't need a cursor range
    fn handle_shortcuts(&mut self, ui: &Ui) {
        if ui.input_mut().consume_shortcut(&FORMAT_SHORTCUT) { self.format_source(); }
        if ui.input_mut().consume_shortcut(&LINE_PICKER_SHORTCUT) {
            self.is_ui_enabled = false;
            LinePickerDialog::set_open(ui.ctx(), true);
        }
        if ui.input_mut().consume_shortcut(&SEARCH_SHORTCUT) {
            self.search_state.open = true;
            self.search_state.should_have_focus = true;
        }
    }

    fn toggle_commentation(&mut self, cursor_range: CursorRange) {
        let start_line = cursor_range.primary.pcursor.paragraph;
        let end_line = cursor_range.secondary.pcursor.paragraph;

        let has_uncommented_line = self.source.lines()
            .skip(start_line)
            .filter(|l| !l.is_empty())
            .take(if end_line == 0 { 1 } else { end_line })
            .any(|l| !l.trim_start().starts_with('#'));

        // If there is an uncommented line, we even the lines out by commenting
        // uncommented lines too.
        // Otherwise, we uncomment, since all lines are commented.

        let mut new_source = String::new();
        let line_count = self.source.lines().count();
        for (i, line) in self.source.lines().enumerate() {
            if i < start_line || i > end_line {
                new_source += line;
                if i != line_count - 1 { new_source.push('\n'); }
                continue;
            } else if line.is_empty() {
                if i != line_count - 1 { new_source.push('\n'); }
                continue;
            }

            let trimmed = line.trim_start();
            let offset = line.len() - trimmed.len();

            if has_uncommented_line {
                if !line.trim_start().starts_with('#') {
                    for _ in 0..offset { new_source.push(' '); }
                    new_source.push('#');
                    new_source += &line[offset..];
                    if i != line_count - 1 { new_source.push('\n'); }
                } else {
                    new_source += line;
                    if i != line_count - 1 { new_source.push('\n'); }
                }
            } else {
                for _ in 0..offset { new_source.push(' '); }
                new_source += line.chars()
                    .skip(offset + 1)
                    .collect::<String>().as_str();
                if i != line_count - 1 { new_source.push('\n'); }
            }
        }

        self.source = new_source;
    }

    fn surround_selection_with_brackets(&mut self, cursor_range: CursorRange) {
        // Check that we have a range spanning only one line
        let primary = &cursor_range.primary.pcursor;
        let secondary = &cursor_range.secondary.pcursor;

        if (*primary == *secondary) || (primary.paragraph != secondary.paragraph) {
            return;
        }

        let mut new_source = String::new();
        let line_count = self.source.lines().count();
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
            if i != line_count - 1 {
                new_source.push('\n');
            }
        }

        self.source = new_source;
    }

    fn copy_result(&mut self, ui: &mut Ui, cursor_range: CursorRange) {
        let line = cursor_range.primary.rcursor.row;
        if let Some(Line::Line { output_text, .. }) = self.lines.get(line) {
            ui.output().copied_text = output_text.to_string();
        }
    }

    fn format_source(&mut self) {
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

    fn line_picker_dialog(&mut self, ctx: &Context) {
        let result = LinePickerDialog::new(
            FONT_ID,
            Id::new(INPUT_TEXT_EDIT_ID),
            &self.source,
        ).show(ctx);

        if result {
            self.is_ui_enabled = true;
            self.input_should_request_focus = true;
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn new_version_dialog(&mut self, ctx: &Context) {
        if let Ok(mut show_new_version_dialog) = self.show_new_version_dialog.lock() {
            if *show_new_version_dialog {
                self.is_ui_enabled = false;
                dialog(ctx, Some("New Version"), |ui| {
                    ui.vertical(|ui| {
                        ui.label("There is a new version available!");

                        ui.horizontal_wrapped(|ui| {
                            ui.spacing_mut().item_spacing.x = 0.0;
                            ui.label("Download the latest version from ");
                            ui.hyperlink_to("the Website", "https://funcially.com/download");
                            ui.label(".");
                        });

                        ui.add_space(15.0);
                        ui.vertical_centered(|ui| {
                            if ui.button("Ok").clicked() {
                                *show_new_version_dialog = false;
                                self.is_ui_enabled = true;
                            }
                        });
                    });

                    if helpers::is_key_pressed(ui, Key::Escape) {
                        *show_new_version_dialog = false;
                        self.is_ui_enabled = true;
                    }
                });
            }
        }
    }

    fn search_ui(&mut self, ui: &mut Ui) {
        if !self.search_state.open { return; }
        let output = TextEdit::singleline(&mut self.search_state.text)
            .font(FontSelection::from(FONT_ID))
            .hint_text("Search")
            .show(ui);

        ui.label(format!(
            "{}/{}",
            self.search_state.selected_range.map(|i| i + 1).unwrap_or_default(),
            self.search_state.occurrences.len()
        ));


        ui.toggle_value(&mut self.search_state.match_case, "Aa")
            .on_hover_text("Match case");

        self.search_state.update(&self.source);

        if ui.small_button("X").clicked() {
            self.search_state.open = false;
            self.input_should_request_focus = true;
            self.search_state.set_range_in_text_edit_state(ui.ctx(), INPUT_TEXT_EDIT_ID);
        }

        if self.search_state.should_have_focus {
            output.response.request_focus();
            self.search_state.should_have_focus = false;
        }

        if helpers::is_key_pressed(ui, Key::Escape) {
            self.search_state.open = false;
            self.search_state.should_have_focus = false;
            self.input_should_request_focus = true;
            self.search_state.set_range_in_text_edit_state(ui.ctx(), INPUT_TEXT_EDIT_ID);
        } else if helpers::is_key_pressed(ui, Key::Enter) {
            // TextEdit automatically looses focus when pressing enter, so we have to take it
            // back
            output.response.request_focus();
            if helpers::is_key_pressed_fn(
                ui,
                |k, down, mods| *k == Key::Enter && down && mods.shift,
            ) {
                self.search_state.decrement_selected_range();
            } else {
                self.search_state.increment_selected_range();
            }

            if !self.search_state.occurrences.is_empty() {
                self.search_state.set_range_in_text_edit_state(ui.ctx(), INPUT_TEXT_EDIT_ID);
            }
        }
    }
}

impl eframe::App for App<'_> {
    fn update(&mut self, ctx: &Context, _frame: &mut Frame) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            if self.first_frame {
                self.check_for_update();
            }

            self.new_version_dialog(ctx);
        }

        if !self.cached_help_window_color_segments.is_empty() && !self.is_help_open {
            self.cached_help_window_color_segments.clear();
        }
        if !self.is_debug_info_open { self.debug_information = None; }

        FullScreenPlot::new(
            ctx.available_rect().size(),
            &self.lines,
            &self.calculator,
        ).maybe_show(ctx);

        self.line_picker_dialog(ctx);

        TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            ui.set_enabled(self.is_ui_enabled);

            menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.toggle_value(&mut self.is_settings_open, "Settings").clicked() {
                        ui.close_menu();
                    }

                    if ui.button("Collapse side panels").clicked() {
                        fn collapse_panel_state(ctx: &Context, id: impl Into<Id>) {
                            let id = id.into();
                            if let Some(mut state) = PanelState::load(ctx, id) {
                                state.rect.set_width(5.0);
                                ctx.data().insert_persisted(id, state);
                            }
                        }

                        collapse_panel_state(ctx, OUTPUT_PANEL_ID);
                        collapse_panel_state(ctx, PLOT_PANEL_ID);
                        self.is_plot_open = false;
                    }

                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        ui.separator();
                        if ui.button("Exit").clicked() {
                            _frame.close();
                        }
                    }
                });

                ui.menu_button("Edit", |ui| {
                    let shortcut = ui.ctx().format_shortcut(&SURROUND_WITH_BRACKETS_SHORTCUT);
                    if shortcut_button(ui, "Surround selection with brackets", &shortcut).clicked() {
                        self.surround_selection_with_brackets(self.input_text_cursor_range);
                        ui.close_menu();
                    }

                    let shortcut = ui.ctx().format_shortcut(&TOGGLE_COMMENTATION_SHORTCUT);
                    if shortcut_button(ui, "(Un)Comment selected lines", &shortcut).clicked() {
                        self.toggle_commentation(self.input_text_cursor_range);
                        ui.close_menu();
                    }

                    let shortcut = ui.ctx().format_shortcut(&COPY_RESULT_SHORTCUT);
                    if shortcut_button(ui, "Copy result", &shortcut).clicked() {
                        self.copy_result(ui, self.input_text_cursor_range);
                        ui.close_menu();
                    }

                    let shortcut = ui.ctx().format_shortcut(&FORMAT_SHORTCUT);
                    if shortcut_button(ui, "Format input", &shortcut).clicked() {
                        self.format_source();
                        ui.close_menu();
                    }
                });

                ui.menu_button("Navigate", |ui| {
                    let shortcut = ui.ctx().format_shortcut(&SEARCH_SHORTCUT);
                    if shortcut_button(ui, "Search", &shortcut).clicked() {
                        self.search_state.open = true;
                        self.search_state.should_have_focus = true;
                        ui.close_menu();
                    }

                    let shortcut = ui.ctx().format_shortcut(&LINE_PICKER_SHORTCUT);
                    if shortcut_button(ui, "Go to Line", &shortcut).clicked() {
                        LinePickerDialog::set_open(ctx, true);
                        self.is_ui_enabled = false;
                        ui.close_menu();
                    }
                });

                ui.menu_button("Debug", |ui| {
                    if ui.button("Print Debug Information for current line").clicked() {
                        self.get_debug_info_for_current_line();
                        self.is_debug_info_open = true;
                        ui.close_menu();
                    }
                });

                #[cfg(target_arch = "wasm32")]
                ui.toggle_value(&mut self.is_download_open, "Download");

                ui.toggle_value(&mut self.is_help_open, "Help");

                ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                    ui.toggle_value(&mut self.is_plot_open, "🗠 Plot");
                });
            })
        });

        TopBottomPanel::bottom("bottom_bar")
            .frame(egui::Frame {
                inner_margin: style::Margin {
                    left: 2.0,
                    right: 8.0,
                    top: 2.0,
                    bottom: 2.0,
                },
                rounding: Rounding::none(),
                shadow: Shadow::default(),
                ..egui::Frame::window(&ctx.style())
            })
            .show(ctx, |ui| {
                ui.set_enabled(self.is_ui_enabled);

                ui.with_layout(Layout::left_to_right(Align::Center), |ui| {
                    self.search_ui(ui);
                    ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                        let bottom_text = RichText::new(&self.bottom_text)
                            .font(FontId::proportional(FOOTER_FONT_SIZE));
                        ui.label(bottom_text);
                    });
                });
            });

        // We wait for the second frame to have the lines updated if they've been loaded on startup
        if !self.first_frame && self.is_plot_open { self.plot_panel(ctx); }

        if self.is_help_open { self.help_window(ctx); }
        #[cfg(target_arch = "wasm32")]
        if self.is_download_open { self.download_window(ctx); }
        if self.is_settings_open { self.settings_window(ctx); }
        if self.is_debug_info_open { self.show_debug_information(ctx); }

        if !self.lines.is_empty() {
            #[cfg(not(target_arch = "wasm32"))]
                let default_width = _frame.info().window_info.size.x * (1.0 / 3.0);
            #[cfg(target_arch = "wasm32")]
                let default_width = 40.0;

            SidePanel::right(OUTPUT_PANEL_ID)
                .default_width(default_width)
                .show(ctx, |ui| {
                    ui.add_space(8.0);
                    ui.spacing_mut().item_spacing.y = 0.0;

                    let mut line_index = 1usize;
                    for line in &mut self.lines {
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
                                            let mut show_ui = |ui: &mut Ui| {
                                                ui.checkbox(show_in_plot, "Plot");
                                            };

                                            if ui.available_width() < 30.0 {
                                                ui.menu_button("☰", show_ui);
                                            } else {
                                                show_ui(ui);
                                            }
                                            ui.add_space(-2.0);
                                        });
                                        continue;
                                    }
                                }
                            }

                            output_text(ui, text, FONT_ID, line_index);
                        } else {
                            ui.add_space(FONT_SIZE + 2.0);
                        }

                        if matches!(line, Line::Line { .. } | Line::Empty) {
                            line_index += 1;
                        }
                    }
                });
        }

        CentralPanel::default().show(ctx, |ui| {
            ui.set_enabled(self.is_ui_enabled);

            // FIXME: Scroll bar is too long (potential issue in egui?)
            let rows = ((ui.available_height() - TEXT_EDIT_MARGIN.y) / FONT_SIZE) as usize;

            ScrollArea::vertical().show(ui, |ui| {
                ui.with_layout(Layout::left_to_right(Align::TOP), |ui| {
                    let char_width = ui.fonts().glyph_width(&FONT_ID, '0') + 2.0;

                    let longest_row_chars = self.line_numbers_text.lines()
                        .last()
                        .map(str::len)
                        .unwrap_or_default() as f32;

                    TextEdit::multiline(&mut self.line_numbers_text)
                        .frame(false)
                        .font(FontSelection::from(FONT_ID))
                        .interactive(false)
                        .desired_width(longest_row_chars * char_width)
                        .desired_rows(rows)
                        .margin(vec2(0.0, 2.0))
                        .show(ui);

                    if let Some(mut input_state) = TextEditState::load(ctx, Id::new(INPUT_TEXT_EDIT_ID)) {
                        if let Some(mut cursor_range) = input_state.ccursor_range() {
                            let mut i = 0usize;
                            let events = &mut ui.input_mut().events;
                            while i < events.len() {
                                if let Event::Text(text) = &events[i] {
                                    let mut remove = false;
                                    for c in [')', ']', '}'] {
                                        if *text == String::from(c) &&
                                            self.source.chars().nth(cursor_range.primary.index)
                                                .map(|char| char == c)
                                                .unwrap_or_default() {
                                            cursor_range.primary.index += 1;
                                            cursor_range.secondary.index += 1;
                                            input_state.set_ccursor_range(Some(cursor_range));
                                            remove = true;
                                        }
                                    }
                                    if remove {
                                        events.remove(i);
                                        continue;
                                    }
                                }

                                i += 1;
                            }
                        }

                        input_state.store(ctx, Id::new(INPUT_TEXT_EDIT_ID));
                    }

                    let lines = &mut self.lines;
                    let output = TextEdit::multiline(&mut self.source)
                        .id(Id::new(INPUT_TEXT_EDIT_ID))
                        .lock_focus(true)
                        .hint_text("Calculate something")
                        .frame(false)
                        .desired_width(ui.available_width())
                        .font(FontSelection::from(FONT_ID))
                        .desired_rows(rows)
                        .layouter(&mut input_layouter(
                            lines,
                            if self.search_state.open { Some(self.search_state.occurrences.clone()) } else { None },
                            self.search_state.selected_range_if_open(),
                        ))
                        .show(ui);

                    self.update_lines(output.galley);

                    if let Some(range) = output.cursor_range {
                        self.input_text_cursor_range = range;

                        for event in &ui.input().events {
                            if let Event::Text(text) = event {
                                if let Some(c) = match text.as_str() {
                                    "(" => Some(')'),
                                    "{" => Some('}'),
                                    "[" => Some(']'),
                                    _ => None,
                                } {
                                    self.source.insert(range.primary.ccursor.index, c);
                                }
                            }
                        }
                    }

                    if self.input_should_request_focus {
                        self.input_should_request_focus = false;
                        output.response.request_focus();
                    }

                    if let Some(range) = output.cursor_range {
                        self.handle_text_edit_shortcuts(ui, range);
                    }
                    self.handle_shortcuts(ui);
                });
            });
        });

        self.first_frame = false;
    }

    fn save(&mut self, storage: &mut dyn Storage) {
        eframe::set_value(storage, &app_key(), self);
        eframe::set_value(storage, &settings_key(), &self.calculator.settings);
    }
}

fn input_layouter(
    lines: &[Line],
    highlighted_ranges: Option<Vec<Range<usize>>>,
    selection_preview: Option<Range<usize>>,
) -> impl FnMut(&Ui, &str, f32) -> Arc<Galley> + '_ {
    // we need a Vec to chain it to the other iterators in `iter_over_all_ranges()`
    let selection_preview_vec = if let Some(sp) = &selection_preview {
        vec![sp.clone()]
    } else {
        vec![]
    };
    let highlighted_ranges = highlighted_ranges.unwrap_or_default();

    move |ui, string, wrap_width| {
        let mut job = text::LayoutJob {
            text: string.into(),
            ..Default::default()
        };

        if !lines.is_empty() {
            let mut last_end = 0usize;
            let mut offset = 0usize;
            let mut line_counter = 0usize;

            let verify_char_boundary = |i: usize| {
                let mut i = i;
                while !string.is_char_boundary(i) { i += 1; }
                i
            };

            'outer: for line in string.lines() {
                if line_counter > lines.len() { break; }

                let trimmed_line = line.trim();
                if !trimmed_line.is_empty() {
                    let segments = 'blk: {
                        // If the line is a comment, we don't have color segments, however we might
                        // have highlights, etc. in this line, so we just return an empty slice
                        const EMPTY: &[ColorSegment] = &[];
                        if trimmed_line.starts_with('#') { break 'blk EMPTY; }

                        // NOTE: We use `Line::Empty`s for empty lines and `Line::WrappedLine` to
                        //  add spacing if the line spans multiple rows. We have to skip these
                        //  lines here to get to the actual color segments.
                        while matches!(lines.get(line_counter), Some(Line::Empty | Line::WrappedLine)) { line_counter += 1; }

                        let Some(Line::Line { color_segments: segments, .. }) =
                            lines.get(line_counter) else { break 'outer; };
                        &segments[..]
                    };

                    // We often add `offset` to numbers here. This is because we need to translate
                    // per-line indices to their respective index in `string` (the full text).

                    let segments = segments.iter()
                        .map(|seg| {
                            let mut seg = seg.clone();
                            seg.range.start += offset;
                            seg.range.end += offset;
                            seg
                        })
                        .collect::<Vec<_>>();

                    let iter_over_all_ranges = || {
                        segments.iter().map(|s| &s.range)
                            .chain(highlighted_ranges.iter())
                            .chain(selection_preview_vec.iter())
                    };

                    // Adds a section. It finds out what color it needs to have, as well as whether
                    // the section needs to be highlighted by checking what ranges contain the
                    // given index.
                    // NOTE: We pass last_end since the clojure would otherwise borrow it, causing
                    //       issues further down
                    let mut add_section = |i_in_string: usize, last_end: usize| {
                        let segment = segments.iter()
                            .find(|seg| {
                                (seg.range.start..seg.range.end)
                                    .contains(&(i_in_string - 1))
                            })
                            .map(|seg| {
                                Color32::from_rgba_premultiplied(
                                    seg.color.0[0],
                                    seg.color.0[1],
                                    seg.color.0[2],
                                    seg.color.0[3],
                                )
                            });
                        let highlighted = highlighted_ranges.iter()
                            // don't need to add offset here, since the highlighted_ranges already
                            // have it added
                            .any(|range| range.contains(&(i_in_string - 1)));
                        let is_selection_preview = selection_preview
                            .as_ref()
                            .map(|range| range.contains(&(i_in_string - 1)))
                            .unwrap_or(false);

                        let last_end = verify_char_boundary(last_end);
                        let i_in_string = verify_char_boundary(i_in_string);

                        job.sections.push(text::LayoutSection {
                            leading_space: 0.0,
                            byte_range: last_end..i_in_string,
                            format: TextFormat {
                                font_id: FONT_ID,
                                color: segment.unwrap_or(Color32::GRAY),
                                underline: if highlighted {
                                    Stroke::new(3.0, Color32::GOLD)
                                } else {
                                    Stroke::NONE
                                },
                                background: if is_selection_preview {
                                    ui.visuals().selection.bg_fill
                                } else { Color32::TRANSPARENT },
                                ..Default::default()
                            },
                        });
                    };

                    for i in 0..line.len() {
                        let i_in_string = i + offset;

                        // check if this char is the start of a range
                        let is_start = iter_over_all_ranges().any(|range| range.start == i_in_string);
                        // check if this char is the end of a range
                        let is_end = iter_over_all_ranges().any(|range| range.end == i_in_string);

                        // if this that is at the end of a range, or we're at the start and have
                        // characters left to add (last_end is not here)
                        if is_end || is_start && last_end != i_in_string {
                            add_section(i_in_string, last_end);
                            last_end = i_in_string;
                        }
                    }

                    if last_end != line.len() {
                        let mut i_in_string = line.len() + offset;
                        add_section(i_in_string, last_end);
                        if i_in_string < string.len() {
                            let start = verify_char_boundary(i_in_string);
                            let end = verify_char_boundary(i_in_string + 1);
                            job.sections.push(helpers::section(start..end, FONT_ID, Color32::GRAY));
                            i_in_string += 1;
                        }
                        last_end = i_in_string;
                    }
                }

                offset += line.len() + 1;
                line_counter += 1;
            }

            if last_end != string.len() {
                let last_end = verify_char_boundary(last_end);
                job.sections.push(helpers::section(last_end..string.len(), FONT_ID, Color32::GRAY));
            }
        } else {
            job.sections.push(helpers::section(0..string.len(), FONT_ID, Color32::GRAY));
        }

        job.wrap.max_width = wrap_width;
        ui.fonts().layout_job(job)
    }
}
