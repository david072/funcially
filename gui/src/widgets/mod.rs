/*
 * Copyright (c) 2022, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use eframe::egui::*;
use eframe::egui::style::Margin;
use eframe::egui::text_edit::TextEditState;
use eframe::epaint::Shadow;

use calculator::{Calculator, colorize_text, ColorSegment};
pub use helpers::*;

use crate::Line;

pub mod helpers;

macro_rules! storable {
    ($st:ident) => {
        impl $st {
            pub fn load(ctx: &Context, id: &str) -> $st {
                ctx.data()
                    .get_temp(Id::new(id))
                    .unwrap_or_default()
            }

            pub fn store(self, ctx: &Context, id: &str) {
                ctx.data().insert_temp(Id::new(id), self)
            }
        }
    }
}

const LINE_PICKER_ID: &str = "line-picker-dialog";
const FULL_SCREEN_PLOT_ID: &str = "full-screen-plot";

#[derive(Default, Clone, serde::Serialize, serde::Deserialize)]
pub struct LinePickerDialogState {
    is_open: bool,
    text: String,
}

storable!(LinePickerDialogState);

pub struct LinePickerDialog<'a> {
    font_id: FontId,
    target_text_edit_id: Id,
    target_text_edit_text: &'a str,
}

fn dialog<R>(ctx: &Context, add_contents: impl FnOnce(&mut Ui) -> R) -> R {
    let response = Window::new("Go to Line")
        .title_bar(false)
        .anchor(Align2::CENTER_CENTER, Vec2::ZERO)
        .resizable(false)
        .scroll2([false, false])
        .collapsible(false)
        .show(ctx, add_contents);

    response.unwrap().inner.unwrap()
}

impl<'a> LinePickerDialog<'a> {
    pub fn set_open(ctx: &Context, open: bool) {
        let mut state = LinePickerDialogState::load(ctx, LINE_PICKER_ID);
        state.is_open = open;
        state.store(ctx, LINE_PICKER_ID);
    }

    pub fn new(
        font_id: FontId,
        target_text_edit_id: Id,
        target_text_edit_text: &'a str,
    ) -> Self {
        Self {
            font_id,
            target_text_edit_id,
            target_text_edit_text,
        }
    }

    /// Returns whether something happened, and if something happened, whether a line as picked
    pub fn show(&mut self, ctx: &Context) -> Option<bool> {
        let mut state = LinePickerDialogState::load(ctx, LINE_PICKER_ID);

        let mut result: Option<bool> = None;
        if state.is_open {
            dialog(ctx, |ui| {
                let output = TextEdit::singleline(&mut state.text)
                    .hint_text("Go to Line")
                    .font(FontSelection::from(self.font_id.clone()))
                    .layouter(&mut |ui, str, wrap_width| {
                        let job = text::LayoutJob::simple(
                            str.into(),
                            self.font_id.clone(),
                            if str.parse::<usize>().is_ok() { Color32::GRAY } else { Color32::RED },
                            wrap_width,
                        );
                        ui.fonts().layout_job(job)
                    })
                    .show(ui);
                output.response.request_focus();

                let events = {
                    // avoid deadlock when loading TextEditState
                    let res = ui.input().events.clone();
                    res
                };
                for event in events {
                    if let Event::Key { key, .. } = event {
                        if key == Key::Escape {
                            state.text = String::new();
                            state.is_open = false;
                            result = Some(false);
                        } else if key == Key::Enter {
                            let Ok(line_index) = state.text.parse::<usize>() else { return; };

                            let Some(mut text_edit_state) = TextEditState::load(ctx, self.target_text_edit_id) else { return; };
                            let Some(mut cursor_range) = text_edit_state.ccursor_range() else { return; };

                            let mut index = 0usize;
                            for (i, line) in self.target_text_edit_text.lines().enumerate() {
                                if i == line_index - 1 { break; }
                                index += line.len() + 1;
                            }

                            if cursor_range.primary == cursor_range.secondary {
                                cursor_range.secondary.index = index;
                            }
                            cursor_range.primary.index = index;
                            text_edit_state.set_ccursor_range(Some(cursor_range));
                            text_edit_state.store(ctx, self.target_text_edit_id);

                            state.text = String::new();
                            state.is_open = false;
                            result = Some(true);
                        }
                    }
                }
            });
        }

        state.store(ctx, LINE_PICKER_ID);
        result
    }
}

#[derive(Default, Clone, serde::Serialize, serde::Deserialize)]
struct FullScreenPlotState {
    is_full_screen: bool,
}

storable!(FullScreenPlotState);

pub struct FullScreenPlot<'a> {
    full_size: Vec2,
    lines: &'a Vec<Line>,
    calculator: &'a Calculator<'a>,
}

impl<'a> FullScreenPlot<'a> {
    pub fn set_fullscreen(ctx: &Context, fullscreen: bool) {
        let mut state = FullScreenPlotState::load(ctx, FULL_SCREEN_PLOT_ID);
        state.is_full_screen = fullscreen;
        state.store(ctx, FULL_SCREEN_PLOT_ID);
    }

    pub fn is_fullscreen(ctx: &Context) -> bool {
        FullScreenPlotState::load(ctx, FULL_SCREEN_PLOT_ID).is_full_screen
    }

    pub fn new(
        full_size: Vec2,
        lines: &'a Vec<Line>,
        calculator: &'a Calculator<'a>,
    ) -> Self {
        Self {
            full_size,
            lines,
            calculator,
        }
    }

    pub fn maybe_show(&self, ctx: &Context) {
        let mut state = FullScreenPlotState::load(ctx, FULL_SCREEN_PLOT_ID);

        // if we're not in full, stop showing
        if !state.is_full_screen {
            state.store(ctx, FULL_SCREEN_PLOT_ID);
            return;
        }

        Window::new("__full_screen_plot_window")
            .title_bar(false)
            .frame(Frame {
                stroke: Stroke::none(),
                shadow: Shadow::default(),
                rounding: Rounding::none(),
                inner_margin: Margin::same(0.0),
                ..Frame::window(&ctx.style())
            })
            .anchor(Align2::RIGHT_TOP, Vec2::ZERO)
            .min_height(100.0)
            .resizable(false)
            .fixed_size(self.full_size)
            .show(ctx, |ui| {
                let response = plot(ui, self.lines, self.calculator);

                // only show this is we're in fullscreen and the animation has finished
                ui.allocate_ui_at_rect(
                    response.response.rect.shrink(10.0),
                    |ui| {
                        ui.with_layout(Layout::right_to_left(Align::TOP), |ui| {
                            if ui.small_button("✖ Close").clicked() {
                                state.is_full_screen = false;
                            }
                        });
                    },
                );
            });

        state.store(ctx, FULL_SCREEN_PLOT_ID);
    }
}

pub fn plot(ui: &mut Ui, lines: &Vec<Line>, calculator: &Calculator) -> InnerResponse<()> {
    plot::Plot::new("calculator_plot")
        .data_aspect(1.0)
        .coordinates_formatter(
            plot::Corner::LeftBottom, plot::CoordinatesFormatter::default(),
        )
        .legend(plot::Legend::default().position(plot::Corner::RightBottom))
        .show(ui, |plot_ui| {
            for line in lines {
                if let Line::Line { function, show_in_plot, .. } = line {
                    if !show_in_plot { continue; }
                    if let Some(function) = function {
                        if function.1 != 1 { continue; }

                        let env = calculator.clone_env();
                        let f = function.2.clone();
                        let currencies = calculator.currencies.clone();

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
        })
}

pub fn vertical_spacer(ui: &mut Ui) -> Response {
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

pub fn output_text(ui: &mut Ui, str: &str, font_id: FontId, index: usize) -> Response {
    let text: WidgetText = str.into();
    let valign = ui.layout().vertical_align();

    let mut text_job = text.into_text_job(
        ui.style(), FontSelection::FontId(font_id.clone()), valign,
    );
    text_job.job.wrap.max_width = f32::INFINITY;
    text_job.job.halign = Align::RIGHT;
    let galley = text_job.into_galley(&ui.fonts());

    let glyph_width = ui.fonts().glyph_width(&font_id, '0');
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
                font_id,
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

        let mut show_copied_text = false;
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

pub fn shortcut_button(ui: &mut Ui, text: &str, shortcut: &str) -> Response {
    let text: WidgetText = text.into();
    let shortcut: WidgetText = shortcut.into();

    let text = text.into_galley(ui, Some(false), 0.0, TextStyle::Button);
    let shortcut = shortcut.into_galley(ui, Some(false), 0.0, TextStyle::Small);

    let frame = ui.visuals().button_frame;
    // for some reason, the y component of button_padding is 0 here...?
    let button_padding = ui.spacing().button_padding.at_least(vec2(2.0, 2.0));

    let mut content_size = text.size() + vec2(shortcut.size().x, 0.0) + vec2(10.0, 0.0);
    let mut desired_size = content_size + 2.0 * button_padding;

    // expand sizes if we have space left
    if ui.available_width() > desired_size.x {
        desired_size.x = ui.available_width();
        content_size.x = desired_size.x - 2.0 * button_padding.x;
    }

    let (rect, response) = ui.allocate_at_least(desired_size, Sense::click());
    response.widget_info(|| WidgetInfo::labeled(WidgetType::Button, text.text()));

    if ui.is_rect_visible(rect) {
        let visuals = ui.style().interact(&response);
        let text_pos = ui.layout()
            .align_size_within_rect(content_size, rect.shrink2(button_padding));

        if frame {
            let fill = visuals.bg_fill;
            let stroke = visuals.bg_stroke;
            ui.painter().rect(
                rect.expand(visuals.expansion),
                visuals.rounding,
                fill,
                stroke,
            );
        }

        text.paint_with_visuals(ui.painter(), text_pos.min, visuals);
        let shortcut_pos = pos2(
            text_pos.max.x - shortcut.size().x,
            text_pos.min.y + (text_pos.height() - shortcut.size().y) / 2.0,
        );
        shortcut.paint_with_visuals(ui.painter(), shortcut_pos, visuals);
    }

    response
}

fn help_window_row(ui: &mut Ui, font_id: FontId, color_segments: &mut Vec<Vec<ColorSegment>>, input: &str, output: &str, color_index: usize) {
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
            .font(FontSelection::FontId(font_id.clone()))
            .layouter(&mut |ui, string, wrap_width| {
                let mut job = text::LayoutJob {
                    text: string.into(),
                    ..Default::default()
                };

                let mut end = 0usize;
                layout_segments(font_id.clone(), color_segments, &mut job, string, &mut end, 0);

                job.wrap.max_width = wrap_width;
                ui.fonts().layout_job(job)
            })
            .show(ui);

        TextEdit::singleline(&mut output)
            .interactive(false)
            .font(FontSelection::FontId(font_id.clone()))
            .text_color(Color32::GREEN)
            .show(ui);
    });
}

pub fn build_help(ui: &mut Ui, font_id: FontId, color_segments: &mut Vec<Vec<ColorSegment>>) {
    ui.horizontal(|ui| {
        ui.spacing_mut().item_spacing.x = 0.0;
        ui.label("This is a brief overview of the features. A more detailed documentation can be found ");
        ui.hyperlink_to("here", "https://github.com/david072/funcially/wiki");
        ui.label(".");
    });

    ui.heading("Numbers");
    ui.label("You can type in numbers in different formats:");
    help_window_row(ui, font_id.clone(), color_segments, "123", "123", 0);
    help_window_row(ui, font_id.clone(), color_segments, "10.5 + .5", "12", 1);
    help_window_row(ui, font_id.clone(), color_segments, "0xFF", "255", 2);
    help_window_row(ui, font_id.clone(), color_segments, "0b110", "6", 3);

    ui.separator();
    ui.heading("Operators");
    ui.label("Basic operators: +, -, *, /");
    help_window_row(ui, font_id.clone(), color_segments, "3 + 4 * 2", "11", 4);
    help_window_row(ui, font_id.clone(), color_segments, "8 / 2 - 2", "2", 5);

    ui.label("Extended operators: ^, &, |, <number>!, !<number>");
    help_window_row(ui, font_id.clone(), color_segments, "10 ^ 2 * 4", "400", 6);
    help_window_row(ui, font_id.clone(), color_segments, "5!", "120", 7);

    ui.label("The multiplication sign can be left out before variables, functions and groups.");

    ui.separator();
    ui.heading("Groups");
    help_window_row(ui, font_id.clone(), color_segments, "((3 + 3) / 2) * 3", "9", 8);

    ui.separator();
    ui.heading("Functions");
    ui.label("Builtin functions include 'sin', 'cos', 'log' or 'sqrt':");
    help_window_row(ui, font_id.clone(), color_segments, "sin(30)", "0.5", 9);
    help_window_row(ui, font_id.clone(), color_segments, "sqrt(20 + 5)", "5", 10);
    help_window_row(ui, font_id.clone(), color_segments, "2log(2, 8)", "6", 11);

    ui.label("You can also define your own custom functions:");
    help_window_row(ui, font_id.clone(), color_segments, "f(x) := x * 3", "", 12);
    help_window_row(ui, font_id.clone(), color_segments, "pow(a, b) := a ^ b", "", 13);

    ui.label("To remove a custom function, simply leave out the right side of ':='.");

    ui.separator();
    ui.heading("Variables");
    ui.label("Builtin variables include 'pi', 'e' and 'tau':");
    help_window_row(ui, font_id.clone(), color_segments, "pi", "3.1415926536", 14);

    ui.label("You can also define your own custom variables:");
    help_window_row(ui, font_id.clone(), color_segments, "x := 3 * 4", "", 15);

    ui.label("To remove a custom variable, simply leave out the right side of ':='.");

    ui.label("The 'ans' variable always contains the result of the last calculation.");
    help_window_row(ui, font_id.clone(), color_segments, "3 * 4", "12", 16);
    help_window_row(ui, font_id.clone(), color_segments, "ans", "12", 17);

    ui.separator();
    ui.heading("Percentages");
    ui.label("The '%'-Sign is a shorthand for 'n / 100':");
    help_window_row(ui, font_id.clone(), color_segments, "20%", "0.2", 18);

    ui.label("The 'of' operator can be used to take a percentage from a value:");
    help_window_row(ui, font_id.clone(), color_segments, "20% of 100", "20", 19);
    help_window_row(ui, font_id.clone(), color_segments, "(10 + 10)% of 10 ^ 2", "20", 20);

    ui.separator();
    ui.heading("Equality checks");
    ui.label("The '='-Sign can be used to make a line an equality check:");
    help_window_row(ui, font_id.clone(), color_segments, "3 + 4 = 7", "True", 21);
    help_window_row(ui, font_id.clone(), color_segments, "log(2, 8) = 5", "False", 22);

    ui.separator();
    ui.heading("Units");
    ui.label("Units are number suffixes with an optional unit prefix (e.g. 'm', 'k', etc.).");
    ui.label("Numbers with and without units can be mixed in a single calculation.");
    ui.label("If the line consists only of a number with a unit, the unit is written in its long form.");
    help_window_row(ui, font_id.clone(), color_segments, "10m", "10 Meters", 23);
    help_window_row(ui, font_id.clone(), color_segments, "(10 + 10)min", "20min", 24);
    help_window_row(ui, font_id.clone(), color_segments, "20h + 30min", "20.5h", 25);

    ui.label("The 'in' operator can be used to convert between units:");
    help_window_row(ui, font_id.clone(), color_segments, "180min in h", "3h", 26);

    ui.label("The 'in' operator can also be used to convert the result to a different representation.");
    ui.label("However, this only has an affect when it is the last thing in a line.");
    help_window_row(ui, font_id.clone(), color_segments, "0xFF in decimal", "255 (decimal is default)", 27);
    help_window_row(ui, font_id.clone(), color_segments, "255 in hex", "0xFF", 28);
    help_window_row(ui, font_id.clone(), color_segments, "0x6 in binary", "0b110", 29);
    help_window_row(ui, font_id.clone(), color_segments, "255km in hex", "0xFF Kilometers", 30);

    ui.separator();
    ui.heading("Solving equations");
    ui.label("The calculator can solve linear equations if you put a question mark in them.");
    help_window_row(ui, font_id.clone(), color_segments, "3 + ? = 30", "27", 31);
    help_window_row(ui, font_id.clone(), color_segments, "(3 + ?) * 2 = 30", "12", 32);
    ui.label("The question mark has the following position restrictions:");
    ui.label("- It or its enclosing groups cannot be surrounded by a power sign");
    ui.label("- It can only be an argument of custom functions that are themselves linear");

    ui.add_space(5.0);
    ui.label("If you prefix the question mark with a variable name, its value will be assigned to that variable:");
    help_window_row(ui, font_id.clone(), color_segments, "(3 + x?) * 2 = 30", "12", 33);
    help_window_row(ui, font_id, color_segments, "x", "12", 34);
}
