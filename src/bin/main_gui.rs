#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

extern crate eframe;
extern crate calculator;

use eframe::{egui, Frame};
use egui::*;
use calculator::{Calculator, CalculatorResult, Format, round_dp, Verbosity};

const FONT_SIZE: f32 = 16.0;
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
    calculator: Calculator,

    source_old: String,
    source: String,
    output: String,

    first_frame: bool,
}

impl Default for App {
    fn default() -> App {
        App {
            calculator: Calculator::new(),
            source_old: String::new(),
            source: String::new(),
            output: String::new(),
            first_frame: true,
        }
    }
}

impl App {
    fn calculate(&mut self, str: &str) -> String {
        let str = str.to_string();
        let str = str.trim();
        if str.is_empty() { return String::new(); }

        let result = self.calculator.calculate(str, Verbosity::None);
        match result {
            Ok(res) => {
                match res {
                    CalculatorResult::Number { result, unit, format } => {
                        let unit = unit.unwrap_or_default();
                        match format {
                            Format::Decimal => format!("= {}{}", round_dp(result, 10), unit),
                            Format::Binary => format!("= {:#b}{}", result as i64, unit),
                            Format::Hex => format!("= {:#X}{}", result as i64, unit),
                        }
                    }
                    CalculatorResult::Boolean(b) => (if b { "True" } else { "False" }).to_string(),
                }
            }
            Err(e) => format!("{:?}", e.error), // TODO: Highlight error in red
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &Context, _frame: &mut Frame) {
        CentralPanel::default().show(ctx, |ui| {
            let rows = ((ui.available_height() - TEXT_EDIT_MARGIN.y) / FONT_SIZE) as usize;

            ScrollArea::vertical().show(ui, |ui| {
                ui.horizontal(|ui| {
                    let font_id = FontId::monospace(FONT_SIZE);

                    // TODO: Make this fill the whole screen on startup
                    let output = TextEdit::multiline(&mut self.source)
                        .lock_focus(true)
                        .hint_text("Calculate something")
                        .frame(false)
                        .font(FontSelection::from(font_id.clone()))
                        .desired_rows(rows)
                        .show(ui);

                    if self.first_frame {
                        self.first_frame = false;
                        ui.ctx().memory().request_focus(output.response.id);
                    }

                    if self.source != self.source_old {
                        self.source_old = self.source.clone();

                        if !output.galley.rows.is_empty() && !output.galley.rows[0].glyphs.is_empty() {
                            let mut output_str = String::new();
                            let mut line = String::new();
                            let mut newlines = String::new();

                            for row in &output.galley.rows {
                                if row.glyphs.is_empty() { continue; }

                                newlines.push('\n');
                                line += row.glyphs.iter().map(|g| g.chr).collect::<String>().as_str();

                                if row.ends_with_newline {
                                    output_str = format!("{}{}{}", output_str, self.calculate(&line), newlines);
                                    newlines.clear();
                                    line.clear();
                                }
                            }

                            if !line.is_empty() {
                                let res = self.calculate(&line);
                                output_str = format!("{}{}{}", output_str, res, newlines);
                            }

                            self.output = output_str;
                        } else {
                            self.output.clear();
                        }
                    }

                    spacer(ui);
                    TextEdit::multiline(&mut self.output)
                        .interactive(false)
                        .frame(false)
                        .font(FontSelection::from(font_id))
                        .desired_rows(rows)
                        .show(ui);
                });
            });
        });
    }
}

fn spacer(ui: &mut Ui) -> Response {
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
