#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

extern crate eframe;
extern crate calculator;

use std::collections::HashMap;
use eframe::{egui, Frame};
use egui::*;
use calculator::{Calculator, CalculatorResultData, Format, round_dp, Verbosity, Segment};
use std::ops::Range;

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
    color_segments: HashMap<usize, Vec<Segment>>,
    error_ranges: HashMap<usize, Range<usize>>,

    first_frame: bool,
}

impl Default for App {
    fn default() -> App {
        App {
            calculator: Calculator::new(),
            source_old: String::new(),
            source: String::new(),
            output: String::new(),
            color_segments: HashMap::new(),
            error_ranges: HashMap::new(),
            first_frame: true,
        }
    }
}

impl App {
    fn calculate(&mut self, line: usize, str: &str) -> String {
        let str = str.to_string();
        let str = str.trim();
        if str.is_empty() { return String::new(); }

        let result = self.calculator.calculate(str, Verbosity::None);
        match result {
            Ok(res) => {
                self.color_segments.insert(line, res.color_segments);
                self.error_ranges.remove(&line);

                match res.data {
                    CalculatorResultData::Number { result, unit, format } => {
                        let unit = unit.unwrap_or_default();
                        match format {
                            Format::Decimal => format!("= {}{}", round_dp(result, 10), unit),
                            Format::Binary => format!("= {:#b}{}", result as i64, unit),
                            Format::Hex => format!("= {:#X}{}", result as i64, unit),
                        }
                    }
                    CalculatorResultData::Boolean(b) => (if b { "True" } else { "False" }).to_string(),
                }
            }
            Err(e) => {
                self.color_segments.remove(&line);
                self.error_ranges.insert(line, e.start..e.end);
                format!("{:?}", e.error)
            }
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

                    let color_segments = &mut self.color_segments;
                    let error_ranges = &mut self.error_ranges;
                    let mut layouter = |ui: &Ui, string: &str, wrap_width: f32| {
                        let layout_section = |range: Range<usize>, color: Color32| {
                            text::LayoutSection {
                                leading_space: 0.0,
                                byte_range: range,
                                format: TextFormat {
                                    font_id: font_id.clone(),
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

                                    job.sections.push(layout_section(end..range_start, Color32::GRAY));
                                    job.sections.push(layout_section(range_start..range_end, Color32::RED));
                                    end = range_end;
                                } else if let Some(segments) = color_segments.get(&i) {
                                    for segment in segments {
                                        let range_start = segment.range.start + offset;
                                        let range_end = segment.range.end + offset;
                                        if range_end > string.len() { break 'outer; }
                                        if range_start != end {
                                            job.sections.push(layout_section(end..range_start, Color32::GRAY));
                                        }

                                        job.sections.push(layout_section(range_start..range_end, segment.color));
                                        end = range_end;
                                    }
                                }

                                offset += line.len() + 1;
                            }

                            if end != string.len() {
                                job.sections.push(layout_section(end..string.len(), Color32::GRAY));
                            }
                        }

                        job.wrap.max_width = wrap_width;
                        ui.fonts().layout_job(job)
                    };

                    let output = TextEdit::multiline(&mut self.source)
                        .lock_focus(true)
                        .hint_text("Calculate something")
                        .frame(false)
                        .font(FontSelection::from(font_id.clone()))
                        .desired_rows(rows)
                        .layouter(&mut layouter)
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

                            let mut line_index = 0usize;
                            for row in &output.galley.rows {
                                newlines.push('\n');
                                line += row.glyphs.iter().map(|g| g.chr).collect::<String>().as_str();

                                if row.ends_with_newline {
                                    output_str = format!("{}{}{}", output_str, self.calculate(line_index, &line), newlines);
                                    newlines.clear();
                                    line.clear();
                                    line_index += 1;
                                }
                            }

                            if !line.is_empty() {
                                let res = self.calculate(line_index, &line);
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
