/*
 * Copyright (c) 2022-2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

use eframe::egui::*;
use eframe::egui::style::Margin;
use eframe::egui::text_edit::TextEditState;
use eframe::epaint::Shadow;
use egui_commonmark::{CommonMarkCache, CommonMarkViewer};

use calculator::{Calculator, SourceRange};

use crate::Line;

pub mod helpers;

macro_rules! storable {
    ($st:ident) => {
        impl $st {
            pub fn load(ctx: &Context, id: &str) -> $st {
                ctx.data_mut(|data| data
                    .get_temp(Id::new(id))
                    .unwrap_or_default())
            }

            pub fn store(self, ctx: &Context, id: &str) {
                ctx.data_mut(|data| data.insert_temp(Id::new(id), self))
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

pub fn dialog<R>(ctx: &Context, title: Option<&str>, add_contents: impl FnOnce(&mut Ui) -> R) -> R {
    let response = Window::new(title.unwrap_or(""))
        .title_bar(title.is_some())
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

    /// Returns whether something happened, and if something happened, whether the cursor was changed
    pub fn show(&mut self, ctx: &Context) -> Option<bool> {
        let mut state = LinePickerDialogState::load(ctx, LINE_PICKER_ID);

        let mut result: Option<bool> = None;

        if state.is_open {
            dialog(ctx, None, |ui| {
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
                        ui.fonts(|f| f.layout_job(job))
                    })
                    .show(ui);
                output.response.request_focus();

                let events = ui.input(|i| i.events.clone());
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
                stroke: Stroke::NONE,
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
                        let currencies = calculator.currencies.clone();
                        let settings = calculator.settings.clone();
                        let f = function.2.clone();

                        plot_ui.line(plot::Line::new(
                            plot::PlotPoints::from_explicit_callback(move |x| {
                                match env.resolve_specific_function(
                                    &f,
                                    &[(calculator::NumberValue::new(x), SourceRange::empty())],
                                    SourceRange::empty(),
                                    calculator::Context {
                                        env: &env,
                                        currencies: &currencies,
                                        settings: &settings,
                                    },
                                ) {
                                    Ok(v) => v.to_number()
                                        .map(|num| num.number)
                                        .unwrap_or(f64::NAN),
                                    Err(_) => f64::NAN,
                                }
                            }, .., 512)
                        ).name(&function.0));
                    }
                }
            }
        })
}

pub fn output_text(ui: &mut Ui, str: &str, font_id: FontId, index: usize) -> Response {
    let text: WidgetText = str.into();
    let valign = ui.layout().vertical_align();

    let mut text_job = text.into_text_job(
        ui.style(), FontSelection::FontId(font_id.clone()), valign,
    );
    text_job.job.wrap.max_width = f32::INFINITY;
    text_job.job.halign = Align::RIGHT;
    let galley = ui.fonts(|fonts| text_job.into_galley(fonts));

    let glyph_width = ui.fonts(|f| f.glyph_width(&font_id, '0'));
    let index = index.to_string();
    let index_str_width = index.len() as f32 * glyph_width;

    let (full_rect, response) = ui.allocate_exact_size(
        vec2(ui.available_width(), galley.size().y), Sense::click());

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
                        Stroke::NONE,
                    );
            }
        }

        let galley_length = galley.size().x;
        ui.painter()
            .with_clip_rect(text_max_rect)
            .galley_with_color(text_max_rect.right_top(), galley.galley, text_color);

        let mut show_copied_text = false;
        if response.clicked() && bg_rect.contains(response.hover_pos().unwrap()) {
            ui.output_mut(|out| out.copied_text = str.to_owned());
            show_copied_text = true;
        }

        let show_copied_label = ui.ctx().animate_bool_with_time(
            response.id.with("__copied_text_anim"),
            show_copied_text,
            2.0,
        ) != 0.0;

        if (galley_length >= text_max_rect.width() && response.hovered()) || show_copied_label {
            show_tooltip_at(
                ui.ctx(),
                response.id.with("__out_tooltip"),
                Some(full_rect.right_bottom()),
                |ui| {
                    if galley_length >= text_max_rect.width() && response.hovered() {
                        ui.label(str);
                    }
                    if show_copied_label {
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

pub fn build_help(ui: &mut Ui) {
    ui.horizontal(|ui| {
        ui.spacing_mut().item_spacing.x = 0.0;
        ui.label("This is a brief overview of the features. A more detailed documentation can be found ");
        ui.hyperlink_to("here", "https://github.com/david072/funcially/wiki");
        ui.label(".");
    });

    let markdown =
        r#"
# Literals

Literals can be specified using different representations.

```
3
0xFF
0b110
3.123
.123
```

## Scientific notation

Scientific notation can be used to input big numbers more easily.

```
1e2
3e5
```

# Objects

funcially supports using objects which encapsulate more complex data structures. Objects always follow the syntax:
`{<name> <args>}`.

The following objects can be used:

| Name | Syntax                                           |
|------|--------------------------------------------------|
| date | `{date now}` / `{date day.month.year}`<sup>[1](#date-footnote)</sup> |

---

<sup><a name="date-footnote">1</a></sup>: This format can be changed in the [settings](#Settings).

# Operators

## Basic

| Name           | Operator     |
|----------------|--------------|
| Addition       | `+`          |
| Subtraction    | `-`          |
| Multiplication | `*`          |
| Division       | `/`          |

## Extended

| Name                            | Operator     |
|---------------------------------|--------------|
| Exponentiation                  | `^`          |
| Bitwise AND                     | `&`          |
| Bitwise OR                      | `\|`          |
| Left Shift                      | `<<`         |
| Right Shift                     | `>>`         |
| Modulo                          | `mod`        |
| Taking a percentage of a number | `of`         |
| Unit conversion                 | `in`         |

## Modifiers

Modifiers are similar to operators, except that they only have one operand.

They change the value of what they were pre- or appended to.

| Name        | Modifier        | Long form               |
|-------------|-----------------|-------------------------|
| Percent     | `%`             | `n / 100`               |
| Bitwise NOT | `!` (prepended) | -                       |
| Factorial   | `!` (appended)  | `n * (n - 1) * ... * 1` |

```
20%
!0b101
5!
(50 / 2)%
```

## Operator order

1. Functions and variables
2. Groups ("(...)")
3. Extended operators
4. Multiplication and division
5. Addition and subtraction
6. `of` and `in`

## Inferred multiplication

The multiplication sign can be left out in the following scenarios:

- Before variables
- Before functions
- Before groups

```
2pi
2sin(30)
2(3 + 4)
(3 + 4)(1 + 2)
```

# Functions

## Standard Functions

Trigonometric functions use parameters in radians and their inverse functions also return values in radians.

To use degrees, simply convert the units:
```
sin(30°)
asin(.5) in °
```

| Description                        | Function                              | Example                  |
|------------------------------------|---------------------------------------|--------------------------|
| Sine                               | sin                                   | `sin(1)`                 |
| Arcsine                            | asin                                  | `asin(.5)`               |
| Cosine                             | cos                                   | `cos(1)`                 |
| Arccosine                          | acos                                  | `acos(.5)`               |
| Tangent                            | tan                                   | `tan(1)`                 |
| Arctangent                         | atan                                  | `atan(.5)`               |
| Cotangent                          | cot                                   | `cot(1)`                 |
| Arccotangent                       | acot                                  | `acot(.5)`               |
| Natural logarithm                  | ln                                    | `ln(3)`                  |
| Logarithm                          | log(base, n)                          | `log(2, 8)`              |
| Square root                        | sqrt                                  | `sqrt(25)`               |
| Cube root                          | cbrt                                  | `cbrt(1000)`             |
| Root                               | root(index, n)                        | `root(2, 8)`             |
| Absolute value                     | abs                                   | `abs(-10)`               |
| Flooring                           | floor                                 | `floor(3.5)`             |
| Lerping                            | lerp(a, b, t)                         | `lerp(0, 50, .5)`        |
| Ceiling                            | ceil                                  | `ceil(20.2)`             |
| Clamping                           | clamp(n, start, end)                  | `clamp(5, 0, 2)`         |
| Map from one range to another      | map(n, start1, end1, start2, end2)    | `map(5, 0, 10, 20, 100)` |
| Rounding (optional decimal places) | round(n) / round(n, decimal places)   | `round(5.2)`             |

## Custom functions

Syntax: `name(arg1, arg2, ...) := expr`

A custom functions is defined with a name, a set of arguments separated by commas and an expression that evaluates to
the function's value. In the expression, the names given to the arguments before the definition sign (`:=`) can be used.
As such, this expression cannot be evaluated right away.

If "expr" is empty, the function is removed.

When a function is re-declared (i.e. the function already exists), one of two things happens:

- If the function being declared is a standard function (e.g. `sin`), an error occurs, saying that standard functions
  cannot be overridden.
- Otherwise, the function's expression and arguments are updated.

The same applies when removing a function.

```
f(x) := x * 3
f(x, y) := x ^ y
f(x, y) :=
```

# Variables

## Constants

| Constant | Value        |
|----------|--------------|
| pi       | 3.1415926536 |
| e        | 2.7182818285 |
| tau      | 6.2831853072 |

## `ans` Variable

The `ans` variable contains the result of the previous calculation. It starts out at 0.

## Custom variables

Syntax: `name := expr`

A custom variable is defined with a name, and an expression that resolves to its value.
When the variable is declared, its expression is evaluated and the result (including an optional unit)
is stored for future use. Variable **do not** store formats.

If "expr" is empty, the variable is removed.

When a variable is re-declared (i.e. the variable already exists), one of two things happens:

- If the variable being declared is a builtin variable (e.g. `pi`), an error occurs, saying that standard
  variables cannot be overridden.
- Otherwise, the variable's value is updated.

The same applies when removing a variable.

```
x := 4 + 3
pi := 4   => Error: ReservedVariable
x := 20 * 2
x :=
```

# Equality checks

An equals sign ("=") marks this line as an equality check. funcially then returns `True` or `False`, depending on whether
the two expressions evaluate to the same value (including units).

The equals sign **must** be at the top level and there can only be one equals sign in a line.

```
20 + 30 = 25 * 2
20 * 5 = 10
```

# Equation solving

funcially can solve **linear** equations if there is a question mark (`?`) in either sides.

- There can only be one question mark in an equation.
- The question mark can only be a function argument of custom functions that are themselves linear.

If the question mark is preceded by a variable name, its calculated value will be assigned to the given
variable.

```
30 + ? = 100
? * sin(30) = 1 + 1
20 + x? = 100.5
```

# Units

Units are defined with a name and an optional *unit prefix*.

Units and unit prefixes are case-sensitive.

| Prefix | Name  | Value |
|--------|-------|-------|
| `n`    | Nano  | 1e-9  |
| `m`    | Milli | 1e-3  |
| `c`    | Centi | 1e-2  |
| `d`    | Deci  | 1e-1  |
| `h`    | Hecto | 1e2   |
| `k`    | Kilo  | 1e3   |
| `M`    | Mega  | 1e6   |
| `G`    | Giga  | 1e9   |
| `T`    | Tera  | 1e12  |
| `P`    | Peta  | 1e15  |
| `E`    | Exa   | 1e18  |
| `Z`    | Zetta | 1e21  |
| `Y`    | Yotta | 1e24  |

Unit prefixes can be used without a unit and thus act like a modifier (e.g. `3k = 3000`).

Numbers with and without units can be mixed, resulting in the unit being carried to the result.<br>
If the line consists **only** of a literal with a unit, the unit in the result is printed in it's long form. Otherwise,
the abbreviated version is used.

Two units can be combined using a `/` in between.

funcially will automatically convert units to perform operations if needed. In that case, the right hand
side's is converted into the left hand side's unit.

```
10m
10km/h
(10 + 10)km
20 * 2min
```

## Currencies

funcially supports currencies from [exchangerate.host](https://exchangerate.host), like `EUR` or `USD`.
These can be used the same way as units. funcially updates the exchange rates on every startup and saves them
for when there is no internet connection available.

## "in" operator

The `in operator` can be used to convert between units and formats.

Syntax: `<expr> in <dec/decimal/bin/binary/hex/sci/scientific> <unit>`
where **either** the format or the unit can be left out.

```
0xFF in decimal	    => 255 (default)
255 in hex		    => 0xFF
6 in binary	        => 0b110
255km in mi         => 158.4496540205mi
255km in sci mi	    => 1.58e2mi
```

# Settings

There are some settings, which allow the user to customize funcially to their preferences.

The settings can be accessed via `File>Settings` in the GUI, or via the `set` and `get` commands in the CLI.

## `set` and `get` commands

Syntax: `set <setting> = <value>`, `get <setting>`

A setting is always specified by a "path", delimited by dots.

For both, a question mark behind `<setting>` may be used to list available settings (e.g. `set ?` or `set date ?`).

# GUI

Other features:

- Saving: The input field's text is saved across restarts
- Syntax highlighting
- Installation with installers
- Available on Desktop and Web. The web version can be downloaded to be available offline, and can thus be used
  on e.g. Android tablets
- Searching

## Comments

Calculations can be annotated using comments. Comments start with `#` and span the entire rest of the line.

```
# This is a comment spanning the entire line
3 + 4
sin(30)
20% of 100 # This gives more details about the calculation
```

## Plotting

The GUI supports plotting **single argument** functions. When declaring a single argument function,
a checkbox appears in the output column, allowing the function to be selected to show in the plot.
The plot can be brought up via the button in the top right ("🗠 Plot").

If there are multiple re-declarations of a function, each re-declaration can be shown individually.

## Shortcuts

"Cmd" is `ctrl` on Windows and Linux and `⌘` on macOS.

| Description                         | Shortcut  |
|-------------------------------------|-----------|
| Comment/Uncomment selected lines    | Cmd+Alt+N |
| Surround selection with parentheses | Cmd+B     |
| Copy result of current line         | Cmd+⇧+C   |
| Format input text                   | Cmd+Alt+L |
| Search                              | Cmd+F     |
| Go to line                          | Cmd+G     |
"#;

    let mut cache = CommonMarkCache::default();
    CommonMarkViewer::new("viewer").show(ui, &mut cache, markdown);
}
