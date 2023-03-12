# Documentation

## Literals

Literals can be specified using different representations.

```
3
0xFF
0b110
3.123
.123
```

### Scientific notation

Scientific notation can be used to input big numbers more easily.

```
1e2
3e5
1E-2
```

## Operators

### Basic

| Name           | Operator |
|----------------|----------|
| Addition       | `+`      |
| Subtraction    | `-`      |
| Multiplication | `*`      |
| Division       | `/`      |

### Extended

| Name                            | Operator     |
|---------------------------------|--------------|
| Exponentiation                  | `^`          |
| Bitwise AND                     | `&`          |
| Bitwise OR                      | `&#124;`     |
| Left Shift                      | `<<`         |
| Right Shift                     | `&#62;&#62;` |
| Modulo                          | `mod`        |
| Taking a percentage of a number | `of`         |
| Unit / format conversion        | `in`         |

### Modifiers

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

### Operator order

1. Functions and variables
2. Groups ("(...)")
3. Extended operators
4. Multiplication and division
5. Addition and subtraction
6. `of` and `in`

### Inferred multiplication

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

## Functions

### Standard Functions

By default, trigonometric functions use parameters in radians and their inverse functions also return values in radians.

| Description                   | Function                           | Example                  |
|-------------------------------|------------------------------------|--------------------------|
| Sine                          | sin                                | `sin(1)`                 |
| Arcsine                       | asin                               | `asin(.5)`               |
| Cosine                        | cos                                | `cos(1)`                 |
| Arccosine                     | acos                               | `acos(.5)`               |
| Tangent                       | tan                                | `tan(1)`                 |
| Arctangent                    | atan                               | `atan(.5)`               |
| Natural logarithm             | ln                                 | `ln(3)`                  |
| Logarithm                     | log(base, n)                       | `log(2, 8)`              |
| Square root                   | sqrt                               | `sqrt(25)`               |
| Cube root                     | cbrt                               | `cbrt(1000)`             |
| Root                          | root(index, n)                     | `root(2, 8)`             |
| Absolute value                | abs                                | `abs(-10)`               |
| Flooring                      | floor                              | `floor(3.5)`             |
| Ceiling                       | ceil                               | `ceil(20.2)`             |
| Clamping                      | clamp(n, start, end)               | `clamp(5, 0, 2)`         |
| Map from one range to another | map(n, start1, end1, start2, end2) | `map(5, 0, 10, 20, 100)` |
| Rounding                      | round                              | `round(5.2)`             |

### Custom functions

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

## Variables

## Constants

| Constant | Value        |
|----------|--------------|
| pi       | 3.1415926536 |
| e        | 2.7182818285 |
| tau      | 6.2831853072 |

### `ans` Variable

The `ans` variable contains the result of the previous calculation. It starts out at 0.

### Custom variables

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

## Equality checks

An equals sign ("=") marks this line as an equality check. funcially then returns `True` or `False`, depending on
whether
the two expressions evaluate to the same value (including units).

The equals sign **must** be at the top level and there can only be one equals sign in a line.

```
20 + 30 = 25 * 2
20 * 5 = 10
```

## Equation solving

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

## Units

Units are defined with a name and an optional *unit prefix*.

Units and unit prefixes are case-sensitive.

| Prefix | Value |
|--------|-------|
| `y`    | 1e-24 |
| `z`    | 1e-21 |
| `a`    | 1e-18 |
| `f`    | 1e-15 |
| `p`    | 1e-12 |
| `n`    | 1e-9  |
| `m`    | 1e-3  |
| `c`    | 1e-2  |
| `d`    | 1e-1  |
| `h`    | 1e2   |
| `k`    | 1e3   |
| `M`    | 1e6   |
| `G`    | 1e9   |
| `T`    | 1e12  |
| `P`    | 1e15  |
| `E`    | 1e18  |
| `Z`    | 1e21  |
| `Y`    | 1e24  |

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

### Currencies

funcially supports currencies from [exchangerates.host](https://exchangerates.host), like `EUR` or `USD`.
These can be used the same way as units. funcially updates the exchange rates on every startup and saves them
for when there is no internet connection available.

### "in" operator

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

## GUI

Other features:

- Saving: The input field's text is saved across restarts
- Syntax highlighting
- Installation with installers
- Available on Desktop and Web. The web version can be downloaded to be available offline, and can thus be used
  on e.g. tablets

### Comments

Calculations can be annotated using comments. Comments start with `//` and span the entire rest of the line.

```
// This is a comment spanning the entire line
3 + 4
sin(30)
20% of 100 // This gives more details about the calculation
```

### Plotting

The GUI supports plotting **single argument** functions. When declaring a single argument function,
a checkbox appears in the output column, allowing the function to be selected to show in the plot.
The plot can be brought up via the button in the top left ("Open/Close Plot").

If there are multiple re-declarations of a function, each re-declaration can be shown individually.

### Shortcuts

"Cmd" is `ctrl` on Windows and Linux and `⌘` on macOS.

| Description                         | Shortcut  |
|-------------------------------------|-----------|
| Comment/Uncomment selected lines    | Cmd+Alt+N |
| Surround selection with parentheses | Cmd+B     |
| Copy result of current line         | Cmd+⇧+C   |
| Format input text                   | Cmd+Alt+L |
