# axioma

An advanced scientific calculator working with text inputs.

![Image](/media/thumbnail.png)

## Downloading

- Desktop Version (Windows + Mac) available in the `Releases` Tab
- Web Version available [here](https://david072.github.io/axioma)!

## Features

- Desktop, Web (downloadable as mobile "app" (PWA)) and CLI
- GUI:
    - Input data is persisted across restarts
    - Plotting single argument functions
    - Syntax highlighting
    - Installation with installers
    - Shortcuts
- Literals in decimal, hex and binary
    - The integer part of floats can be omitted
- Basic operators (`+ - * /`)
- Extended operators (`^`, Bitwise operators, ...)
- Groups and nested groups (`(2 + 2) * 2`)
- Functions (such as `sin` or `cos`)
    - Custom Functions (`f(x) := 3x`)
- Variables
    - Builtin variables such as `pi` or `e`
    - `ans` variable, holding the result of the last computation
    - Custom variables (`x := 3 + 4`)
- Inferred multiplication before groups, functions and variables (`2pi`)
- Number modifiers (such as `!` for factorial or Bitwise NOT or `%` as a shorthand for `n / 100`)
- `of` operator for taking a percentage from a number
- Equality checks (`4 = 2 * 2`)
    - Supports convertible units being used (`60min = 1h`)
- Units
    - Unit suffixes such as (`m`, `lb` or `°C`)
    - Unit prefixes such as (`k` (Kilo), `g` (Giga), or `m` (Milli))
    - Supports units that are built from two units, delimited by a `/`. Conversions are supported (`3km/h`)
- `in` operator
    - Conversion between units
    - Conversion between representations (if last thing in line)
- Solving **linear** equations (`3 + ? = 30`)
    - Assign the result to a variable (`3 + x? = 30` => `x = 27`)
    - Supports units

### Order of operations

1. Functions and variables
2. Groups
3. Extended operators
4. Multiplication and division
5. Addition and subtraction
6. `of` and `in`

## Building

1. Clone the project with `git clone https://github.com/david072/axioma`
2. `cd axioma`
3. Run the CLI version with `cargo run -p cli`, the GUI version with `cargo run -p gui`

To pass arguments to the CLI, use `cargo run -p cli -- <args>`

## Contributing

You can contribute either by using the application and reporting issues and submitting feature requests,
or by forking the project and working on it yourself.
