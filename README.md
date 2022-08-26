# Calculator

An advanced calculator engine for calculating text inputs.

![Image](/media/image.png)

## Features

- Literals in decimal, hex and binary
    - The integer part of floats can be omitted
- Basic operators (`+ - * /`)
- Extended operators (`^`, Bitwise operators, ...)
- Groups and nested groups (`(2 + 2) * 2`)
- Functions (such as `sin` or `cos`)
- Variables
    - Builtin variables such as `pi` or `e`
    - `ans` variable, holding the result of the last computation
- Inferred multiplication before groups, functions and variables (`2pi`)
- Number modifiers (such as `!` for factorial or Bitwise NOT or `%` as a shorthand for `n / 100`)
- `of` operator for taking a percentage from a number
- Equality checks (`4 = 2 * 2`)
- Units
    - Unit suffixes such as (`m`, `lb` or `°C`)
    - Unit prefixes such as (`k` (Kilo), `g` (Giga), or `m` (Milli))
- `in` operator
    - Conversion between units
    - Conversion between representations (if last thing in line)

### Order of operations

1. Functions and variables
2. Groups
3. Extended operators
4. Multiplication and division
5. Addition and subtraction
6. `of` and `in`

## Building

1. Clone the project with `git clone https://github.com/david072/calculator`
2. `cd calculator`
3. Run the CLI version with `cargo run --bin cli` or the GUI version with `cargo run --bin gui`

While it should also work on Linux, it has only been tested on Windows and Mac so far.

## TODO

- Complex units such as `km/h`
- Constructs such as the unit `W` being constructed from the units `V` and `A`

## Contributing

### Issues

The simplest way to contribute is by using the application and reporting bugs and making feature requests.

For reporting a bug:

- Try to provide a concise explanation of what happened
- Make a minimal example that can be used to reproduce the bug / crash / etc. The smaller, the better, while still
  yielding the same result
- See if there already exists an issue with your bug! Duplicate issues only scatter information for no reason. If you
  have something to add to the existing issue, place it in a comment.
- If you're submitting a feature request, try to answer the following questions:
    - Does it need to be a completely new feature or can it be integrated into existing features?
    - How should the feature work from the perspective of the user?

Alternatively, you can, of course, also fork the project and hack on it yourself. After you're done with your feature,
create a PR, and it will be reviewed as soon as possible.