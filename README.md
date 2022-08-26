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

## TODO

- Complex units such as `km/h`
- Constructs such as the unit `W` being constructed from the units `V` and `A`