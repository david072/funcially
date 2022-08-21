# Literals

Literals can be declared using different representations.

```
3       => 3
0xFF    => 255
0b110   => 6
0o1     => 10
```

## Floats

"." is used as a delimiter; the integer part can be omitted.

```
3.123   => 3.123
.123	=> 0.123
```

# Operators

## Basic

Addition (+), Subtraction (-), Multiplication (*) and Division (/)

```
3 + 4		    => 7
3 + 4 * 2	    => 11
4 - 2	        => 2
4 / 2		    => 2
```

## Extended operators + bitwise operators

Exponentiation (^) <br>
Bitwise AND (&), Bitwise OR (|) [^1] <br>
Factorial (*number*!) and Bitwise NOT [^1] (!*number*)

```
10 ^ 2 	                    => 100
10 ^ 2 * 4	(= 100 * 4)     => 400
5!	    			        => 120
```

[^1]: Integers only

## Operator order

(Higher is earlier evaluated)

1. Functions
2. Groups ("(...)")
3. Extended operators
4. Multiplication and division
5. Addition and subtraction
6. "of" and "in"

## Inferred multiplication

The multiplication sign can be left out in the following scenarios:

- Before variables
- Before functions
- Before groups

```
2pi		    (= 2 * pi)		    => 6.283185 (...)
2 pi		(= 2 * pi)			=> 6.283185 (...)
2sin(30)	(= 2 * sin(30))		=> 1
2(3 + 4)	(= 2 * (3 + 4)		=> 14
```

# Groups

Groups are formed with "(" and ")" and are thus evaluated first. They can be nested.

```
(3 + 4) * 2 	        => 14
((3 + 3) / 2) * 3	    => 9
```

# Functions

Functions are defined with a name, and arguments surrounded by brackets "()" and separated by commas ",". Arguments can
be expressions. <br>
Syntax:

```
name(argument 1, argument 2, ...)
```

## Builtin Functions

Builtin functions include (but are not limited to) `sin`, `cos`, `log`, ...

```
sin(30) 							    => 0.5
cos(60)							        => 0.5
cos(30 * 2)						        => 0.5
```

# Variables

Variables are defined with a name and resolve to a value during evaluation.

# Builtin Variables

Builtin variables include (but are not limited to) `pi`, `e`, ...

```
pi	       => 3.14159265 (...)
e		   => <whatever>
```

# Percentages

Percentages are a short form for `number / 100`.

Syntax: `number%`

The percent sign can be applied after groups, making the result of the group the number.
Percentages and non-percentages can additionally be mixed in calculations.

```
20%	        => 0.2
10 * 2%     => 0.2
(25 * 2)%   => 0.5
```

## "of" operator

Syntax: `percentage of expression`, where *expression* can be anything, even other percentages and units.

It "of" operator takes *percentage* of the value of *expression*. If *expression* evaluates to a unit, the percentage is
taken from the unit-number and the unit is preserved to the output.

Because it is evaluated last ([Operator order](#operator-order)) brackets can be omitted from either side.

```
20% of 100		        => 20
(10 + 10)% of 10 ^ 2    => 20
10% of 100km			=> 20km
```

# Equality checks

An equals sign ("=") marks this line as an equality check. This returns `True` or `False`, depending on whether the two
expressions evaluate to the same result.

The equals sign **must** be at the top level and there can only be one equals sign in a line.

```
20 + 30 = 25 * 2	    => True
20 * 5 = 10		        => False
```

# Units

Syntax: `<number><prefix><unit>`

Units are defined with a name and an optional *unit prefix*. Unit prefixes correspond
to [Metric prefixes](https://en.wikipedia.org/wiki/Metric_prefix#List_of_SI_prefixes).

Numbers with and without units can be mixed, resulting in the unit being carried to the result.

If the line consists **only** of a literal with a unit, the unit in the result is printed in it's long form. Otherwise,
the abbreviated version is used.

Complex units (e.g. `km/h`) are included.

```
10m			       => 10 Meters
10km/h		       => 10 Kilometers per Hour
(10 + 10)km	       => 20km
20 * 2min	       => 40min
```

## "in" operator

Syntax: `<number><unit> in unit`

The "in" operator converts between units.

Conversion is inferred when numbers with different units are used in other operations. Left hand side's unit is the
result.

```
10m in km 		    => 0.01km
60km/h in km/min    => 1km/min
10min + 1h		    => 70min
```

### Converting between representations with "in"

Syntax: `<expr> in <decimal/binary/hex/octal>`

The "in" operator can convert numbers to different representations. `decimal` is the default.

This is valid everywhere in a line, but only has an effect if it is at the end.

If the left hand side has a unit, the unit's number is converted and the unit is kept.

```
0xFF in decimal	    => 255 (default)
255 in hex		    => 0xFF
6 in binary	        => 0b110
10 in octal	        => 0o12
10km in octal	    => 0o12km
```