# Print Colored Message to Console

Prints a message to the console using ANSI color codes. Supports both
standard 16-color and extended 256-color modes, as well as bold styling.

## Usage

``` r
colormsg(txt, color_code = 32, bold = FALSE, newline = FALSE)
```

## Arguments

- txt:

  Character scalar. The message text to print.

- color_code:

  Integer or character. ANSI color code (e.g., `31` = red, `32` = green,
  `33` = yellow, `"38;5;244"` = gray256). Defaults to 32 (green).

- bold:

  Logical; if `TRUE`, applies bold formatting. Default is `FALSE`.

- newline:

  Logical; if `TRUE`, prints a newline after the message. Default is
  `FALSE`.

## Value

Invisibly returns `NULL`. Called for its side effect of printing colored
text to the console.
