# Print a Colored Message to the Console

Prints a message using ANSI color codes. Supports the standard 16-color
and extended 256-color modes, plus bold styling. Some consoles (e.g.
RGui on Windows) do not support ANSI colors.

## Usage

``` r
colormsg(txt, color_code = 32, bold = FALSE, newline = FALSE)
```

## Arguments

- txt:

  Character scalar. The message text to print.

- color_code:

  Integer or character. ANSI color code (e.g., `31` = red, `32` = green,
  `"38;5;244"` = gray256). Defaults to 32.

- bold:

  Logical; if `TRUE`, applies bold formatting. Default `FALSE`.

- newline:

  Logical; if `TRUE`, prints a newline after the message. Default
  `FALSE`.

## Value

Invisibly `NULL`; called for the side effect of printing.
