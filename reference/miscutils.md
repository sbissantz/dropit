# Internal Utility Functions

A collection of internal helper functions used across the package for
message formatting, console output, and argument checking.

## Details

These utilities are **not exported** and are primarily intended for
internal use in developer-facing code such as
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md).

Some consoles (e.g., RGui on Windows) may not support ANSI colors. For
full color compatibility, use RStudio, VS Code, or a Unix terminal.

## Included Helpers

- [`colormsg()`](https://sbissantz.github.io/dropit/reference/colormsg.md):

  Prints colored and optionally bold messages to the console using ANSI
  escape codes.

- [`trim_newlines()`](https://sbissantz.github.io/dropit/reference/trim_newlines.md):

  Removes leading and trailing newline characters from a string.

- [`check_ignored()`](https://sbissantz.github.io/dropit/reference/check_ignored.md):

  Emits an informational message when arguments that are not applicable
  to the current method have been provided by the user.
