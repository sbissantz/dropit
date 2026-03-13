# Trim Leading and Trailing Newlines

Removes any leading and trailing newline (`\\n`) characters from a
character string. Useful for cleaning up collected messages or warnings
before printing.

## Usage

``` r
trim_newlines(x)
```

## Arguments

- x:

  Character vector or scalar to process.

## Value

A character vector with newlines removed from the start and end.
