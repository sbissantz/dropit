# Warn About Ignored Arguments

Internal helper used to emit a message when user-specified arguments are
not applicable to the current method (e.g., alpha vs. lambda mode).

## Usage

``` r
check_ignored(usr_sup, ign_nms)
```

## Arguments

- usr_sup:

  Character vector of argument names that were explicitly supplied by
  the user.

- ign_nms:

  Character vector of argument names that are not applicable in the
  current context.

## Value

Invisibly returns `NULL`. Called for its side effect of printing a
message.

## Details

This function prints an informative message rather than a warning,
indicating that some arguments have been ignored intentionally.
