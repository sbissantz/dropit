# Warn About Ignored Arguments

Internal helper used to emit a message when user-specified arguments are
not applicable to the current method (e.g., alpha vs. lambda mode).

## Usage

``` r
check_ignored(usr_sup, ign_nms)
```

## Arguments

- user_supplied:

  Character vector of argument names that were explicitly supplied by
  the user.

- ignored_names:

  Character vector of argument names that are not applicable in the
  current context.

## Value

Invisibly returns `NULL`. Called for its side effect of printing a
message.

## Details

This function prints an informative message rather than a warning,
indicating that some arguments have been ignored intentionally.

## Examples

``` r
check_ignored(c("alpha_args", "lambda_metric"), c("lambda_metric"))
#> Error in check_ignored(c("alpha_args", "lambda_metric"), c("lambda_metric")): could not find function "check_ignored"
# Prints: Argument(s) 'lambda_metric' not applicable and ignored.
```
