# Report Ignored Arguments

Emits an informational message when the user supplied arguments that do
not apply to the chosen method (e.g. `cfa_args` with
`criterion = "alpha"`).

## Usage

``` r
check_ignored(usr_sup, ign_nms)
```

## Arguments

- usr_sup:

  Character vector of argument names the user supplied.

- ign_nms:

  Character vector of argument names not applicable here.

## Value

Invisibly `NULL`; called for the side effect of messaging.
