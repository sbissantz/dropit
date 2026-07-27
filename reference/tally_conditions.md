# Condense Repeated Conditions into a Counted, Unique Set

A run may call the ranking engine many times — once per greedy round,
and again for every partition arm — so the same warning can surface
dozens of times. Reporting each occurrence buries the signal, but plain
deduplication hides how often a problem struck: a model that failed to
converge once and one that failed every round would read identically.
This keeps one line per distinct condition and appends a count when it
occurred more than once.

## Usage

``` r
tally_conditions(x)
```

## Arguments

- x:

  Character vector of collected condition messages.

## Value

A character vector of unique messages in order of first occurrence, each
suffixed with `(xN)` when it occurred `N > 1` times.

## Details

Conditions are flattened to a single line first: engine messages often
wrap across lines, so two textually identical conditions would otherwise
fail to match on line breaks alone.
