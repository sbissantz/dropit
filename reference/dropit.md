# Drop Items from a Psychometric Scale

Removes the weakest (or strongest) items from a psychometric scale by
either (a) minimizing the decrease in Cronbach’s alpha or (b) selecting
the smallest absolute CFA loadings. The function can operate on the full
data set or within user-defined column partitions.

## Usage

``` r
dropit(
  data = data.frame(),
  partition = NULL,
  n_drop = 1L,
  direction = c("tail", "head"),
  criterion = c("alpha", "lambda"),
  approach = c("oneshot", "greedy"),
  output_type = c("names", "subset", "both", "debug"),
  alpha_metric = c("raw_alpha", "std.alpha", "G6(smc)", "average_r", "S/N", "alpha se",
    "var.r", "med.r"),
  alpha_args = list(),
  measurement_model = NULL,
  target_factor = NULL,
  lambda_metric = c("est", "std", "std.lv", "std.nox", "std.all"),
  cfa_args = list(),
  verbose = TRUE
)
```

## Arguments

- data:

  A `data.frame` of item responses (rows = respondents, columns =
  items). Must have at least one row and one column. Non-`data.frame`
  inputs are coerced.

- partition:

  Optional character vector of length `ncol(data)` giving a partition
  label for each column. Dropping is performed independently within each
  partition.

- n_drop:

  Integer scalar. Number of items to remove in each partition.

- direction:

  Character string, `"tail"` (default) or `"head"`. `"tail"` drops the
  weakest items; `"head"` drops the strongest.

- criterion:

  Character string, `"alpha"` (Cronbach’s alpha) or `"lambda"` (CFA
  loadings).

- approach:

  Character string, `"oneshot"` (single pass) or `"greedy"` (iterative
  dropping and refitting).

- output_type:

  One of `"names"` (default), `"subset"`, `"both"`, or `"debug"`.

  - `"names"` – character vector of dropped item names.

  - `"subset"` – reduced data frame with dropped columns removed.

  - `"both"` – list with elements `names` and `subset`.

  - `"debug"` – as `"both"` but also includes all captured messages,
    warnings, and errors.

- alpha_metric:

  Character string. Cronbach’s alpha metric to optimise (passed to
  `psych::alpha$alpha.drop`).

- alpha_args:

  Named list of extra arguments for
  [`alpha`](https://rdrr.io/pkg/psych/man/alpha.html).

- measurement_model:

  Optional character string containing a
  [model.syntax](https://rdrr.io/pkg/lavaan/man/model.syntax.html)
  specification. If `NULL`, a single-factor model with all items loading
  on one latent factor is used.

- target_factor:

  Character scalar giving the name of the factor whose loadings
  determine item ranking when multiple factors are present.

- lambda_metric:

  Character string indicating which solution matrix to extract from
  [`inspect`](https://rdrr.io/pkg/lavaan/man/lavInspect.html) (e.g.,
  `"est"`, `"std"`, `"std.lv"`, `"std.nox"`, `"std.all"`).

- cfa_args:

  Named list of additional arguments passed to
  [`cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- verbose:

  Logical; if `TRUE` (default) prints a structured, color-formatted
  report of all messages, warnings, and errors captured during the run.

## Value

If `output_type != "debug"`, returns either a character vector, data
frame, or list depending on the specified output type. If
`output_type = "debug"`, returns a list with components:

- result:

  The main result (or `NULL` if an error occurred).

- warnings:

  Character vector of captured warnings.

- messages:

  Character vector of informational messages.

## Details

- Input validation is strict: missing or duplicated column names are
  fixed or rejected, and each partition must contain at least `n_drop`
  items.

- Arguments that are not applicable to the chosen method (e.g.,
  `alpha_args` when `criterion = "lambda"`) are detected automatically
  and reported once via an informational message.

- All messages, warnings, and errors from downstream calls are captured
  and printed together at the end when `verbose = TRUE`, formatted using
  the internal helper
  [`colormsg()`](https://sbissantz.github.io/dropit/reference/colormsg.md).

## Methods

- **Alpha** – ranks items using
  [`alpha`](https://rdrr.io/pkg/psych/man/alpha.html), dropping those
  whose removal least reduces Cronbach’s alpha (a measure of internal
  consistency that equals true reliability when essential
  tau-equivalence holds).

- **Lambda** – fits a one-factor CFA via
  [`cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html) and ranks items by
  absolute standardized loadings.

## See also

[`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html),
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html),
[`lavaan::inspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html),
and the internal helpers documented at
[miscutils](https://sbissantz.github.io/dropit/reference/miscutils.md).

## Examples

``` r
dat <- data.frame(
  i1 = c(1, 2, 3, 4, 5),
  i2 = c(2, 2, 3, 4, 4),
  i3 = c(1, 1, 2, 3, 4),
  i4 = c(4, 3, 2, 1, 1)
)

dropit(dat, n_drop = 1, verbose = FALSE)
#> [1] "i1"
```
