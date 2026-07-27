# Drop Items from a Psychometric Scale

Removes the weakest (or strongest) items from a psychometric scale by
either (a) minimizing the decrease in Cronbach’s alpha or (b) selecting
the smallest absolute CFA loadings. The function can operate on the full
data set or within user-defined column partitions.

## Usage

``` r
dropit(
  data = data.frame(),
  anchor = NULL,
  partition = NULL,
  n_drop = 1L,
  direction = c("tail", "head"),
  criterion = names(criterion_registry),
  approach = c("oneshot", "greedy"),
  alpha_metric = c("raw_alpha", "std.alpha", "G6(smc)", "average_r", "S/N", "alpha se",
    "var.r", "med.r"),
  alpha_args = list(),
  measurement_model = NULL,
  target_factor = NULL,
  lambda_metric = c("est", "std", "std.lv", "std.nox", "std.all"),
  cfa_args = list(),
  seed = NULL,
  checks = TRUE,
  trace = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A `data.frame` of item responses (rows = respondents, columns =
  items). Must have at least one row and one column. Non-`data.frame`
  inputs are coerced.

- anchor:

  Optional character vector of item names to protect from removal. These
  theoretical anchors will be shielded from the dropping algorithm,
  ensuring they remain in the final subset regardless of their
  statistical performance. Defaults to `NULL`.

- partition:

  Optional character vector of length `ncol(data)` giving a partition
  label for each column. Dropping is performed independently within each
  partition.

- n_drop:

  Integer scalar. The number of items to remove. Behavior depends on the
  `partition` argument: if `partition = NULL` (the default), this is the
  total number of items dropped from the full dataset. If `partition` is
  specified, this is the number of items dropped from *each* partition
  independently. Setting `n_drop = 0` acts as a safe no-operation,
  returning the original dataset unmodified.

- direction:

  Character string, `"tail"` (default) or `"head"`. `"tail"` drops the
  weakest items; `"head"` drops the strongest.

- criterion:

  Character string, `"alpha"` (Cronbach’s alpha) or `"lambda"` (CFA
  loadings).

- approach:

  Character string, `"oneshot"` (single pass) or `"greedy"` (iterative
  dropping and refitting).

- alpha_metric:

  Character string naming the column of `psych::alpha$alpha.drop` used
  to rank items. Defaults to `"raw_alpha"`.

  The two common choices answer different questions, and they can
  disagree:

  - `"raw_alpha"` is the reliability of the **raw sum** of item
    responses, computed from the covariance matrix. This is the
    reliability of the score an administered form actually produces, so
    it is usually the decision-relevant quantity when abbreviating an
    instrument that will be scored by summing or averaging raw
    responses.

  - `"std.alpha"` is the reliability of the **standardized** composite,
    computed from the correlation matrix, i.e. of a score formed after
    z-scoring each item.

  Because `"raw_alpha"` is variance-weighted, items that spread
  respondents more contribute more to it. That is appropriate for a
  raw-scored form — such items genuinely carry more of the total score
  variance — but it means the two metrics can rank items differently
  when item variances differ, even on a shared response scale. Pick the
  one that matches how the final form will be scored, and state the
  choice; do not switch metrics after inspecting results.

  Note that below three items no `alpha.drop` column is interpretable:
  with two items every item-total correlation is the same single
  correlation, so the items are formally indistinguishable and any
  ordering reflects an artefact rather than a real difference between
  items. `dropit()` warns in that case.

- alpha_args:

  Named list of extra arguments for
  [`alpha`](https://rdrr.io/pkg/psych/man/alpha.html). This is also
  where missing-data handling for the alpha criterion is set, via `use`
  (e.g. `use = "complete.obs"`) or `impute`; see the "Missing data"
  section.

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
  [`cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html). This is also where
  missing-data handling for the lambda criterion is set, via `missing`
  (e.g. `missing = "fiml"`); see the "Missing data" section.

- seed:

  Optional integer scalar. Sets the random seed for stochastic
  operations (e.g., CFA bootstrapping) to ensure reproducibility. The
  global RNG state is temporarily modified and safely restored upon
  exit. Defaults to `NULL`.

- checks:

  Logical; if `TRUE` (default) the advisory guards run — the
  boundary-tie, small-scale, and greedy-with-missing-data warnings. Set
  `FALSE` in a simulation loop, once you have validated the design, to
  skip them and avoid re-emitting the same advisories on every
  iteration. Note that `verbose = FALSE` does **not** do this: it only
  hides the printed summary, while the guards still run and still
  collect into `$log`. Hard input validation is unaffected by `checks`
  and always runs.

- trace:

  Logical; if `TRUE`, records what the machinery did internally into
  `$log$messages` — one entry per greedy round, naming the items still
  in play (for `criterion = "lambda"`, the model syntax actually
  fitted). Defaults to `FALSE`, since a greedy run over several
  partitions produces one entry per round per arm. Only
  `approach = "greedy"` has a history to record; one-shot fits a single
  model. Distinct from `verbose`, which governs the printed end-of-run
  summary rather than what is collected.

- verbose:

  Logical; if `TRUE` (default) prints a structured, color-formatted
  report of all messages, warnings, and errors captured during the run.

## Value

An object of class `dropit`, which is a list containing:

- names:

  A character vector of dropped item names (or a named list of vectors
  if `partition` is used).

- subset:

  The reduced `data.frame` (or a named list of `data.frame`s if
  `partition` is used).

- log:

  A list containing `warnings` and `messages` captured during execution.

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

## Missing data

`dropit()` never deletes respondents itself. Missing values are handled
by the ranking engine, through its own native arguments, so the
treatment is visible in the call rather than hidden behind a wrapper
option: pass `use` or `impute` via `alpha_args` for the alpha criterion,
and `missing` (for example `missing = "fiml"`) via `cfa_args` for the
lambda criterion. The two engines default differently —
[`alpha`](https://rdrr.io/pkg/psych/man/alpha.html) to pairwise
deletion, [`cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html) to listwise —
so a ranking's sample follows whichever engine the chosen criterion
uses. To compare alpha and lambda on one identical sample, set matching
options in both lists (or restrict `data` to complete cases before
calling).

Under `approach = "greedy"` each round refits on a smaller set of items,
so with incomplete data the sample can shift from round to round as the
engine re-derives it. `dropit()` warns when this combination is
requested (unless `checks = FALSE`). If a fixed sample across rounds
matters, choose your missing-data method beforehand — for instance by
restricting `data` to complete cases before the call.

## See also

[`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html),
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html),
[`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html).

## Examples

``` r
dat <- data.frame(
  i1 = c(1, 2, 3, 4, 5),
  i2 = c(2, 2, 3, 4, 4),
  i3 = c(1, 1, 2, 3, 4),
  i4 = c(4, 3, 2, 1, 1)
)

dropit(dat, n_drop = 1, verbose = FALSE)
#> Dropped Items: 
#> [1] "i4"
#> 
#> Subset(s): 
#> 'data.frame':    5 obs. of  3 variables:
#>  $ i1: num  1 2 3 4 5
#>  $ i2: num  2 2 3 4 4
#>  $ i3: num  1 1 2 3 4
#> ------------ 
#> Run ended with 2 warning(s) and 1 message(s) logged. Access via `$log`
```
