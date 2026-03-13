# Internal Helper Functions for Item Dropping

Low-level utilities used internally by
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md) to
implement the different item–removal strategies. These functions are
**not** intended for direct use by end-users but are documented for
developers who may wish to extend or debug the algorithm.

## Usage

``` r
naivedrop(
  dta,
  n_drp,
  dir,
  crt,
  apr,
  out,
  alp_mtr,
  alp_args,
  mmt_mdl,
  tgt_fct,
  lam_mtr,
  cfa_args
)

greedydrop_lambda(dta, n_drp, dir, out, mmt_mdl, tgt_fct, lam_mtr, cfa_args)

greedydrop_alpha(dta, n_drp, dir, out, alp_mtr, alp_args)

oneshotdrop_lambda(dta, n_drp, dir, out, mmt_mdl, tgt_fct, lam_mtr, cfa_args)

oneshotdrop_alpha(dta, n_drp, dir, out, alp_mtr, alp_args)
```

## Arguments

- dta:

  A `data.frame` of item responses (rows = respondents, columns =
  items).

- n_drp:

  Integer scalar giving the number of items to remove.

- dir:

  Character string, either `"tail"` or `"head"`, passed to
  [`utils::tail()`](https://rdrr.io/r/utils/head.html) /
  [`utils::head()`](https://rdrr.io/r/utils/head.html) to select the
  weakest or strongest items.

- crt:

  Character string, `"alpha"` or `"lambda"`, indicating the ranking
  criterion.

- apr:

  Character string, `"oneshot"` or `"greedy"`, selecting the removal
  strategy.

- out:

  Character string, one of `"names"`, `"subset"`, or `"both"`,
  determining the type of object returned.

- alp_mtr:

  Character string naming the column of `psych::alpha$alpha.drop` used
  for ranking.

- alp_args:

  Named list of additional arguments passed to
  [`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html).

- mmt_mdl:

  Character string of lavaan model syntax for the CFA. If `NULL`, a
  single-factor model is created automatically.

- tgt_fct:

  Character scalar giving the name of the target latent factor used for
  ranking. Required when the fitted CFA has multiple factors.

- lam_mtr:

  Character string passed to
  [`lavaan::inspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
  selecting which standardized solution matrix to extract (e.g.,
  `"std"`).

- cfa_args:

  Named list of additional arguments passed to
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

## Value

Depending on `out`:

- `"names"` – character vector of dropped item names.

- `"subset"` – reduced `data.frame` with the dropped items removed.

- `"both"` – list with elements `names` and `subset`.

## Details

- Alpha methods call
  [`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html) with
  `check.keys = TRUE`.

- Lambda methods call
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) (defaulting
  to `std.lv = TRUE` unless overridden) and extract the `"lambda"`
  matrix from
  [`lavaan::inspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html).

- All helpers validate that the number of extracted loadings matches the
  number of columns in `dta` and throw an error otherwise.

## Helper Overview

- naivedrop():

  Dispatch wrapper that calls the correct algorithm based on `criterion`
  (`"alpha"` or `"lambda"`) and `approach` (`"oneshot"` or `"greedy"`).

- greedydrop_alpha():

  Iterative (“greedy”) removal using Cronbach’s alpha.

- oneshotdrop_alpha():

  Single–pass removal using Cronbach’s alpha.

- greedydrop_lambda():

  Iterative (“greedy”) removal using CFA factor loadings.

- oneshotdrop_lambda():

  Single–pass removal using CFA factor loadings.

## See also

[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md),
[`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html),
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)
