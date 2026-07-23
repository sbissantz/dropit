# Tools for Reproducible Psychometric Experimentation

``` r

library(dropit)
library(psych)
```

We will use the BFI data from the `psych` package to illustrate the
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
function for scale modifications. The BFI is a personality inventory
that measures the Big Five traits: agreeableness, conscientiousness,
extraversion, neuroticism, and openness. Each trait is assessed with 5
items, resulting in a total of 25 items.

``` r

# Load BFI data
dta <- psych::bfi

# Get the position of (non) BFI items
xcld <- c("gender", "education", "age") 
xcld_pat <- colnames(dta) %in% xcld

# Store the data in a matrix
bfi <- as.matrix(dta[, !xcld_pat])

# Number of respondents
N <- nrow(bfi)

# Number of items
I <- ncol(bfi)

# Labels
rownames(bfi) <- paste0("p",seq(N))

# Number of dimensions
D <- 5
```

## Assignment

The Q-matrix is a binary matrix that specifies which items measure which
construct(s). Here, it helps us define which BFI items belong to which
domain. A nice feature of this representation is that it allows easy
computation of sum scores for each domain by matrix multiplication.

``` r

Q <- matrix(0, nrow = I, ncol = D) 
rownames(Q) <- colnames(bfi)
colnames(Q) <- c("a", "c", "e", "n", "o")

Q[1:5, "a"]   <- 1 # Agreeableness (A1-A5)
Q[6:10, "c"]  <- 1 # Conscientiousness (C1-C5)
Q[11:15, "e"] <- 1 # Extraversion (E1-E5)
Q[16:20, "n"] <- 1 # Neuroticism (N1-N5)
Q[21:25, "o"] <- 1 # Openness (O1-O5)

# Sumscores (original)
bfi %*% Q |> head()
#>     a  c  e  n  o
#> p1 17 16 17 14 19
#> p2 18 20 15 19 16
#> p3 22 20 19 18 18
#> p4 24 21 20 14 18
#> p5 17 18 18 16 16
#> p6 28 22 20 15 19
```

## Extraversion Domain

For now, let’s focus on the Extraversion items:

``` r

# Extraversion items
extra <- bfi[,Q[,"e"] == 1]
extra |> head()
#>    E1 E2 E3 E4 E5
#> p1  3  3  3  4  4
#> p2  1  1  6  4  3
#> p3  2  4  4  4  5
#> p4  5  3  4  4  4
#> p5  2  2  5  4  5
#> p6  2  1  6  5  6
```

## Drop It!

What happens when we drop the two worst-performing items from the
extraversion scale in a single pass based on their alpha values? The
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
function allows us to explore that question.

``` r

drop2ga <- dropit(
  data = extra,
  n_drop = 2L,
  direction = "tail",
  criterion = "alpha",
  approach = "oneshot",
  alpha_metric = c("raw_alpha"),
  alpha_args = list(check.keys = TRUE),
  verbose = FALSE
)

# Manifest correlation between sum scores
cor(rowSums(extra), rowSums(drop2ga$subset), use = "pairwise.complete.obs")
#> [1] 0.6150422
```

## The Dropping Criterion: Alpha vs. Lambda

With the `criterion` argument, the user specifies the metric to score
items. The two options are:

- **`criterion = "alpha"` (Internal Consistency):** This approach relies
  on [`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html). The
  algorithm drops the item whose removal *least* reduces (or most
  increases) the scale’s overall Cronbach’s alpha.

- **`criterion = "lambda"` (Latent Variable Modeling):** This approach
  relies on [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).
  It fits a one-factor Confirmatory Factor Analysis model and ranks
  items based on their absolute standardized factor loadings
  ($`\lambda`$). It drops the item with the weakest (or strongest)
  relationship to the underlying trait.

Let’s compare dropping a single item from our extraversion scale using
both methods:

``` r

# Drop weakest item based on Cronbach's alpha
drop_alpha <- dropit(
  data = extra,
  n_drop = 1L,
  criterion = "alpha",
  verbose = FALSE
)

# Drop weakest item based on CFA factor loadings
drop_lambda <- dropit(
  data = extra,
  n_drop = 1L,
  criterion = "lambda",
  verbose = FALSE
)

cat("Alpha:", drop_alpha$names, "\n")
#> Alpha: E2
cat("Lambda:", drop_lambda$names, "\n")
#> Lambda: E5
```

## Greedy vs. One-Shot

When dropping multiple items (`n_drop > 1`), the approach used to select
them becomes more critical. `dropit` offers two approaches:

- **`approach = "oneshot"`:** The algorithm evaluates the full scale,
  ranks all items simultaneously, and drops the bottom $`n`$ items in a
  single pass.
- **`approach = "greedy"`:** The algorithm evaluates the scale and drops
  only the single worst item. It then *refits* the alpha or CFA model on
  the remaining items, recalculates the rankings, and drops the next
  “worst” item. This repeats $`n`$ times.

Why does this matter? Well let’s compare the two approaches when
dropping 3 items from our extraversion scale using CFA loadings:

``` r

# One-shot dropping
drop3_oneshot <- dropit(
  data = extra,
  n_drop = 3L,
  criterion = "lambda",
  approach = "oneshot",
  verbose = FALSE
)

# Greedy dropping
drop3_greedy <- dropit(
  data = extra,
  n_drop = 3L,
  criterion = "lambda",
  approach = "greedy",
  verbose = FALSE
)

cat("One-shot:", paste(drop3_oneshot$names, collapse = ", "), "\n")
#> One-shot: E1, E3, E5
cat("Greedy:", paste(drop3_greedy$names, collapse = ", "), "\n")
#> Greedy: E5, E3, E4
```

## Adversarial Modifications

While scale abbreviation typically aims to maximize retained information
by dropping the weakest items (`direction = "tail"`), `dropit` also
allows you to establish worst-case scenarios. Setting
`direction = "head"` forces the algorithm to drop the *strongest*
items—those with the highest absolute CFA loadings or whose removal most
severely reduces Cronbach’s alpha.

``` r

# Maximized abbreviation (lowest)
bestcase <- dropit(
  data = extra, 
  n_drop = 2, 
  criterion = "lambda", 
  direction = "tail", 
  verbose = FALSE
)

# Adversarial abbreviation (highest)
worstcase <- dropit(
  data = extra, 
  n_drop = 2, 
  criterion = "lambda", 
  direction = "head", 
  verbose = FALSE
)

cat("Best-case:", paste(bestcase$names, collapse = ", "), "\n")
#> Best-case: E3, E5
cat("Worst-case:", paste(worstcase$names, collapse = ", "), "\n")
#> Worst-case: E2, E4
```

## Retaining Facets

Most domains in modern psychological inventories are multifaceted. If we
apply an abbreviation algorithm to the entire dimension at once, it
might disproportionately decimate one facet while leaving another
completely untouched, destroying the theoretical structure of the scale.

The `partition` argument solves this by applying the dropping algorithm
independently across user-defined subscales. Since the built-in `bfi`
dataset does not specify official extraversion facets, let’s arbitrarily
divide our five items into two imaginary sub-components: “S” (E1, E2,
E3) and “A” (E4, E5). We can then instruct
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md) to
drop the single weakest item from each facet simultaneously.

``` r

# Arbitrarily assign: 5 extraversion items to 2 facets
fcts_extra <- c("S", "S", "S", "A", "A")
# Drop 1 item from each facet
drop_multi <- dropit(
  data = extra,
  partition = fcts_extra,
  n_drop = 1L,
  direction = "tail",
  criterion = "alpha",
  approach = "oneshot",
  alpha_metric = "raw_alpha",
  alpha_args = list(check.keys = TRUE),
  verbose = FALSE
)
# Result is now organized by facet
drop_multi$names
#> $A
#> [1] "E4"
#> 
#> $S
#> [1] "E3"
```

## Anchoring Items

Researchers often need to retain specific items to ensure strict
comparability against an established baseline. The `anchor` argument
allows you to define a set of items that are immune to dropping. Let’s
assume items `E2` and `E4` are theoretically vital to the extraversion
scale. However, in the previous run, the algorithm dropped them. By
setting them as anchors, we ensure they remain protected.

``` r

drop2ga_anc <- dropit(
  data = extra,
  anchor = c("E2", "E4"),
  n_drop = 2L,
  direction = "tail",
  criterion = "alpha",
  approach = "oneshot",
  alpha_metric = "raw_alpha",
  alpha_args = list(check.keys = TRUE),
  verbose = FALSE
)$names

# Compare dropped items against unanchored baseline
cat("Unanchored:", paste0(drop2ga$names, collapse = ", "), "\n")
#> Unanchored: E3, E5
cat("Anchored:", paste0(drop2ga_anc, collapse = ", "), "\n")
#> Anchored: E3, E5
```

## Missing Data

Missing data is a common issue in psychometric datasets, and how it is
handled can significantly impact the results of item dropping
procedures.
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
never deletes respondents on your behalf. Instead, it hands the missing
values straight to the ranking engine and lets you steer the treatment
through that engine’s own arguments: `alpha_args` for
[`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html) and
`cfa_args` for
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html). The choice
therefore stays visible in the call rather than hidden behind a wrapper
option—and, because the two engines are configured through their own
native arguments, it is plain to any reader that the alpha path and the
lambda path handle missingness by different mechanisms.

### Alpha Dropping with Missing Data

When using `criterion = "alpha"`, the internal rankings are calculated
via [`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html). By
default, this function handles missing values by using pairwise deletion
(`use = "pairwise"`) to construct the underlying correlation matrix.
However, if you want to enforce complete cases (listwise deletion), you
can pass the `use` argument directly into the `alpha_args` list:

``` r

drop2oa_co <- dropit(
  data = extra,
  n_drop = 2,
  criterion = "alpha",
  approach = "oneshot",
  verbose = FALSE,
  alpha_args = list(
    check.keys = TRUE, 
    use = "complete.obs"  # change strategy
  )
)
```

### Lambda Dropping with Missing Data

With `criterion = "lambda"`, the internal CFA models are fitted using
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html), which
defaults to **listwise deletion**. If your dataset contains scattered
missing values, you might want to leverage Full Information Maximum
Likelihood (FIML). Because `dropit` acts as a seamless wrapper, you can
pass `missing = "fiml"` directly into `cfa_args`:

``` r

drop2_fiml<- dropit(
  data = extra,
  n_drop = 2,
  criterion = "lambda",
  approach = "greedy",
  verbose = FALSE,
  cfa_args = list(
    std.lv = TRUE,
    missing = "fiml"  # full information maximum likelihood
  )
)
```

### Greedy Dropping and Sample Stability

There is one combination worth pausing on: greedy dropping on data that
still contains missing values. Recall that the greedy approach refits
the model after every removal. Each of those refits runs on a smaller
set of items, so the engine re-derives its working sample each round
from whichever rows are usable for the items that remain. With scattered
missingness, that sample can quietly grow or shift from one round to the
next, and the ranking in round two is then computed on a slightly
different set of respondents than the ranking in round one. Because this
happens inside the loop, it leaves no trace in the returned object.

[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
therefore raises a warning whenever `approach = "greedy"` meets a
dataset with missing values, so the drift is stated rather than silent.
As with every warning
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
collects, it is retained on the returned object and reachable through
`$log`:

``` r

# Introduce some scattered missingness
extra_na <- extra
extra_na[sample(nrow(extra_na), 20), "E2"] <- NA

drop2_drift <- dropit(
  data = extra_na,
  n_drop = 2,
  criterion = "lambda",
  approach = "greedy",
  verbose = FALSE
)

# The warning is kept in the log
drop2_drift$log$warnings
#> [1] "The value of 'extra_na' was coerced to a data.frame."                                                                                                                                                                                                                                                    
#> [2] "Greedy dropping and missing data: each round refits on a smaller item set, so the sample the ranking uses can change from round to round as the engine re-derives it. Resolve the missing values before calling (for instance by restricting `data` to complete cases) for a fixed sample across rounds."
```

If a fixed sample across rounds matters—and for a comparison against a
baseline it usually does—resolve the missingness *before* the call, most
simply by restricting to complete cases.
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
then has nothing to warn about, and every round ranks the same
respondents:

``` r

drop2_stable <- dropit(
  data = extra_na[complete.cases(extra_na), ],
  n_drop = 2,
  criterion = "lambda",
  approach = "greedy",
  verbose = FALSE
)

cat("Dropped:", paste(drop2_stable$names, collapse = ", "), "\n")
#> Dropped: E5, E3
```

## Simulation Baselines

Because `dropit` is designed for experimental modifications, users might
want to compare abbreviated scales against the full, unmodified scale.
To facilitate seamless programmatic loops, passing `n_drop = 0` acts as
a “no-operation.” It bypasses the dropping algorithms and safely returns
the original dataset wrapped in the standard `dropit` object structure.
This prevents automated pipelines from crashing at the zero-drop
baseline.

``` r

# Dropping 0-3 items
res <- lapply(0:3, function(k) {
  dropit(
    data = extra, 
    n_drop = k, 
    criterion = "lambda", 
    verbose = FALSE
  )
})

# Extract retained datasets
base <- lapply(res, `[[`, "subset")

# View dims to confirm decay
sapply(base, dim)
#>      [,1] [,2] [,3] [,4]
#> [1,] 2800 2800 2800 2800
#> [2,]    5    4    3    2
```

Inside a loop like this,
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
re-runs its advisory guards—the boundary-tie, small-scale, and
greedy-with-missing-data warnings—on every single iteration, quietly
re-collecting the same messages into each `$log`. Once you have
validated your design and know those advisories no longer tell you
anything new, `checks = FALSE` switches them off for the whole run. Note
that `verbose = FALSE` will *not* do this: it only hides the printed
summary, while the guards still fire and still land in the log. The
switch changes nothing about which items are dropped—only whether the
guards run.

``` r

res <- lapply(0:3, function(k) {
  dropit(
    data = extra,
    n_drop = k,
    criterion = "lambda",
    checks = FALSE,  # skip the advisory guards across the simulation
    verbose = FALSE
  )
})

sapply(lapply(res, `[[`, "subset"), dim)
#>      [,1] [,2] [,3] [,4]
#> [1,] 2800 2800 2800 2800
#> [2,]    5    4    3    2
```

## Reproducibility

If your abbreviation strategy relies on stochastic `lavaan` arguments
(e.g., bootstrap standard errors or random starting values), you can
pass a `seed` argument directly to
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md).
Crucially, `dropit` takes a snapshot of your global random state
(`.Random.seed`), applies your exact seed for the internal mechanics,
and then safely restores the original state upon exit. This guarantees
reproducibility for the abbreviation baselines without permanently
side-effecting your R session’s broader simulation environment.

``` r

# Run CFA with bootstrapping twice; same seed
res1 <- dropit(
  data = extra, 
  n_drop = 1L, 
  criterion = "lambda", 
  seed = 42, 
  # Note: bootstrap iterations kept intentionally low for vignette compilation speed
  cfa_args = list(se = "bootstrap", bootstrap = 5), 
  verbose = FALSE
)

res2 <- dropit(
  data = extra, 
  n_drop = 1L, 
  criterion = "lambda", 
  seed = 42, 
  cfa_args = list(se = "bootstrap", bootstrap = 5),
  verbose = FALSE
)

# Verify subsets are identical
identical(res1$subset, res2$subset)
#> [1] TRUE
```
