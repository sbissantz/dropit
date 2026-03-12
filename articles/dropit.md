# Tools for Reproducible Psychometric Experimentation

``` r
library(dropit)
#> dropit
#> Version: 0.0.0.9000
library(psych)
```

We will use the BFI data from the `psych` package to illustrate the
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
function for item dropping based on Cronbach’s alpha.

``` r
# Load BFI data
dta <- psych::bfi

# Get the position of (non) BFI items
xcld <- c("gender", "education", "age") 
xcld_pat <- colnames(dta) %in% xcld

# Store the data in a matrix
bfi <- as.matrix(dta[, !xcld_pat])

# Numer of respondents
N <- nrow(bfi)

# Number of items
I <- ncol(bfi)

# Labels
rownames(bfi) <- paste0("p",seq(N))

# Number of dimensions
D <- 5
```

## Assignment

The Q-matrix is a binary matrix that specifies the mapping of an item to
its respective latent construct(s). Here, it helps us define which BFI
items belong to which domain. A nice feature of this representation is
that it allows easy computation of sum scores for each domain by matrix
multiplication.

``` r
Q <- matrix(0, nrow = I, ncol = D) 
rownames(Q) <- colnames(bfi)
colnames(Q) <- c("a", "c", "e", "n", "o")

Q[1:5, "a"] <- 1 # Extraversion
Q[6:10, "c"] <- 1 # Agreeableness
Q[11:15, "e"] <- 1 # Conscientiousness
Q[16:20, "n"] <- 1 # Neuroticism
Q[21:25, "o"] <- 1 # Openness

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

## Extraversion

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

## Drop it

What happens, when we greadily drop, say the 2 worst-performing items
from the extraversion scale based on their alpha values? The
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
function allows us to explore that question.

``` r
drop2ga <- dropit(
  extra,
  n_drop = 2L,
  direction = "tail",
  criterion = "alpha",
  approach = "greedy",
  output_type = "both",
  alpha_metric = c("raw_alpha"),
  alpha_args = list(check.keys = TRUE),
  verbose = FALSE
)
drop2ga$names
#> [1] "E2" "E4"

# Manifest correlation between sum scores
cor(rowSums(extra), rowSums(drop2ga$subset), use = "pairwise.complete.obs")
#> [1] 0.8282344
```
