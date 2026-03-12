# dropit – R Tools for Reproducible Psychometric Experimentation ![](reference/figures/sticker.png)

### Description

The `dropit` package provides tools for reproducible psychometric
experiments to explore how structural modifications in item-based
measurements affect outcome metrics. The core function
[`dropit()`](https://sbissantz.github.io/dropit/reference/dropit.md)
systematically removes items from questionnaire data to examine their
impact on Cronbach’s alpha or confirmatory factor analysis (CFA)
loadings. All procedures are fully traceable, with strict input
validation, detailed message handling, and informative console output to
support transparent and iterative research workflows.

### Installation

The `dropit` package is not available on CRAN. You can install the
development version from GitHub.

Using **pak** (recommended):

``` r
install.packages("pak")
pak::pak("sbissantz/dropit")
```

Alternatively, using **remotes**:

``` r
install.packages("remotes")
remotes::install_github("sbissantz/dropit") 
```

### Contributing

Contributions, suggestions, and bug reports are welcome. Please open an
issue or submit a pull request on GitHub.
