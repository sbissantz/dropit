# dropit – R Tools for Reproducible Psychometric Experimentation <img src="man/figures/sticker.png" align="right" width="120"/>

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/sbissantz/dropit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbissantz/dropit/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/sbissantz/dropit/branch/master/graph/badge.svg)](https://app.codecov.io/gh/sbissantz/dropit?branch=master)
<!-- badges: end -->

### Description

 The `dropit` package provides tools for reproducible psychometric experiments to explore how structural modifications in item-based measurements affect outcome metrics. The core function `dropit()` systematically removes items from questionnaire data to examine their impact on Cronbach’s alpha or confirmatory factor analysis (CFA) loadings. All procedures are fully traceable, with strict input validation, detailed message handling, and informative console output to support transparent and iterative research workflows.

### Installation

The `dropit` package is not available on CRAN. You can install the development version from GitHub.

Using **pak** (recommended):

```r
install.packages("pak")
pak::pak("sbissantz/dropit")
```

Alternatively, using **remotes**:

```r
install.packages("remotes")
remotes::install_github("sbissantz/dropit") 
```

### Contributing

Contributions, suggestions, and bug reports are welcome. Please open an issue or submit a pull request on GitHub.