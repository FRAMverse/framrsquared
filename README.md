# framrsquared
<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/FRAMverse/framrsquared/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FRAMverse/framrsquared/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/framrsquared)](https://CRAN.R-project.org/package=framrsquared)
<!-- badges: end -->

## Overview

framrsquared is a library with a focus on easing the burden of working in the FRAM database by providing convenient methods to import tables into R, and producing similar analyses as the FRAM executable while extending it some circumstances.
framrsquared is part of the [FRAMverse R-Universe](https://framverse.r-universe.dev/packages).

## Installation


framrsquared can be installed through R-Universe:

``` r
install.packages("framrsquared", repos = "https://framverse.r-universe.dev")
```

Otherwise, if you have Rtools and the `devtools` or `remotes` packages installed, framrsquared can be installed from source code:

``` r
devtools::install_github("FRAMverse/framrsquared")

# Alternatively 
remotes::install_github("FRAMverse/framrsquared")
```

