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

### Note for WDFW employees

Our IT department is treating the default unzipping functionality of the `pak` package
as a security risk. Either install the package with `devtools::install_github()` rather than `pak::pkg_install()` or set up 
R to use a fallback unzipping method when `pak`'s default fails. To do that, edit your .Rprofile file 
(e.g., with `usethis::edit_r_profile()`) to add the following line: `R_ZIP_PROCESS_FALLBACK=true`.


### Instructions

framrsquared can be installed through R-Universe:

``` r
install.packages(c("framrsquared", "framrosetta"), repos = "https://framverse.r-universe.dev")
```

Otherwise, if you have Rtools and the `devtools` or `remotes` packages installed, framrsquared can be installed from source code:

``` r
devtools::install_github("FRAMverse/framrsquared")

# Alternatively 
pak::pkg_install("FRAMverse/framrsquared")
```

To install the development version, which may include new features that have been added but may not be as thoroughly tested:

```r
devtools::install_github("FRAMverse/framrsquared@dev")

#Alternatively
# pak::pkg_install("FRAMverse/framrsquared@dev")
```

You can then load the development version with `library(framrsquared.dev)`.



## Contributing

To support unit tests while maintaining the privacy of parts of the databases, we have set up an optional unit testing procedure. If you want to be able to run the tests during development, contact Collin Edwards to get a zip file containing databases used for testing (pre, post, and transfer databases for Chinook and Coho, as well as several "broken" databases that test framrsquared error handling). The unit tests skip unless  there is a environmental variable `FRAMRSQURED_TEST_DIR`, which should point to your local copy of this test directory (unzipped). 

In addition to enabling the unit tests, setting up the test directory and associated environmental variable will enable a collection of internal `connection_*` functions (defined in `tests/testthat/helper-dir.R`) to quickly make read-only connections with fram databases for ad-hoc testing and development.
