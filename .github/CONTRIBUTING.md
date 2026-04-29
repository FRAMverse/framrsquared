# Contributing to framrsquared

This outlines how to propose a change to framrsquared. Updates to framrsquared will first be pulled into the `dev` branch, which has been renamed `framrsquared.dev()`; write documentation and examples accordingly. 

## General guidance

We use roxygen2 with Markdown syntax for documentation. Development follows the workflow [described here](https://r-pkgs.org/) and relies on the usethis package. 

Changes to user-facing functions (or the addition of new functions) should include a bullet in the `NEWS.md` file. 

Is this package the right home for your desired features? Functions focused on interacting with the TAMM should instead live in `TAMMsupport`, and look-up tables should live in `framrosetta`.

## New functions

Function names should start with a verb if at all possible (e.g. `fetch_table()` rather than `table_fetcher()`), and should use snake_case. In general we favor longer, clearer function names rather than shorter ones. The roxygen2 documentation for new functions should start with \`r lifecycle::badge("experimental")\` to give them the "experimental" badge. This will be removed when the function is first merged into the main branch. 

Some guidelines:

- `fram_abort()` should be used for errors (rather than cli::cli_abort()). This is a wrapper for cli::cli_abort() that includes the custom `framrsquared_error` class label, making it easier for test functions to correctly differentiate intended vs unintended errors.
- Most framrsquared functions should take a fram database connection rather than a path to a database. The only exceptions are `connect_fram_db()` and functions that connect to multiple databases.
- Many framrsquared functions take similar arguments (e.g. run id, stock id, etc). Make sure your new function uses the existing argument naming conventions, if relevant.
- framrsquared has a suite of argument validation functions in `integrity.R` for common argument types (fram database, numeric inputs, character inputs, logical flags, run ids, etc). Most of these functions have optional arguments to require a specific argument length (e.g., `n = 2` requires the argument be length 2) or to allow the arugment to have a value of `NULL` for optional arguments (`allow_null = TRUE`). Where possible, use these pre-existing functions rather than writing custom argument validation code. 
- Messages to users should be written using the `cli` package. 
- Figures should be based on ggplot2 rather than base R graphics. This provides much better handling for saving, connecting functions, etc.
- Functions that work with dataframes should be written to support piping (i.e., have the dataframe be the first argument). `filter_*()` functions are good examples. 
- fetch_table() adds a "species" attribute to dataframes, identifying either "CHINOOK" or "COHO". Functions that output dataframes should retain that attribute; be aware that some R functions strip attributes, and it may be necessary to add the attribute back (e.g. `filter_*()`).
- functions should not change the working directory.
- Unless they are very simple, functions for internal use should still be documented with roxygen2 formatting, should not be exported (no `"' @export`), and should be listed as internal functions (`"' @keywords internal`). This means the function will have documentation for other developers accessible with the `?`, but won't clog up user-facing lists of functions
- We are working to organize similar functions with `@family` [roxygen2 tag](https://roxygen2.r-lib.org/articles/index-crossref.html), which allows easy cross-referencing and organization of functions in pkgdown. If you are writing a new function that is similar to others (e.g., a new fishery filter function), see if there is already a family for these functions. 
- framrsquared documentation is implemented using the pkgdown package, which uses a custom function organization. Mismatches between the package and the pkgdown organization are a common cause of issues when checking pushes on Github. New functions typically need to be added to that organization, accessible in `./_pkgdown.yml`. The exception are functions that match a pattern already used for organization (e.g., functions starting with "compare") or functions that are associated with an existing concept (e.g., roxygen2 documentation includes `"' @family fishery_filters`).
- Helper functions that implement most of the work of a user-facing function (e.g., `msf_encounters_chinook_()`, or alias functions (e.g., `fetch_table_()`) should end in an underscore to help clarify their use. 

## Testing functions

We use the `testthat` framework for writing function tests.

- `test_helpers.R` has some useful functions for mocking databases and viewing mock databases. It also has functions to access local test databases -- reach out to Collin for a copy of those if you want to use them.
- functions that we have decided not to test should be given the custom tag `@notag` in the roxygen documentation. This will be ignored by the package dev process, but helps identify which functions we
have chosen not to test.
- Any test function that references local databases for integration tests should start with `skip_if_no_test_db()` to ensure that it doesn't cause errors when run on computers without the test databasess set up.
- Any function that creates a connection to a database or a mock database should use `withr::defer()` to call the disconnection on exit or function error. For examples, see `test-compare_inputs.R`. This approach can also be used to remove temporary files if testing a function that creates a file or needs a temporary file to interact with (e.g., `compare_runs()` when saving to file).
- expect_error() calls should include `class = "framrsquared_error"` for any package-based errors (e.g., errors coming from `fram_abort()`). This helps avoid situations in which an unanticipated error from another package or base R is being interpretted as the intended framrsquared error. 

