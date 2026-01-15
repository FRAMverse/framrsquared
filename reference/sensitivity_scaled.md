# **\[experimental\]** Generate sensitivity analyses runs scaled by template values

From a template FRAM run, for a single vector of scaling factors (e.g.,
`c(0.5, 2)` would test halving and doubling), generate sensitivity
analyses which rescale the columns specied in (`cols_to_vary`) for rows
which match the conditions specified in `match_df`. Optionally creates
corresponding tamms from a template TAMM, labeled to work with folder
loading option in the FRAM multi-run fork.

## Usage

``` r
sensitivity_scaled(
  fram_db,
  template_run,
  table_name,
  match_df,
  scale_values,
  cols_to_vary,
  tamm_template = NULL,
  tamm_target_folder = NULL,
  label = "sensitivity",
  save_log = TRUE
)
```

## Arguments

- fram_db:

  Fram database

- template_run:

  Run ID of the run that should be used as a template for the
  sensitivity analyses.

- table_name:

  Name of FRAM table that will be modified for the sensitivity analyses

- match_df:

  dataframe that defines which rows should be modified during
  sensitivity analyses. To modify some values for marked and unmarked
  Stillaguamish stocks, we would use data.frame(StockID = c(17, 18)). To
  modify values only for Stillaguamish age 2s, we would use
  expand_grid(StockID = c(17, 19), Age = 2). Unlike match/replace
  dataframes for
  [`modify_table()`](https://framverse.github.io/framrsquared/reference/modify_table.md),
  column names do not need to start with "match\_" (but this function
  will still work if they do).

- scale_values:

  Numeric vector of the scaling factors to be be used, one per
  sensitivity analysis run. Defines the number of runs generated. For
  example, `scale_values = 2:10` would generate 9 runs. The first would
  multiply the values of interest by 2, the second by 3, etc.

- cols_to_vary:

  Character or character vector of column names of FRAM table
  `table_name` that should be rescaled.

- tamm_template:

  Optional; character string of filepath of a TAMM to be used as a
  template. If provided (and tamm_target_folder provided),
  `sensitivity_scaled` will make a tamm for each sensitivity analysis
  run, using names that work with the FRAM multirun fork "Use folder"
  option.

- tamm_target_folder:

  Folder to copy TAMMs into. Will create if it does not exist.

- label:

  Label added to each of the generated run names to identify this
  sequence of sensitivity analyses. String, defaults to "sensitivity"

- save_log:

  Should a log .csv of the specifics used (row ids, match criterion,
  scaling or replacement values) be saved in the same folder as the FRAM
  database? Logical, defaults to TRUE.

## Value

Invisibly returns a list of dataframes. \$scales_by_runs contains a row
for each sensitivity run and maps the scaling factors to run ids.
\$full_df is the full match/scale factor used by calc_fram_scaling, and
shows the match conditions and scaling used for each run.

## Details

Dev note: update to allow match_df to NOT start with "match\_" – it's
implied.

## See also

[`sensitivity_exact()`](https://framverse.github.io/framrsquared/reference/sensitivity_exact.md),
[`sensitivity_custom()`](https://framverse.github.io/framrsquared/reference/sensitivity_custom.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Testing sensitivity_scaled
library(here)
fram_db <- connect_fram_db(here("Valid2024_sens_test.mdb"))

tamm_template <- "ChinValidrunTest.xlsx"
tamm_target_folder <- here("sens_test/")
fram_db |>
  sensitivity_scaled(
    template_run = 28,
    table_name = "StockRecruit",
    match_df = data.frame(match_StockID = c(17, 19)),
    scale_values = seq(0.1, 2, length = 10),
    cols_to_vary = c("RecruitScaleFactor", "RecruitCohortSize"),
    tamm_template = tamm_template,
    tamm_target_folder = tamm_target_folder
  )
disconnect_fram_db(fram_db)
} # }
```
