# **\[experimental\]** Generate sensitivity analyses runs based on exact values

As `sensitivity_scaled`, but provide exact values for the sensitivity
analyses (in argument `exact_values`) instead of scaling factors.

## Usage

``` r
sensitivity_exact(
  fram_db,
  template_run,
  table_name,
  match_df,
  exact_values,
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

- exact_values:

  numeric vector of values to exact values to use for sensitivity
  analyses.

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

Invisibly returns a list of dataframes. \$values_by_run contains a row
for each sensitivity run and maps the values used to run ids. \$full_df
is the full match/scale factor used by calc_fram_scaling, and shows the
match conditions and scaling used for each run. If `cols_to_vary` has
length 1, the two dataframes will contain the same information.

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db <- connect_fram_db(here("Valid2024_sens_test.mdb"))

tamm_template <- here("Coho2513NOF-165.xlsx")
tamm_target_folder <- here("sens_test_exact/")
fram_db |>
  sensitivity_exact(
    template_run = 28,
    table_name = "StockRecruit",
    match_df = data.frame(match_StockID = 1:2),
    exact_values = seq(0.5, 5, by = 0.5),
    cols_to_vary = c("RecruitScaleFactor"),
    tamm_template = tamm_template,
    tamm_target_folder = tamm_target_folder,
    label = "Stilly sensitivity exact"
  )
disconnect_fram_db(fram_db)
} # }
```
