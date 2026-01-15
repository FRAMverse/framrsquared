# **\[experimental\]** Generate sensitivity analyses runs based on a list of match/replace dataframes

For complex sensitivity analyses, it may be easiest to programmatically
create a series of match/replace dataframes
([`?modify_table`](https://framverse.github.io/framrsquared/reference/modify_table.md)),
one for each sensitivity run. `sensitivity_custom()` uses a list of
these dataframes to create a series of sensitivity analyses runs.
Otherwise behaves as
[`sensitivity_exact()`](https://framverse.github.io/framrsquared/reference/sensitivity_exact.md)
or
[`sensitivity_scaled()`](https://framverse.github.io/framrsquared/reference/sensitivity_scaled.md).
Saved log is a .rds file that contains `scenario_list` but with each
list item named with the matching RunID.

## Usage

``` r
sensitivity_custom(
  fram_db,
  template_run,
  table_name,
  scenario_list,
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

  Name of FRAM table that will be modified for the sensitivity analyses.
  For list items that are named, be ignored in favor of item name.

- scenario_list:

  List of match/replace dataframes as described in documentation of
  [`modify_table()`](https://framverse.github.io/framrsquared/reference/modify_table.md).
  If present, list item names are assumed to identify the table to be
  changed.

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

Invisibly returns object `scenario_list`, but with list items named with
the corresponding RunID.

## Details

Current framework does not support automating creation of sensitivity
analyses in which changes are being made to multiple tables for a single
run.

## See also

[`sensitivity_scaled()`](https://framverse.github.io/framrsquared/reference/sensitivity_scaled.md),
[`sensitivity_exact()`](https://framverse.github.io/framrsquared/reference/sensitivity_exact.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## silly quick-and-dirty example: try these
## two scenarios: mark release rates of 0.05 and 0.01 for fisheries 1 and 2
## for timestep 1, or flipping those. Modifications to FisheryScalers table
fram_db <- connect_fram_db(here("Valid2024_sens_test.mdb"))

custom_scenarios = list(data.frame(match_FisheryID = c(1, 2),
                                  match_TimeStep = c(1, 1),
                                  replace_MarkReleaseRate = c(.05, .01)),
                       data.frame(match_FisheryID = c(1, 2),
                                  match_TimeStep = c(1, 1),
                                  replace_MarkReleaseRate = c(.01, .05))
)

tamm_template = here("Coho2513NOF-165.xlsx")
tamm_target_folder = here("sens_test_custom/")
fram_db |>
 sensitivity_custom(template_run = 28,
                    table_name = 'FisheryScalers',
                    scenario_list = custom_scenarios,
                    tamm_template = tamm_template,
                    tamm_target_folder = tamm_target_folder,
                    label = "markrelease custom")
disconnect_fram_db(fram_db)
} # }
```
