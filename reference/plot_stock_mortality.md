# Plot total mortalities by fishery

Creates an ordered bar chart with the top number of mortalities per
fishery.

## Usage

``` r
plot_stock_mortality(
  fram_db,
  run_id,
  stock_id,
  top_n = 10,
  filters_list = NULL,
  msp = TRUE
)
```

## Arguments

- fram_db:

  fram database object, supplied through connect_fram_db

- run_id:

  numeric, RunID

- stock_id:

  numeric, ID of focal stock

- top_n:

  numeric, Number of fisheries to display

- filters_list:

  list of framrsquared filter functions to apply before plotting.

- msp:

  Use Model Stock Proportion? Logical, defaults to TRUE.

## See also

[`plot_impacts_per_catch_heatmap()`](https://framverse.github.io/framrsquared/reference/plot_impacts_per_catch_heatmap.md),
[`plot_stock_mortality_time_step()`](https://framverse.github.io/framrsquared/reference/plot_stock_mortality_time_step.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |> plot_stock_mortality(run_id = 132, stock_id = 17)
fram_db |> plot_stock_mortality(run_id = 132, stock_id = 17,
        filters_list = list(filter_wa, filter_sport))
} # }
```
