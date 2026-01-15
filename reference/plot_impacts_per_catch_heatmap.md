# Make plots to show the amount of landed catch_per_impact

Identify how much reduction in landed catch at each fishery that would
be needed to reduce the impacts on a focal stock by 1 fish.

## Usage

``` r
plot_impacts_per_catch_heatmap(
  fram_db,
  run_id,
  stock_id,
  filters_list = list(filter_wa, filter_sport),
  msp = TRUE
)
```

## Arguments

- fram_db:

  fram database connection

- run_id:

  run_id of interest

- stock_id:

  stock_id of interest

- filters_list:

  list of framrsquared filter functions to apply before plotting.
  Defaults to `list(filter_wa, filter_sport)`, which filters to WA sport
  fisheries.

- msp:

  Use Model Stock Proportion? Logical, defaults to TRUE.

## Value

ggplot object

## See also

[`plot_stock_mortality()`](https://framverse.github.io/framrsquared/reference/plot_stock_mortality.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path = "FRAM compilations - readonly/2024-Pre-Season-Chinook-DB/2024 Pre-Season Chinook DB.mdb"
run_id = 132
stock_id = 3
plot_impacts_per_catch_heatmap(path,
                               run_id = 132,
                               stock_id = 5)
} # }
```
