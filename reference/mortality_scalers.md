# Quantify the proportion of fishery mortalities associated with stock(s) of interest

Supports guestimating the impact of making changes to a fishery on a
particular stock (or group of stocks) by multiplying its mortalities by
the `stock_mortality_ratio` produced by `mortality_scalers()`.

## Usage

``` r
mortality_scalers(fram_db, run_id, stock_id, msp = FALSE)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID

- stock_id:

  A focal stock or stocks

- msp:

  Do we use MSP expansion? Logical, defaults to FALSE. Only relevant for
  Chinook

## See also

[`plot_impacts_per_catch_heatmap()`](https://framverse.github.io/framrsquared/reference/plot_impacts_per_catch_heatmap.md)

## Examples

``` r
if (FALSE) fram_db |> mortality_scalers(run_id = 101, stock_id = c(17:18)) # \dontrun{}
```
