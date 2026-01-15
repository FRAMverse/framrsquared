# Plot stock composition

Produces a stock composition chart, low frequency stocks are grouped
into geographic area.

## Usage

``` r
plot_stock_comp(fram_db, run_id, fishery_id, time_step, group_threshold = 0.01)
```

## Arguments

- fram_db:

  Fram database object

- run_id:

  numeric, RunID

- fishery_id:

  numeric, Fishery ID

- time_step:

  numeric, Time Step

- group_threshold:

  numeric, Stock percentages below this number will be grouped. Default
  is 1%, setting to zero will turn grouping off

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |> stock_comp(run_id = 132)
} # }
```
