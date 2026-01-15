# Replicate Stock Mortality screen

Returns a tibble matching the Stock Mortality screen.

## Usage

``` r
stock_mortality(fram_db, run_id = NULL)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |>
 stock_mortality(run_id=132) |>
 filter(stock_id == 17, fishery_id == 36)

} # }
```
