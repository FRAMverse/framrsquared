# Returns a tibble matching the Fishery Mortality screen.

Returns a tibble matching the Fishery Mortality screen.

## Usage

``` r
fishery_mortality(fram_db, run_id = NULL, msp = TRUE)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID

- msp:

  Use Model Stock Proportion? Logical, defaults to TRUE.

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |> fishery_mortality(run_id = 101)
} # }
```
