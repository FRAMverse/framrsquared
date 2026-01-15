# **\[experimental\]** Returns a tibble displaying predicted FRAMencounter mark rates by fishery, fishery type, and time-step.

**\[experimental\]** Returns a tibble displaying predicted FRAMencounter
mark rates by fishery, fishery type, and time-step.

## Usage

``` r
coho_mark_rates(fram_db, run_id = NULL)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID (optional)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |> coho_mark_rates(run_id)
} # }
```
