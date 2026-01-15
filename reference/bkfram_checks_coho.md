# Performs error checks of a backwards FRAM run Returns nested tibble with diagnostics

Performs error checks of a backwards FRAM run Returns nested tibble with
diagnostics

## Usage

``` r
bkfram_checks_coho(fram_db, backward_run_id = NULL, forward_run_id = NULL)
```

## Arguments

- fram_db:

  fram database object, supplied through connect_fram_db

- backward_run_id:

  numeric, RunID

- forward_run_id:

  numeric, RunID

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |> bkfram_checks_coho(backward_run_id = 132, forward_run_id = 133)
} # }
```
