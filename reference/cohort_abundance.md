# **\[experimental\]** Calculate starting cohort abundance

The starting cohort abundance listed in the database can be wrong. This
function calculates the value by multipying the Stock Recruit Scalar by
the base period abundance.

## Usage

``` r
cohort_abundance(fram_db, run_id = NULL)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID (optional)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |>  cohort_abundance(run_id = 145)
} # }
```
