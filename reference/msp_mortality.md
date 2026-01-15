# Expand Chinook mortality table using Model-Stock Proportion

See
https://framverse.github.io/fram_doc/calcs_data_chin.html#46_Model-Stock_Proportion.

## Usage

``` r
msp_mortality(fram_db, run_id = NULL)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  One or more run ids (optional)

## Value

Mortality table with mortality values expanded by msp

## See also

[`aeq_mortality()`](https://framverse.github.io/framrsquared/reference/aeq_mortality.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |> msp_mortality(run_id = 132)
} # }
```
