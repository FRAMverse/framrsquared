# **\[experimental\]** Chinook flavor of the stock fate function

**\[experimental\]** Chinook flavor of the stock fate function

## Usage

``` r
stock_fate_chinook(fram_db, run_id = NULL, units = c("fish", "percentage"))
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID (optional)

- units:

  'fish' or 'percentage'. Percentage is proportion of starting abundance

## See also

[`stock_fate()`](https://framverse.github.io/framrsquared/reference/stock_fate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |> stock_fate_chinook(run_id = 145)
} # }
```
