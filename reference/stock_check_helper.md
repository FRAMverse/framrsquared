# Helper function to check that stock id make sense

More thorough checking than stock_id_comp. Checks that the number of
stock IDs makes sense given `NumStk`, that Stock IDs are sequential (in
the sense that if NumStk = n, every integer up to n is represented).
Optionally, can check that each stock ID is unique.

## Usage

``` r
stock_check_helper(table_name, NumStk, stock_vec, uniques_only = FALSE)
```

## Arguments

- table_name:

  Character of table name, for informative messages.

- NumStk:

  Maximum number of stock, pulled from BaseID table

- stock_vec:

  vector of stock ids to check. Presumably column of fetched table.

- uniques_only:

  Do we want warnings if there are duplicats of StockIDs? Useful for
  tables like Stock and Growth that should have only one entry per
  stock. Logical, default = `FALSE`.

## Value

Numeric, returning number of warnings detected.

## See also

[`addstock_check()`](https://framverse.github.io/framrsquared/reference/addstock_check.md)
