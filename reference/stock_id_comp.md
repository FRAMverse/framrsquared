# Helper function to check that stock id exist in the Stock database

Intended for internal use, makes some assumptions about inputs.

## Usage

``` r
stock_id_comp(table_name, df, stock_ref)
```

## Arguments

- table_name:

  Character of table name, for informative messages

- df:

  Dataframe

- stock_ref:

  numeric vector of all stock IDs. Should be stock_df\$stock_id.

## Value

numeric; 0 if no warning, 1 if warning.

## See also

[`addstock_check()`](https://framverse.github.io/framrsquared/reference/addstock_check.md)
