# Helper function to check that all stock x age combinations are present

\#' Intended for internal use, makes some assumptions about inputs.

## Usage

``` r
stock_age_checker(table_name, NumStk, old_stockcount, df, min_age, max_age)
```

## Arguments

- table_name:

  Character of table name, for informative messages.

- NumStk:

  Maximum number of stock, pulled from BaseID table

- old_stockcount:

  Number of stock in previous FRAM baseperiod. Only looks for problems
  for StockID \> this number.

- df:

  Dataframe to check. Must have columns "stock_id" and "age" (which are
  the names for relevant columns of framrsquared::fetch_table).

- min_age:

  Minimum age modeled. Should be the min_age from the baseid_df.

- max_age:

  Maximum age modeled. Should be the max_age from the baseid_df.

## Value

numeri; 0 if no warning, 1 if warning.
