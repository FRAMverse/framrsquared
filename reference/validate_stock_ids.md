# Convenience function to check fishery input

No error checking for transfer databases

## Usage

``` r
validate_stock_ids(fram_db, stock_id, call = rlang::caller_env())
```

## Arguments

- fram_db:

  FRAM database object

- stock_id:

  one or more stock_ids

- call:

  internal use: identify name of function that called this function (for
  informative error message)
