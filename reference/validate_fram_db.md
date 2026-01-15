# Convenience function to check fram_db input

Convenience function to check fram_db input

## Usage

``` r
validate_fram_db(
  fram_db,
  db_type = NULL,
  db_species = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- fram_db:

  FRAM database object

- db_type:

  Enforcement of a database type 'full' or 'transfer'

- db_species:

  Enforcement of a species 'COHO' or 'CHINOOK'

- call:

  internal use: identify name of function that called this function (for
  informative error message)
