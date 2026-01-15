# Check FRAM database after adding new stock

Either provides the step by step process of adding new stock to a FRAM
database, or walks through fram database run and checks the tables for
potential errors associated with adding new stock.

## Usage

``` r
addstock_check(
  file_name = NULL,
  run_id = NULL,
  old_stockcount = 78,
  override_db_checks = FALSE
)
```

## Arguments

- file_name:

  filepath to database. If `NULL`, provide summary of process instead.
  Default = `NULL`.

- run_id:

  RunID associated with the new stock in the FRAM database. If left as
  `NULL`, provide summary of process instead. Default = `NULL`.

- old_stockcount:

  The number of stocks previously present to treat as the "baseline" –
  several checking steps will focus solely on newly added stocks.
  Defaults to 78.

- override_db_checks:

  Ignore species, database type. When `FALSE`, function will stop if the
  database is not Chinook or if it's a transfer file. Defaults to FALSE.

## Examples

``` r
if (FALSE) { # \dontrun{
## review process
addstock_check()
## check database for additional stock
addstock_check("2024 Pre-Season Chinook DB - first test.mdb",
run_id = 138)
} # }
```
