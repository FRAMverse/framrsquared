# Label stocks based on FRAM database

Like
[`label_stocks()`](https://framverse.github.io/framrosetta/reference/label_stocks.html),
but uses an active FRAM database to label stocks, rather than the
look-up table present in the framrosetta package. Primarily used in
[`fetch_table()`](https://framverse.github.io/framrsquared/reference/fetch_table.md),
useful if working with databases with unusual stock or fishery tables.

## Usage

``` r
label_stocks_db(.data, fram_db)
```

## Arguments

- .data:

  Dataframe containing \`stock_id\` column (or analogous column with
  different name specied by \`stocks_col\` argument)

- fram_db:

  FRAM database connection

## Value

`.data` with additional column, `$stock_label`

## See also

[`label_stocks()`](https://framverse.github.io/framrosetta/reference/label_stocks.html),
[`label_fisheries_db()`](https://framverse.github.io/framrsquared/reference/label_fisheries_db.md)
