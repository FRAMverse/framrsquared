# Label fisheries based on FRAM database

Like
[`label_fisheries()`](https://framverse.github.io/framrosetta/reference/label_fisheries.html),
but uses an active FRAM database to label fisheries, rather than the
look-up table present in the framrosetta package. Primarily used in
[`fetch_table()`](https://framverse.github.io/framrsquared/reference/fetch_table.md),
useful if working with databases with unusual stock or fishery tables.

## Usage

``` r
label_fisheries_db(.data, fram_db)
```

## Arguments

- .data:

  Dataframe containing \`fishery_id\` column (or analogous column with
  different name specied by \`fisheries_col\` argument)

- fram_db:

  FRAM database connection

## Value

`.data` with additional column, `$fishery_label`

## See also

[`label_fisheries()`](https://framverse.github.io/framrosetta/reference/label_fisheries.html),
[`label_stocks_db()`](https://framverse.github.io/framrsquared/reference/label_stocks_db.md)
