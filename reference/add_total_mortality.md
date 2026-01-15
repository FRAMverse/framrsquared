# Sum separate mortality columns into new "total_mortality" column

Convenience function for combining the separate mortality columns of the
Mortality table. Note: this does *not* account for AEQ for Chinook.

## Usage

``` r
add_total_mortality(.data)
```

## Arguments

- .data:

  Dataframe with separate mortality columns `landed_catch`,
  `non_retention`, `shaker`, `drop_off`, and `msf_` versions of each.
  Typically comes from `fetch_table("Mortality")` or
  [`aeq_mortality()`](https://framverse.github.io/framrsquared/reference/aeq_mortality.md).

## Value

`.data` with additional `$total_mortality` column just before
`$landed_catch`.
