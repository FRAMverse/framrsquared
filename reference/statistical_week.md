# Vectorized approach to calculating the statistical week, returns an integer

Statistical weeks start on mondays, so the first statistical week of the
year starts on the first monday of the year. (Contrast with management
weeks which start on Sundays).

## Usage

``` r
statistical_week(date)
```

## Arguments

- date:

  A vector of dates

## Examples

``` r
if (FALSE) { # \dontrun{
data_fram |>
  mutate(mngmt_week = statistical_week(date_field))
} # }
```
