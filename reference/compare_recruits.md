# Compares the recruit scalers of two runs

Compares the recruit scalers of two runs

## Usage

``` r
compare_recruits(fram_db, run_ids, tolerance = 0.01, verbose = TRUE)
```

## Arguments

- fram_db:

  FRAM database object

- run_ids:

  Two run ids

- tolerance:

  Tolerance for detecting changes

- verbose:

  If `TRUE`, print an update to screen when there are no differences in
  recruits.

## See also

[`compare_runs()`](https://framverse.github.io/framrsquared/reference/compare_runs.md)

## Examples

``` r
if (FALSE) fram_db |> compare_recruits() # \dontrun{}
```
