# Compares the non retention flags of two runs

Compares the non retention flags of two runs

## Usage

``` r
compare_non_retention_input_flags(fram_db, run_ids, verbose = TRUE)
```

## Arguments

- fram_db:

  FRAM database object

- run_ids:

  Two run ids

- verbose:

  If `TRUE`, print an update to screen when there are no differences in
  recruits.

## See also

[`compare_runs()`](https://framverse.github.io/framrsquared/reference/compare_runs.md)

## Examples

``` r
if (FALSE) fram_db |> compare_non_retention_inputs(c(55, 56)) # \dontrun{}
```
