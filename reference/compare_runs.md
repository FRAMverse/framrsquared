# Generates a report to the console of changes to inputs between two runs

Generates a report to the console of changes to inputs between two runs

## Usage

``` r
compare_runs(fram_db, run_ids, tolerance = 0.01)
```

## Arguments

- fram_db:

  FRAM database object

- run_ids:

  Two run ids. Run names must differ; change in FRAM if necessary.

- tolerance:

  Tolerance of detection, 1 percent default

## Examples

``` r
if (FALSE) fram_db |> compare_runs(c(55, 56)) # \dontrun{}
```
