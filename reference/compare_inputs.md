# Generates a dataframe that compares fishery scalers table for two runs identified by run_id's

Generates a dataframe that compares fishery scalers table for two runs
identified by run_id's

## Usage

``` r
compare_inputs(fram_db, run_ids)
```

## Arguments

- fram_db:

  FRAM database object

- run_ids:

  Vector of two run_ids

## Value

Data frame of differences. `$percentdiff` = change in quota (comparing
the appropriate quotas based on fishery flags), `$regulation_comparison`
= change in regulation (NS, MSF, NS + MSF). Columns present in the
FisheriesScalers database are included, with `_original` and
`_comparison` suffixes identifying entries associated with the first and
second entries of `run_ids`, respectively.

## Details

Comparisons assume the first run provided is the baseline, and provide
relative changes from that. This includes percent changes
(\$percent.diff)include percent changes (infinite when)

## See also

[`compare_inputs_chart()`](https://framverse.github.io/framrsquared/reference/compare_inputs_chart.md)

## Examples

``` r
if (FALSE) fram_db |> compare_inputs(c(100,101)) # \dontrun{}
```
