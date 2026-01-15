# Generates post-season January age 3 abundances by stock from post-season databases. Used for forecasting.

Generates post-season January age 3 abundances by stock from post-season
databases. Used for forecasting.

## Usage

``` r
post_season_abundance(fram_db, units = c("ja3", "oa3"), run_ids = NULL)
```

## Arguments

- fram_db:

  FRAM database object

- units:

  Default January Age 3 'ja3', optional ocean age 3 'oa3'

- run_ids:

  Numeric vector of run_ids to use, necessary when there are multiple
  runs with the same run_year in the database. Optional, defaults to
  NULL.

## Examples

``` r
if (FALSE) framdb |> post_season_abundance() # \dontrun{}
```
