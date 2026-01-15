# Reproduce MSF mortalities screen

Produces the MSF screen report numbers for mortalities. Returns
different format depending database.

## Usage

``` r
msf_mortalities(fram_db, run_id = NULL)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID

## See also

[`msf_encounters()`](https://framverse.github.io/framrsquared/reference/msf_encounters.md),
[`msf_landed_catch()`](https://framverse.github.io/framrsquared/reference/msf_landed_catch.md)

## Examples

``` r
if (FALSE) fram_db |> msf_mortalities_coho_(run_id = 101) # \dontrun{}
```
