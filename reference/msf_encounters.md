# Reproduce MSF encounters screen

Produces the MSF screen report numbers for encounters. Returns different
format depending database.

## Usage

``` r
msf_encounters(fram_db, run_id = NULL)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID

## See also

[`msf_mortalities()`](https://framverse.github.io/framrsquared/reference/msf_mortalities.md),
[`msf_landed_catch()`](https://framverse.github.io/framrsquared/reference/msf_landed_catch.md)

## Examples

``` r
if (FALSE) fram_db |> msf_encounters(run_id = 101) # \dontrun{}
```
