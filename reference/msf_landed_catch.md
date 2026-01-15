# Reproduce MSF landed catch screen

Produces the MSF screen report numbers for landed catch. Returns
different format depending database.

## Usage

``` r
msf_landed_catch(fram_db, run_id = NULL)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID

## See also

[`msf_encounters()`](https://framverse.github.io/framrsquared/reference/msf_encounters.md),
[`msf_mortalities()`](https://framverse.github.io/framrsquared/reference/msf_mortalities.md)

## Examples

``` r
if (FALSE) fram_db |> msf_landed_catch(run_id = 101) # \dontrun{}
```
