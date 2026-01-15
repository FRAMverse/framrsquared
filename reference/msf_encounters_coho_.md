# Replicate MSF screen report encounters for COHO

Returns a tibble matching the MSF screen report encounters for Coho This
is specific for Coho and in most cases msf_encounters() is preferable.

## Usage

``` r
msf_encounters_coho_(fram_db)
```

## Arguments

- fram_db:

  FRAM database object

## See also

[`msf_encounters()`](https://framverse.github.io/framrsquared/reference/msf_encounters.md),
[`msf_landed_catch_coho_()`](https://framverse.github.io/framrsquared/reference/msf_landed_catch_coho_.md),
[`msf_mortalities_coho_()`](https://framverse.github.io/framrsquared/reference/msf_mortalities_coho_.md)

## Examples

``` r
if (FALSE) fram_db |> msf_encounters_coho_() # \dontrun{}
```
