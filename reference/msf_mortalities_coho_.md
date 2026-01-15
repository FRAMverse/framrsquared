# Replicate MSF screen report mortalities for COHO

Returns a tibble matching the MSF screen report mortalities for Coho.
This is specific for Coho and in most cases
[`msf_mortalities()`](https://framverse.github.io/framrsquared/reference/msf_mortalities.md)
is preferable.

## Usage

``` r
msf_mortalities_coho_(fram_db)
```

## Arguments

- fram_db:

  FRAM database object

## See also

[`msf_mortalities()`](https://framverse.github.io/framrsquared/reference/msf_mortalities.md),
[`msf_encounters_coho_()`](https://framverse.github.io/framrsquared/reference/msf_encounters_coho_.md),
[`msf_landed_catch_coho_()`](https://framverse.github.io/framrsquared/reference/msf_landed_catch_coho_.md)

## Examples

``` r
if (FALSE) fram_db |> msf_mortalities_coho_() # \dontrun{}
```
