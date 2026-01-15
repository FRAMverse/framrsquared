# Replicate MSF screen report landed catch for COHO

Returns a tibble matching the MSF screen report landed catch for Coho
This is specific for Coho and in most cases msf_landed_catch() is
preferable.

## Usage

``` r
msf_landed_catch_coho_(fram_db)
```

## Arguments

- fram_db:

  FRAM database object

## See also

[`msf_landed_catch()`](https://framverse.github.io/framrsquared/reference/msf_landed_catch.md),
[`msf_encounters_coho_()`](https://framverse.github.io/framrsquared/reference/msf_encounters_coho_.md),
[`msf_mortalities_coho_()`](https://framverse.github.io/framrsquared/reference/msf_mortalities_coho_.md)

## Examples

``` r
if (FALSE) fram_db |> msf_landed_catch_coho_() # \dontrun{}
```
