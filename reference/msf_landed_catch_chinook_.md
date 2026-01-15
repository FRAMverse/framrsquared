# Replicate MSF screen report landed catch for Chinook

Returns a tibble matching the MSF screen report landed catch for
Chinook. This is specific for Chinook and in most cases
[`msf_landed_catch()`](https://framverse.github.io/framrsquared/reference/msf_landed_catch.md)
is preferable.

## Usage

``` r
msf_landed_catch_chinook_(fram_db)
```

## Arguments

- fram_db:

  FRAM database object

## See also

[`msf_landed_catch()`](https://framverse.github.io/framrsquared/reference/msf_landed_catch.md),
[`msf_mortalities_chinook_()`](https://framverse.github.io/framrsquared/reference/msf_mortalities_chinook_.md),
[`msf_encounters_chinook_()`](https://framverse.github.io/framrsquared/reference/msf_encounters_chinook_.md)

## Examples

``` r
if (FALSE) fram_db |> msf_landed_catch_chinook_(run_id = 101) # \dontrun{}
```
