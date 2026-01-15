# Filters a dataframe to Alaska fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to Alaska fisheries. Will automatically detect
whether it's working with a Chinook or Coho dataset if the tables were
generated within this package. `.data` must have a `fishery_id` column
name.

## Usage

``` r
filter_ak(.data, species = NULL)
```

## Arguments

- .data:

  Dataframe containing `fishery_id` column. Commonly, output from
  [`framrsquared::fetch_table()`](https://framverse.github.io/framrsquared/reference/fetch_table.md).

- species:

  Optional argument to identify species if `.data` doesn't already. If
  provided, must be "COHO" or "CHINOOK" or variations thereof. Defaults
  to `NULL`

## See also

[`filter_sport()`](https://framverse.github.io/framrsquared/reference/filter_sport.md),
[`filter_net()`](https://framverse.github.io/framrsquared/reference/filter_net.md),
[`filter_puget_sound()`](https://framverse.github.io/framrsquared/reference/filter_puget_sound.md),
[`filter_bc()`](https://framverse.github.io/framrsquared/reference/filter_bc.md),
[`filter_wa()`](https://framverse.github.io/framrsquared/reference/filter_wa.md),
[`filter_ca()`](https://framverse.github.io/framrsquared/reference/filter_ca.md),
[`filter_or()`](https://framverse.github.io/framrsquared/reference/filter_or.md),
[`filter_coast()`](https://framverse.github.io/framrsquared/reference/filter_coast.md),
[`filter_marine()`](https://framverse.github.io/framrsquared/reference/filter_marine.md),
[`filter_commercial_wa_nt()`](https://framverse.github.io/framrsquared/reference/filter_commercial_wa_nt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_dataframe |> filter_ak()
} # }
framrosetta::fishery_chinook_fram |> filter_ak(species = "CHINOOK")
#> # A tibble: 3 × 5
#>   species version_number fishery_id fishery_name fishery_title  
#>   <chr>            <int>      <int> <chr>        <chr>          
#> 1 CHINOOK              1          1 SEAK Troll   SE Alaska Troll
#> 2 CHINOOK              1          2 SEAK Net     SE Alaska Net  
#> 3 CHINOOK              1          3 SEAK Sport   SE Alaska Sport
framrosetta::fishery_coho_fram |> filter_ak(species = "COHO")
#> # A tibble: 5 × 5
#>   species version_number fishery_id fishery_name fishery_title       
#>   <chr>            <int>      <int> <chr>        <chr>               
#> 1 COHO                 1        194 SW AK  Trl   SEAK Southwest Troll
#> 2 COHO                 1        195 SE AK  Trl   SEAK Southeast Troll
#> 3 COHO                 1        196 NW AK  Trl   SEAK Northwest Troll
#> 4 COHO                 1        197 NE AK  Trl   SEAK Northeast Troll
#> 5 COHO                 1        198 Alaska Net   Southeast Alaska Net
```
