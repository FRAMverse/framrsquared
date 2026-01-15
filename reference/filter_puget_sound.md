# Filters a dataframe to Puget Sound fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to Puget Sound fisheries. Will automatically detect
whether it's working with a Chinook or Coho dataset if the tables were
generated within this package. `.data` must have a `fishery_id` column
name.

## Usage

``` r
filter_puget_sound(.data, species = NULL)
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
[`filter_wa()`](https://framverse.github.io/framrsquared/reference/filter_wa.md),
[`filter_bc()`](https://framverse.github.io/framrsquared/reference/filter_bc.md),
[`filter_ak()`](https://framverse.github.io/framrsquared/reference/filter_ak.md),
[`filter_ca()`](https://framverse.github.io/framrsquared/reference/filter_ca.md),
[`filter_or()`](https://framverse.github.io/framrsquared/reference/filter_or.md),
[`filter_coast()`](https://framverse.github.io/framrsquared/reference/filter_coast.md),
[`filter_marine()`](https://framverse.github.io/framrsquared/reference/filter_marine.md),
[`filter_commercial_wa_nt()`](https://framverse.github.io/framrsquared/reference/filter_commercial_wa_nt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_dataframe |> filter_puget_sound()
} # }
framrosetta::fishery_chinook_fram |> filter_puget_sound(species = "CHINOOK")
#> # A tibble: 36 × 5
#>    species version_number fishery_id fishery_name fishery_title    
#>    <chr>            <int>      <int> <chr>        <chr>            
#>  1 CHINOOK              1         55 Tr 6B:9Net   Tr Area 6B:9 Net 
#>  2 CHINOOK              1         56 A 10 Sport   NT Area 10 Sport 
#>  3 CHINOOK              1         57 A 11 Sport   NT Area 11 Sport 
#>  4 CHINOOK              1         58 NT10:11Net   NT Area 10:11 Net
#>  5 CHINOOK              1         59 Tr10:11Net   Tr Area 10:11 Net
#>  6 CHINOOK              1         60 A 10A Sprt   NT Area 10A Sport
#>  7 CHINOOK              1         61 Tr 10A Net   Tr Area 10A Net  
#>  8 CHINOOK              1         62 A 10E Sprt   NT Area 10E Sport
#>  9 CHINOOK              1         63 Tr 10E Net   Tr Area 10E Net  
#> 10 CHINOOK              1         64 A 12 Sport   NT Area 12 Sport 
#> # ℹ 26 more rows
framrosetta::fishery_coho_fram |> filter_puget_sound(species = "COHO")
#> # A tibble: 91 × 5
#>    species version_number fishery_id fishery_name fishery_title                 
#>    <chr>            <int>      <int> <chr>        <chr>                         
#>  1 COHO                 1         76 Mak FW Spt   Makah Tributary Sport         
#>  2 COHO                 1         77 Mak FW Net   Makah Freshwater Net          
#>  3 COHO                 1         78 Makah  C&S   Makah C&S                     
#>  4 COHO                 1         79 A 4-4A Net   WA Area 4-4A Net              
#>  5 COHO                 1         80 A4B6CNetNT   WA Area 4B-5-6C Non-Treaty Net
#>  6 COHO                 1         81 A4B6CNetTR   WA Area 4B-5-6C Treaty Net    
#>  7 COHO                 1         82 Ar6D NetNT   6D Non-Treaty Net (Dungeness …
#>  8 COHO                 1         83 Ar6D NetTR   6D Treaty Net (Dungeness Bay …
#>  9 COHO                 1         84 Elwha  Net   Elwha R Net                   
#> 10 COHO                 1         85 WJDF T Net   West JDF Straits Trib Net     
#> # ℹ 81 more rows
```
