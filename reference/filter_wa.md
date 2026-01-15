# Filters a dataframe to Washington State fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to Washington State fisheries. Will automatically
detect whether it's working with a Chinook or Coho dataset if the tables
were generated within this package. `.data` must have a `fishery_id`
column name.

## Usage

``` r
filter_wa(.data, species = NULL)
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
[`filter_ak()`](https://framverse.github.io/framrsquared/reference/filter_ak.md),
[`filter_ca()`](https://framverse.github.io/framrsquared/reference/filter_ca.md),
[`filter_or()`](https://framverse.github.io/framrsquared/reference/filter_or.md),
\#'
[`filter_coast()`](https://framverse.github.io/framrsquared/reference/filter_coast.md),
[`filter_marine()`](https://framverse.github.io/framrsquared/reference/filter_marine.md),
[`filter_commercial_wa_nt()`](https://framverse.github.io/framrsquared/reference/filter_commercial_wa_nt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_dataframe |> filter_wa()
} # }
framrosetta::fishery_chinook_fram |> filter_wa(species = "CHINOOK")
#> # A tibble: 56 × 5
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
#> # ℹ 46 more rows
framrosetta::fishery_coho_fram |> filter_wa(species = "COHO")
#> # A tibble: 144 × 5
#>    species version_number fishery_id fishery_name fishery_title                 
#>    <chr>            <int>      <int> <chr>        <chr>                         
#>  1 COHO                 1         23 Buoy10 Spt   Col. Rvr. Buoy 10 Sport       
#>  2 COHO                 1         24 L ColR Spt   Col. Rvr. Lower R Sport       
#>  3 COHO                 1         25 L ColR Net   Col. Rvr. Lower R Net         
#>  4 COHO                 1         26 Yngs B Net   Col. Rvr. Youngs Bay Net      
#>  5 COHO                 1         27 LCROrT Spt   Col. Rvr. Ore Trib Spt        
#>  6 COHO                 1         28 Clackm Spt   Clackamas R Sport             
#>  7 COHO                 1         29 SandyR Spt   Sandy R Sport                 
#>  8 COHO                 1         30 LCRWaT Spt   Col. Rvr. Wash Trib Spt       
#>  9 COHO                 1         31 UpColR Spt   Col. Rvr. Sport Above Bonnevi…
#> 10 COHO                 1         32 UpColR Net   Col. Rvr. Net Above Bonneville
#> # ℹ 134 more rows
```
