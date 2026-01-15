# Filters a dataframe to Canadian (BC) fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to Canadian (BC) fisheries. Will automatically
detect whether it's working with a Chinook or Coho dataset if the tables
were generated within this package. `.data` must have a `fishery_id`
column name.

## Usage

``` r
filter_bc(.data, species = NULL)
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
[`filter_ak()`](https://framverse.github.io/framrsquared/reference/filter_ak.md),
[`filter_wa()`](https://framverse.github.io/framrsquared/reference/filter_wa.md),
[`filter_ca()`](https://framverse.github.io/framrsquared/reference/filter_ca.md),
[`filter_or()`](https://framverse.github.io/framrsquared/reference/filter_or.md),
[`filter_coast()`](https://framverse.github.io/framrsquared/reference/filter_coast.md),
[`filter_marine()`](https://framverse.github.io/framrsquared/reference/filter_marine.md),
[`filter_commercial_wa_nt()`](https://framverse.github.io/framrsquared/reference/filter_commercial_wa_nt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_dataframe |> filter_bc()
} # }
framrosetta::fishery_chinook_fram |> filter_bc(species = "CHINOOK")
#> # A tibble: 12 × 5
#>    species version_number fishery_id fishery_name fishery_title            
#>    <chr>            <int>      <int> <chr>        <chr>                    
#>  1 CHINOOK              1          4 N/C BC Net   BC No/Cent Net           
#>  2 CHINOOK              1          5 WCVI Net     BC WCVI Net              
#>  3 CHINOOK              1          6 GeoStr Net   BC Georgia Strait Net    
#>  4 CHINOOK              1          7 BC JDF Net   BC JDF Net               
#>  5 CHINOOK              1          8 BCOutSport   BC Outside Sport         
#>  6 CHINOOK              1          9 N/C BC Trl   BC No/Cent Troll         
#>  7 CHINOOK              1         10 WCVI Troll   BC WCVI Troll            
#>  8 CHINOOK              1         11 WCVI Sport   BC WCVI Sport            
#>  9 CHINOOK              1         12 GeoS Troll   BC Georgia Strait Troll  
#> 10 CHINOOK              1         13 N GS Sport   BC N Georgia Strait Sport
#> 11 CHINOOK              1         14 S GS Sport   BC S Georgia Strait Sport
#> 12 CHINOOK              1         15 BC JDF Spt   BC JDF Sport             
framrosetta::fishery_coho_fram |> filter_bc(species = "COHO")
#> # A tibble: 27 × 5
#>    species version_number fishery_id fishery_name fishery_title            
#>    <chr>            <int>      <int> <chr>        <chr>                    
#>  1 COHO                 1        167 FRSLOW Trm   Lower Fraser R Term Catch
#>  2 COHO                 1        168 FRSUPP Trm   Upper Fraser R Term Catch
#>  3 COHO                 1        169 Fraser Spt   Lower Fraser River Sport 
#>  4 COHO                 1        170 JStrBC Trl   Johnstone Strait Troll   
#>  5 COHO                 1        171 No BC  Trl   BC Northern Troll        
#>  6 COHO                 1        172 NoC BC Trl   BC North Central Troll   
#>  7 COHO                 1        173 SoC BC Trl   BC South Central Troll   
#>  8 COHO                 1        174 NW VI  Trl   NW Vancouver Island Troll
#>  9 COHO                 1        175 SW VI  Trl   SW Vancouver Island Troll
#> 10 COHO                 1        176 GeoStr Trl   Georgia Straits Troll    
#> # ℹ 17 more rows
```
