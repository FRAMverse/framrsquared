# Filters a dataframe to Coastal fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to Coastal fisheries. Will automatically detect
whether it's working with a Chinook or Coho dataset if the tables were
generated within this package. `.data` must have a `fishery_id` column
name.

## Usage

``` r
filter_coast(.data, species = NULL)
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
[`filter_bc()`](https://framverse.github.io/framrsquared/reference/filter_bc.md),
[`filter_wa()`](https://framverse.github.io/framrsquared/reference/filter_wa.md),
[`filter_ca()`](https://framverse.github.io/framrsquared/reference/filter_ca.md),
[`filter_or()`](https://framverse.github.io/framrsquared/reference/filter_or.md),
[`filter_marine()`](https://framverse.github.io/framrsquared/reference/filter_marine.md),
[`filter_commercial_wa_nt()`](https://framverse.github.io/framrsquared/reference/filter_commercial_wa_nt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_dataframe |> filter_coast()
} # }
framrosetta::fishery_chinook_fram |> filter_coast(species = "CHINOOK")
#> # A tibble: 35 × 5
#>    species version_number fishery_id fishery_name fishery_title        
#>    <chr>            <int>      <int> <chr>        <chr>                
#>  1 CHINOOK              1          1 SEAK Troll   SE Alaska Troll      
#>  2 CHINOOK              1          2 SEAK Net     SE Alaska Net        
#>  3 CHINOOK              1          3 SEAK Sport   SE Alaska Sport      
#>  4 CHINOOK              1          4 N/C BC Net   BC No/Cent Net       
#>  5 CHINOOK              1          5 WCVI Net     BC WCVI Net          
#>  6 CHINOOK              1          6 GeoStr Net   BC Georgia Strait Net
#>  7 CHINOOK              1          7 BC JDF Net   BC JDF Net           
#>  8 CHINOOK              1          8 BCOutSport   BC Outside Sport     
#>  9 CHINOOK              1          9 N/C BC Trl   BC No/Cent Troll     
#> 10 CHINOOK              1         10 WCVI Troll   BC WCVI Troll        
#> # ℹ 25 more rows
framrosetta::fishery_coho_fram |> filter_coast(species = "COHO")
#> # A tibble: 65 × 5
#>    species version_number fishery_id fishery_name fishery_title              
#>    <chr>            <int>      <int> <chr>        <chr>                      
#>  1 COHO                 1          1 No Cal Trm   No Calif Cst Terminal Catch
#>  2 COHO                 1          2 Cn Cal Trm   Cntrl Cal Cst Term Catch   
#>  3 COHO                 1          3 Ft Brg Spt   Fort Bragg Sport           
#>  4 COHO                 1          4 Ft Brg Trl   Fort Bragg Troll           
#>  5 COHO                 1          5 Ca KMZ Spt   KMZ Sport                  
#>  6 COHO                 1          6 Ca KMZ Trl   KMZ Troll                  
#>  7 COHO                 1          7 So Cal Spt   So Calif. Sport            
#>  8 COHO                 1          8 So Cal Trl   So Calif. Troll            
#>  9 COHO                 1          9 So Ore Trm   So Ore Coast Terminal Catch
#> 10 COHO                 1         10 Or Prv Trm   Ore Private Hat Term Catch 
#> # ℹ 55 more rows
```
