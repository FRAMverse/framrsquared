# Filters a dataframe to Oregon fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to Oregon fisheries. Will automatically detect
whether it's working with a Chinook or Coho dataset if the tables were
generated within this package. `.data` must have a `fishery_id` column
name.

## Usage

``` r
filter_or(.data, species = NULL)
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
[`filter_coast()`](https://framverse.github.io/framrsquared/reference/filter_coast.md),
[`filter_marine()`](https://framverse.github.io/framrsquared/reference/filter_marine.md),
[`filter_commercial_wa_nt()`](https://framverse.github.io/framrsquared/reference/filter_commercial_wa_nt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_dataframe |> filter_or()
} # }
framrosetta::fishery_chinook_fram |> filter_or(species = "CHINOOK")
#> # A tibble: 6 × 5
#>   species version_number fishery_id fishery_name fishery_title     
#>   <chr>            <int>      <int> <chr>        <chr>             
#> 1 CHINOOK              1         28 ColRvr Net   Columbia River Net
#> 2 CHINOOK              1         29 Buoy10 Spt   Buoy 10 Sport     
#> 3 CHINOOK              1         30 Cen OR Trl   Central OR Troll  
#> 4 CHINOOK              1         31 Cen OR Spt   Central OR Sport  
#> 5 CHINOOK              1         32 KMZ Troll    KMZ Troll         
#> 6 CHINOOK              1         33 KMZ Sport    KMZ Sport         
framrosetta::fishery_coho_fram |> filter_or(species = "COHO")
#> # A tibble: 28 × 5
#>    species version_number fishery_id fishery_name fishery_title              
#>    <chr>            <int>      <int> <chr>        <chr>                      
#>  1 COHO                 1          5 Ca KMZ Spt   KMZ Sport                  
#>  2 COHO                 1          6 Ca KMZ Trl   KMZ Troll                  
#>  3 COHO                 1          7 So Cal Spt   So Calif. Sport            
#>  4 COHO                 1          8 So Cal Trl   So Calif. Troll            
#>  5 COHO                 1          9 So Ore Trm   So Ore Coast Terminal Catch
#>  6 COHO                 1         10 Or Prv Trm   Ore Private Hat Term Catch 
#>  7 COHO                 1         11 SMi Or Trm   So Mid Ore Coast Term Catch
#>  8 COHO                 1         12 NMi Or Trm   No Mid Ore Coast Term Catch
#>  9 COHO                 1         13 No Ore Trm   North Ore Coast Term Catch 
#> 10 COHO                 1         14 Or Cst Trm   Oregon Coast Term Catch    
#> # ℹ 18 more rows
```
