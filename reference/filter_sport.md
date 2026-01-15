# Filters a dataframe to sport fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to sport fisheries. Will automatically detect
whether it's working with a Chinook or Coho dataset if the tables were
generated within this package. `.data` must have a `fishery_id` column
name.

## Usage

``` r
filter_sport(.data, species = NULL)
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

[`filter_net()`](https://framverse.github.io/framrsquared/reference/filter_net.md),
[`filter_puget_sound()`](https://framverse.github.io/framrsquared/reference/filter_puget_sound.md),
[`filter_wa()`](https://framverse.github.io/framrsquared/reference/filter_wa.md),
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
fram_dataframe |> filter_sport()
} # }
framrosetta::fishery_chinook_fram |> filter_sport(species = "CHINOOK")
#> # A tibble: 26 × 5
#>    species version_number fishery_id fishery_name fishery_title    
#>    <chr>            <int>      <int> <chr>        <chr>            
#>  1 CHINOOK              1         56 A 10 Sport   NT Area 10 Sport 
#>  2 CHINOOK              1         57 A 11 Sport   NT Area 11 Sport 
#>  3 CHINOOK              1         60 A 10A Sprt   NT Area 10A Sport
#>  4 CHINOOK              1         62 A 10E Sprt   NT Area 10E Sport
#>  5 CHINOOK              1         64 A 12 Sport   NT Area 12 Sport 
#>  6 CHINOOK              1         67 A 13 Sport   NT Area 13 Sport 
#>  7 CHINOOK              1         72 FW Sport     Freshwater Sport 
#>  8 CHINOOK              1          3 SEAK Sport   SE Alaska Sport  
#>  9 CHINOOK              1          8 BCOutSport   BC Outside Sport 
#> 10 CHINOOK              1         11 WCVI Sport   BC WCVI Sport    
#> # ℹ 16 more rows
framrosetta::fishery_coho_fram |> filter_sport(species = "COHO")
#> # A tibble: 70 × 5
#>    species version_number fishery_id fishery_name fishery_title          
#>    <chr>            <int>      <int> <chr>        <chr>                  
#>  1 COHO                 1          3 Ft Brg Spt   Fort Bragg Sport       
#>  2 COHO                 1          5 Ca KMZ Spt   KMZ Sport              
#>  3 COHO                 1          7 So Cal Spt   So Calif. Sport        
#>  4 COHO                 1         15 Brkngs Spt   Brookings Sport        
#>  5 COHO                 1         17 Newprt Spt   Newport Sport          
#>  6 COHO                 1         19 Coos B Spt   Coos Bay Sport         
#>  7 COHO                 1         21 Tillmk Spt   Tillamook Sport        
#>  8 COHO                 1         23 Buoy10 Spt   Col. Rvr. Buoy 10 Sport
#>  9 COHO                 1         24 L ColR Spt   Col. Rvr. Lower R Sport
#> 10 COHO                 1         28 Clackm Spt   Clackamas R Sport      
#> # ℹ 60 more rows
```
