# Filters a dataframe to marine fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to marine fisheries. Will automatically detect
whether it's working with a Chinook or Coho dataset if the tables were
generated within this package. `.data` must have a `fishery_id` column
name.

## Usage

``` r
filter_marine(.data, species = NULL)
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
[`filter_coast()`](https://framverse.github.io/framrsquared/reference/filter_coast.md),
[`filter_commercial_wa_nt()`](https://framverse.github.io/framrsquared/reference/filter_commercial_wa_nt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_dataframe |> filter_marine()
} # }
framrosetta::fishery_chinook_fram |> filter_marine(species = "CHINOOK")
#> # A tibble: 71 × 5
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
#> # ℹ 61 more rows
framrosetta::fishery_coho_fram |> filter_marine(species = "COHO")
#> # A tibble: 115 × 5
#>    species version_number fishery_id fishery_name fishery_title   
#>    <chr>            <int>      <int> <chr>        <chr>           
#>  1 COHO                 1          3 Ft Brg Spt   Fort Bragg Sport
#>  2 COHO                 1          4 Ft Brg Trl   Fort Bragg Troll
#>  3 COHO                 1          5 Ca KMZ Spt   KMZ Sport       
#>  4 COHO                 1          6 Ca KMZ Trl   KMZ Troll       
#>  5 COHO                 1          7 So Cal Spt   So Calif. Sport 
#>  6 COHO                 1          8 So Cal Trl   So Calif. Troll 
#>  7 COHO                 1         15 Brkngs Spt   Brookings Sport 
#>  8 COHO                 1         16 Brkngs Trl   Brookings Troll 
#>  9 COHO                 1         17 Newprt Spt   Newport Sport   
#> 10 COHO                 1         18 Newprt Trl   Newport Troll   
#> # ℹ 105 more rows
```
