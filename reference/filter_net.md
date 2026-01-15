# Filters a dataframe to net fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to net fisheries. Will automatically detect whether
it's working with a Chinook or Coho dataset if the tables were generated
within this package. `.data` must have a `fishery_id` column name.

## Usage

``` r
filter_net(.data, species = NULL)
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
[`filter_puget_sound()`](https://framverse.github.io/framrsquared/reference/filter_puget_sound.md),
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
fram_dataframe |> filter_net()
} # }
framrosetta::fishery_chinook_fram |> filter_net(species = "CHINOOK")
#> # A tibble: 47 × 5
#>    species version_number fishery_id fishery_name fishery_title    
#>    <chr>            <int>      <int> <chr>        <chr>            
#>  1 CHINOOK              1         55 Tr 6B:9Net   Tr Area 6B:9 Net 
#>  2 CHINOOK              1         58 NT10:11Net   NT Area 10:11 Net
#>  3 CHINOOK              1         59 Tr10:11Net   Tr Area 10:11 Net
#>  4 CHINOOK              1         61 Tr 10A Net   Tr Area 10A Net  
#>  5 CHINOOK              1         63 Tr 10E Net   Tr Area 10E Net  
#>  6 CHINOOK              1         65 NT HC Net    NT Hood Canal Net
#>  7 CHINOOK              1         66 Tr HC Net    Tr Hood Canal Net
#>  8 CHINOOK              1         68 NT SPS Net   NT SPS Net       
#>  9 CHINOOK              1         69 Tr SPS Net   Tr SPS Net       
#> 10 CHINOOK              1         70 NT 13A Net   NT Area 13A Net  
#> # ℹ 37 more rows
framrosetta::fishery_coho_fram |> filter_net(species = "COHO")
#> # A tibble: 128 × 5
#>    species version_number fishery_id fishery_name fishery_title              
#>    <chr>            <int>      <int> <chr>        <chr>                      
#>  1 COHO                 1          1 No Cal Trm   No Calif Cst Terminal Catch
#>  2 COHO                 1          2 Cn Cal Trm   Cntrl Cal Cst Term Catch   
#>  3 COHO                 1          4 Ft Brg Trl   Fort Bragg Troll           
#>  4 COHO                 1          6 Ca KMZ Trl   KMZ Troll                  
#>  5 COHO                 1          8 So Cal Trl   So Calif. Troll            
#>  6 COHO                 1          9 So Ore Trm   So Ore Coast Terminal Catch
#>  7 COHO                 1         10 Or Prv Trm   Ore Private Hat Term Catch 
#>  8 COHO                 1         11 SMi Or Trm   So Mid Ore Coast Term Catch
#>  9 COHO                 1         12 NMi Or Trm   No Mid Ore Coast Term Catch
#> 10 COHO                 1         13 No Ore Trm   North Ore Coast Term Catch 
#> # ℹ 118 more rows
```
