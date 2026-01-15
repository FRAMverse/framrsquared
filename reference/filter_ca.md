# Filters a dataframe to California fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to California fisheries. Will automatically detect
whether it's working with a Chinook or Coho dataset if the tables were
generated within this package. `.data` must have a `fishery_id` column
name.

## Usage

``` r
filter_ca(.data, species = NULL)
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
[`filter_or()`](https://framverse.github.io/framrsquared/reference/filter_or.md),
[`filter_coast()`](https://framverse.github.io/framrsquared/reference/filter_coast.md),
[`filter_marine()`](https://framverse.github.io/framrsquared/reference/filter_marine.md),
[`filter_commercial_wa_nt()`](https://framverse.github.io/framrsquared/reference/filter_commercial_wa_nt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_dataframe |> filter_ca()
} # }
framrosetta::fishery_chinook_fram |> filter_ca(species = "CHINOOK")
#> # A tibble: 3 × 5
#>   species version_number fishery_id fishery_name fishery_title  
#>   <chr>            <int>      <int> <chr>        <chr>          
#> 1 CHINOOK              1         32 KMZ Troll    KMZ Troll      
#> 2 CHINOOK              1         33 KMZ Sport    KMZ Sport      
#> 3 CHINOOK              1         34 So Cal Trl   So Calif. Troll
framrosetta::fishery_coho_fram |> filter_ca(species = "COHO")
#> # A tibble: 8 × 5
#>   species version_number fishery_id fishery_name fishery_title              
#>   <chr>            <int>      <int> <chr>        <chr>                      
#> 1 COHO                 1          1 No Cal Trm   No Calif Cst Terminal Catch
#> 2 COHO                 1          2 Cn Cal Trm   Cntrl Cal Cst Term Catch   
#> 3 COHO                 1          3 Ft Brg Spt   Fort Bragg Sport           
#> 4 COHO                 1          4 Ft Brg Trl   Fort Bragg Troll           
#> 5 COHO                 1          5 Ca KMZ Spt   KMZ Sport                  
#> 6 COHO                 1          6 Ca KMZ Trl   KMZ Troll                  
#> 7 COHO                 1          7 So Cal Spt   So Calif. Sport            
#> 8 COHO                 1          8 So Cal Trl   So Calif. Troll            
```
