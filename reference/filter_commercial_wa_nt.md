# Filters a dataframe to WA non-treaty commercial fisheries. Will automatically detect whether it's working with a Chinook or Coho dataset if the tables were generated within this package. `.data` must have a `fishery_id` column name.

Filters a dataframe to WA non-treaty commercial fisheries. Will
automatically detect whether it's working with a Chinook or Coho dataset
if the tables were generated within this package. `.data` must have a
`fishery_id` column name.

## Usage

``` r
filter_commercial_wa_nt(.data, species = NULL)
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
[`filter_marine()`](https://framverse.github.io/framrsquared/reference/filter_marine.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fram_dataframe |> filter_commercial_wa_NT()
} # }
framrosetta::fishery_chinook_fram |> filter_commercial_wa_nt(species = "CHINOOK")
#> # A tibble: 10 × 5
#>    species version_number fishery_id fishery_name fishery_title      
#>    <chr>            <int>      <int> <chr>        <chr>              
#>  1 CHINOOK              1         58 NT10:11Net   NT Area 10:11 Net  
#>  2 CHINOOK              1         65 NT HC Net    NT Hood Canal Net  
#>  3 CHINOOK              1         68 NT SPS Net   NT SPS Net         
#>  4 CHINOOK              1         70 NT 13A Net   NT Area 13A Net    
#>  5 CHINOOK              1         37 NT 7:7ANet   NT Area 6A:7:7A Net
#>  6 CHINOOK              1         39 NT 7BCDNet   NT Area 7B-7D Net  
#>  7 CHINOOK              1         43 NT JDF Net   NT JDF Net         
#>  8 CHINOOK              1         46 NT SkagNet   NT Skagit Net      
#>  9 CHINOOK              1         49 NT StSnNet   NT St/Snohomish Net
#> 10 CHINOOK              1         51 NT TulaNet   NT Tulalip Bay Net 
framrosetta::fishery_coho_fram |> filter_commercial_wa_nt(species = "COHO")
#> # A tibble: 20 × 5
#>    species version_number fishery_id fishery_name fishery_title                 
#>    <chr>            <int>      <int> <chr>        <chr>                         
#>  1 COHO                 1         82 Ar6D NetNT   6D Non-Treaty Net (Dungeness …
#>  2 COHO                 1         87 A6-7ANetNT   WA Area 7-7A Non-Treaty Net   
#>  3 COHO                 1         96 A7BCDNetNT   WA Area 7B-7C-7D Non-Treaty N…
#>  4 COHO                 1        101 Ar 8 NetNT   WA Area 8 Non-Treaty Net (Ska…
#>  5 COHO                 1        109 Ar8A NetNT   WA Area 8A Non-Treaty Net     
#>  6 COHO                 1        111 Ar8D NetNT   WA Area 8D Non-Treaty Net (Tu…
#>  7 COHO                 1        119 Ar10 NetNT   WA Area 10 Non-Treaty Net (Se…
#>  8 COHO                 1        121 Ar10ANetNT   WA Area 10A Non-Treaty Net (E…
#>  9 COHO                 1        123 Ar10ENetNT   WA Area 10E Non-Treaty Net (E…
#> 10 COHO                 1        130 Ar11 NetNT   WA Area 11 Non-Treaty Net (E/…
#> 11 COHO                 1        132 Ar11ANetNT   WA Area 11A Non-Treaty Net (C…
#> 12 COHO                 1        137 Ar13 NetNT   Area 13 Non-Treaty Net (So Pu…
#> 13 COHO                 1        139 Ar13CNetNT   Area 13C Non-Treaty Net (Cham…
#> 14 COHO                 1        141 Ar13ANetNT   Area 13A Non-Treaty Net (Carr…
#> 15 COHO                 1        143 Ar13DNetNT   Area 13D Non-Treaty Net       
#> 16 COHO                 1        145 A13FKNetNT   Area 13F-13K Non-Treaty Net   
#> 17 COHO                 1        153 1212BNetNT   Area 12-12B Hood Canal Non-Tr…
#> 18 COHO                 1        155 A9-9ANetNT   Area 9/9A Non-Treaty Net      
#> 19 COHO                 1        157 Ar12ANetNT   Area 12A Non-Treaty Net (Quil…
#> 20 COHO                 1        159 A12CDNetNT   Area 12C-12D Non-Treaty Net (…
```
