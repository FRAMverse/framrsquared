# List names of FRAM table

Provides list of FRAm database names, typically useful for internal
functions.

## Usage

``` r
provide_table_names(is_full = TRUE)
```

## Arguments

- is_full:

  Logical. Provide names for a full FRAM database (TRUE) or a model
  transfer (FALSE)?

## Value

Character string of the names of FRAM tables

## Examples

``` r
provide_table_names(is_full = FALSE)
#>  [1] "BackwardsFRAM"          "BaseID"                 "Cohort"                
#>  [4] "Escapement"             "FisheryMortality"       "FisheryScalers"        
#>  [7] "Mortality"              "NonRetention"           "PSCMaxER"              
#> [10] "RunID"                  "SizeLimits"             "SLRatio"               
#> [13] "StockFisheryRateScaler" "StockRecruit"           "TAAETRSList"           
```
