# auditing_tools

``` r
library(framrsquared)
#> 
#>              .
#>             ":"
#>           ___:____     |"\/"|
#>         ,'        `.    \  /
#>         |  O        \___/ |
#>       ~^~^~^~^~^~^~^~^~^~^~^~^~
#>           framrsquared 0.8.0
#> 
```

## Auditing Tools

We have built a series functions in framrsquared to compare two runs and
identify differences; these can be useful for QAQC.

*Input specific*:

- [`compare_fishery_inputs()`](https://framverse.github.io/framrsquared/reference/compare_fishery_inputs.md)
  compares fishery inputs
  [`compare_fishery_input_flags()`](https://framverse.github.io/framrsquared/reference/compare_fishery_input_flags.md)
  is a variant to compare flags
- [`compare_recruits()`](https://framverse.github.io/framrsquared/reference/compare_recruits.md)
  compares recruits between two runs
- [`compare_non_retention_inputs()`](https://framverse.github.io/framrsquared/reference/compare_non_retention_inputs.md)
  compares non-retention inputs
  [`compare_non_retention_input_flags()`](https://framverse.github.io/framrsquared/reference/compare_non_retention_input_flags.md)
  is a variant to compare flags
- [`compare_stock_fishery_rate_scalers()`](https://framverse.github.io/framrsquared/reference/compare_stock_fishery_rate_scalers.md)
  compares the stock fishery rate scalers

*Reporting to console*:

- [`compare_runs()`](https://framverse.github.io/framrsquared/reference/compare_runs.md)
  Generates a report to the console with all of the outputs from the
  functions above

*FRAM development QAQC*

- [`compare_databases()`](https://framverse.github.io/framrsquared/reference/compare_databases.md)
  compares all relevant tables of two FRAM databases and generates
  diagnostics list of differences and their magnitude. This can be used
  when developing modifications to the FRAM software: after running the
  original version of the software and the modified version of the
  software on the runs of two initially identical databases,
  [`compare_databases()`](https://framverse.github.io/framrsquared/reference/compare_databases.md)
  will identify how the outputs were affected by the modifications in
  the FRAM software.

## Example

Most commonly we will use
[`compare_runs()`](https://framverse.github.io/framrsquared/reference/compare_runs.md)
during the NOF process when developing a new run to confirm that we only
have intended changes to the database.

``` r
fram_db <- connect_fram_db('<path_to_coho_database>')

fram_db |>
    compare_runs(c(56:57))

── Comparing run bc-Coho2423_STT_NOF to bc-Coho2424_NOF ─────────────────────────────────────────────────────────────────────────────────
ℹ bc-Coho2423_STT_NOF was run at 2024-04-09 10:41:30 PM, bc-Coho2424_NOF was run at 2024-04-10 12:40:07 PM

── Non-Retention Inputs ──

── Checking for changes in non-retention flagging 
ℹ Changes detected in non-retention flagging, below is a table outlining them
# A tibble: 4 × 5
  fishery_id time_step fishery_name `bc-Coho2423_STT_NOF` `bc-Coho2424_NOF`
       <int>     <int> <chr>                        <int>             <int>
1        154         4 1212BNetTR                       1                NA
2         36         4 Area2TrlTR                      NA                 1
3         39         4 Area3TrlTR                      NA                 1
4         43         4 A4/4BTrlTR                      NA                 1

── Checking for changes in non-retention inputs 
ℹ Changes detected in non-retention inputs, below is a table outlining them
# A tibble: 7 × 6
  fishery_id time_step name       fishery_name `bc-Coho2423_STT_NOF` `bc-Coho2424_NOF`
       <int>     <int> <chr>      <chr>                        <dbl>             <dbl>
1        154         3 cnr_input1 1212BNetTR                    71.4              36.7
2        154         4 cnr_input1 1212BNetTR                    28.4               0  
3        154         5 cnr_input1 1212BNetTR                     7                 8  
4        156         5 cnr_input1 A9-9ANetTR                    15                18  
5         36         4 cnr_input1 Area2TrlTR                     0                52  
6         39         4 cnr_input1 Area3TrlTR                     0                52  
7         43         4 cnr_input1 A4/4BTrlTR                     0                52  

── Recruit Inputs ──

── Checking for changes to recruits 
✔ No changes detected in recruit inputs

── Fishery Inputs ──

── Checking for changes to fishery flags 
✔ No changes detected in fishery flag inputs

── Checking for changes to fishery inputs 
ℹ Detection tolerance set to: 1%
ℹ Changes detected in fishery inputs, below is a table outlining them
# A tibble: 12 × 6
   fishery_id fishery_name time_step name  `bc-Coho2423_STT_NOF` `bc-Coho2424_NOF`
        <int> <chr>            <int> <chr>                 <dbl>             <dbl>
 1         47 WlpaBT Net           4 quota                 26427             25411
 2         48 GryHbr Spt           4 quota                  2489              2285
 3         49 SGryHb Spt           5 quota                    79                73
 4         50 GryHbr Net           5 quota                  5873              4751
 5         51 Hump R Spt           5 quota                  1551              1505
 6         52 LwCheh Net           5 quota                 24219             26299
 7         54 Chehal Spt           5 quota                 14723             14553
 8         55 Hump R Net           5 quota                  4485              4372
 9         56 UpCheh Net           5 quota                  4267              3844
10         63 Quin R Net           4 quota                 10166              9860
11         68 Queets Net           4 quota                  7486              8079
12         68 Queets Net           5 quota                  2358               170

── Checking for changes to stock fishery rate scalers 
✔ No changes detected in fishery inputs
```
