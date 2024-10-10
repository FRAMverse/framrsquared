# framrsquared
<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/FRAMverse/framrsquared/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FRAMverse/framrsquared/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/framrsquared)](https://CRAN.R-project.org/package=framrsquared)
<!-- badges: end -->

## Overview

framrsquared is a library with a focus on easing the burden of working in the FRAM database by providing convenient methods to import tables into R, and producing similar analyses as the FRAM executable while extending it some circumstances.

## Installation

``` r
devtools::install_github("FRAMverse/framrsquared")

# Alternatively 
remotes::install_github("FRAMverse/framrsquared")
```

## Connecting to a FRAM database
```r
library(framrsquared)

fram_db <- connect_fram_db("<path_to_database>")

✔ Successfully connected to FRAM database
Database Species: COHO
Run Count:  34
Last Run Date:  Wed, April 10, 2024 05:42 PM
Last Run Name:  bc-Coho2425
Last Modify Date:  Wed, April 10, 2024 05:41 PM

```

## Importing FRAM tables into R
Imported tables from the FRAM database are automatically converted to [tibbles](https://github.com/tidyverse/tibble) and column names are converted to snakecase.
```r
time_step <- fram_db |> 
  fetch_table('TimeStep')

time_step

# A tibble: 9 × 5
  species version_number time_step_id time_step_name time_step_title   
  <chr>            <int>        <int> <chr>          <chr>             
1 COHO                 1            1 Jan-Jun        January - June    
2 COHO                 1            2 July           July              
3 COHO                 1            3 August         August            
4 COHO                 1            4 Septmbr        September         
5 COHO                 1            5 Oct-Dec        October - December
6 CHINOOK              1            1 Oct-Apr-1      October-April-1   
7 CHINOOK              1            2 May-June       May - June        
8 CHINOOK              1            3 July-Sept      July - September  
9 CHINOOK              1            4 Oct-Apr-2      October-April-2 
```

## Dynamic and Additive Fisheries Filtering
Filtering functions in this package are agnostic to species and designed to retain the same syntax between Chinook FRAM databases and coho FRAM databases.

State level filters:
- `filter_wa()` Filters fisheries to Washington
- `filter_bc()` Filters fisheries to Canada
- `filter_ca()` Filters fisheries to California
- `filter_or()` Filters fisheries to Oregon
- `filter_or()` Filters fisheries to Alaska

Gear level filters:
- `filter_net()` Filters to net fisheries
- `filter_sport()` Filters to sport fisheries

Regional level filters;
- `filter_marine()` Filters to marine fisheries
- `filter_coast()` Filters to coastal fisheries
- `filter_puget_sound()` Filters to Puget Sound Fisheries

```r
# coho database
fram_db_coho <- connect_fram_db('<path_to_coho_database>')

fram_db_coho |> 
  fetch_table('Fishery') |>
  filter_marine() |>
  filter_puget_sound() |>
  filter_sport()

  # A tibble: 10 × 5
   species version_number fishery_id fishery_name fishery_title                     
   <chr>            <int>      <int> <chr>        <chr>                             
 1 COHO                 1         91 Area 5 Spt   WA Area 5 Sport (Sekiu)           
 2 COHO                 1         92 Area 6 Spt   WA Area 6 Sport (Port Angeles)    
 3 COHO                 1         93 Area 7 Spt   WA Area 7 Sport (San Juan Islands)
 4 COHO                 1        106 Ar 8-1 Spt   WA Area 8.1 Sport (Skagit Bay)    
 5 COHO                 1        107 Area 9 Spt   WA Area 9 Sport (Admirality Inlet)
 6 COHO                 1        115 Ar 8-2 Spt   WA Area 8.2 Sport (Everett)       
 7 COHO                 1        118 Ar 10  Spt   WA Area 10 Sport (Seattle)        
 8 COHO                 1        129 Ar 11  Spt   WA Area 11 Sport (Tacoma)         
 9 COHO                 1        136 Ar 13  Spt   WAArea 13 Marine Sport            
10 COHO                 1        152 Ar 12  Spt   Area 12 Marine Sport   

# chinook database
fram_db_chinook <- connect_fram_db('<path_to_chinook_database>')

fram_db_chinook |> 
  fetch_table('Fishery') |>
  filter_marine() |>
  filter_puget_sound() |>
  filter_sport()

# A tibble: 12 × 5
   species version_number fishery_id fishery_name fishery_title    
   <chr>            <int>      <int> <chr>        <chr>            
 1 CHINOOK              1         56 A 10 Sport   NT Area 10 Sport 
 2 CHINOOK              1         57 A 11 Sport   NT Area 11 Sport 
 3 CHINOOK              1         60 A 10A Sprt   NT Area 10A Sport
 4 CHINOOK              1         62 A 10E Sprt   NT Area 10E Sport
 5 CHINOOK              1         64 A 12 Sport   NT Area 12 Sport 
 6 CHINOOK              1         67 A 13 Sport   NT Area 13 Sport 
 7 CHINOOK              1         36 Ar 7 Sport   NT Area 7 Sport  
 8 CHINOOK              1         42 Ar 5 Sport   NT Area 5 Sport  
 9 CHINOOK              1         45 Ar 8-1 Spt   NT Area 8-1 Sport
10 CHINOOK              1         48 Area8D Spt   NT Area 8D Sport 
11 CHINOOK              1         53 Ar 9 Sport   NT Area 9 Sport  
12 CHINOOK              1         54 Ar 6 Sport   NT Area 6 Sport  

# disconnect databases
disconnect_fram_db(fram_db_coho)
✔ Successfully disconnected from FRAM database (fram_db_coho)

disconnect_fram_db(fram_db_chinook)
✔ Successfully disconnected from FRAM database (fram_db_chinook)
```
## Auditing Tools
This family of functions compares two different runs, returning tibbles of detected differences.

Input specific:
- `compare_fishery_inputs()` Compares fishery inputs `compare_fishery_input_flags()` is a variant to compare flags

- `compare_recruits()` Compares recruits between two runs
- `compare_non_retention_inputs()` Compares non-retention inputs `compare_non_retention_input_flags()` is a variant to compare flags
- `compare_stock_fishery_rate_scalers()` Compares the stock fishery rate scalers
- `compare_databases()` Compares all relevant tables of two FRAM databases and generates diagnostics list of differences and their magnitude. Useful for QAQC.

Reporting to console:
- `compare_runs()` Generates a report to the console with all of the outputs from the functions above
```r
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

