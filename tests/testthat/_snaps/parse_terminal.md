# terminal_fisheries() returns correct results

    Code
      result
    Output
      # A tibble: 2 x 3
        taa_name    fishery_label              fishery_id
        <chr>       <chr>                           <dbl>
      1 TAA Group A Ore Private Hat Term Catch         10
      2 TAA Group A Coos Bay Troll                     20

# terminal_stocks() returns correct results

    Code
      result
    Output
      # A tibble: 2 x 5
        taa_name    stock_label           terminal_months stock_id terminal_time_steps
        <chr>       <chr>                 <glue>             <dbl> <glue>             
      1 TAA Group A Nooksack River Wild ~ Oct-Apr                1 1-2                
      2 TAA Group A Nooksack River Wild ~ Oct-Apr                2 1-2                

# terminal_info() provides consistent results on coho database

    Code
      result
    Output
      # A tibble: 3,112 x 8
         taa_name     taa_num stock_label stock_id terminal_time_steps terminal_months
         <chr>          <int> <chr>          <dbl> <glue>              <glue>         
       1 Skagit NT T~       1 Skagit Riv~       17 4-5                 September-Dece~
       2 Skagit NT T~       1 Skagit Riv~       17 4-5                 September-Dece~
       3 Skagit NT T~       1 Skagit Riv~       17 4-5                 September-Dece~
       4 Skagit NT T~       1 Skagit Riv~       17 4-5                 September-Dece~
       5 Skagit NT T~       1 Skagit Riv~       17 4-5                 September-Dece~
       6 Skagit NT T~       1 Skagit Riv~       17 4-5                 September-Dece~
       7 Skagit NT T~       1 Skagit Riv~       17 4-5                 September-Dece~
       8 Skagit NT T~       1 Skagit Riv~       18 4-5                 September-Dece~
       9 Skagit NT T~       1 Skagit Riv~       18 4-5                 September-Dece~
      10 Skagit NT T~       1 Skagit Riv~       18 4-5                 September-Dece~
      # i 3,102 more rows
      # i 2 more variables: fishery_label <chr>, fishery_id <dbl>

# terminal_stocks() provides consistent results on coho database

    Code
      result
    Output
      # A tibble: 326 x 5
         taa_name      stock_label        terminal_months stock_id terminal_time_steps
         <chr>         <chr>              <glue>             <dbl> <glue>             
       1 Skagit NT TAA Skagit River Wild~ September-Dece~       17 4-5                
       2 Skagit NT TAA Skagit River Wild~ September-Dece~       18 4-5                
       3 Skagit NT TAA Skagit River Hatc~ September-Dece~       19 4-5                
       4 Skagit NT TAA Skagit River Hatc~ September-Dece~       20 4-5                
       5 Skagit NT TAA Baker (Skagit) Ha~ September-Dece~       21 4-5                
       6 Skagit NT TAA Baker (Skagit) Ha~ September-Dece~       22 4-5                
       7 Skagit NT TAA Baker (Skagit) Wi~ September-Dece~       23 4-5                
       8 Skagit NT TAA Baker (Skagit) Wi~ September-Dece~       24 4-5                
       9 Skagit NT TAA Swinomish Channel~ September-Dece~       25 4-5                
      10 Skagit NT TAA Swinomish Channel~ September-Dece~       26 4-5                
      # i 316 more rows

# terminal_fisheries() provides consistent results on coho database

    Code
      result
    Output
      # A tibble: 203 x 3
         taa_name           fishery_label                           fishery_id
         <chr>              <chr>                                        <dbl>
       1 Skagit NT TAA      WA Area 8 Non-Treaty Net (Skagit)              101
       2 Skagit NT TAA      WA Area 8 Treaty Net (Skagit)                  102
       3 Skagit NT TAA      Skagit R Net                                   103
       4 Skagit NT TAA      Skagit River Test Net                          104
       5 Skagit NT TAA      Swinomish Channel Net                          105
       6 Skagit NT TAA      WA Area 8.1 Sport (Skagit Bay)                 106
       7 Skagit NT TAA      Skagit R Sport                                 108
       8 8D Stilly-Snoh TAA WA Area 8A Non-Treaty Net                      109
       9 8D Stilly-Snoh TAA WA Area 8A Treaty Net                          110
      10 8D Stilly-Snoh TAA WA Area 8D Non-Treaty Net (Tulalip Bay)        111
      # i 193 more rows

