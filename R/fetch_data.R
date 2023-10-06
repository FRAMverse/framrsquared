fetch_table <- function(fram_db, table_name){
  if(fram_db$fram_db_type == 'full'){
    table_name <- rlang::arg_match(table_name,
                                c(
                                  'AEQ',
                                  'BackwardsFRAM',
                                  'BaseCohort',
                                  'BaseExploitationRate',
                                  'BaseID',
                                  'ChinookBaseEncounterAdjustment',
                                  'ChinookBaseSizeLimit',
                                  'Cohort',
                                  'EncounterRateAdjustment',
                                  'Escapement',
                                  'Fishery',
                                  'FisheryModelStockProportion',
                                  'FisheryMortality',
                                  'FisheryScalers',
                                  'Growth',
                                  'IncidentalRate',
                                  'MaturationRate',
                                  'Mortality',
                                  'NaturalMortality',
                                  'NonRetention',
                                  'PSCMaxER',
                                  'ReportDriver',
                                  'RunEncounterRateAdjustment',
                                  'RunID',
                                  'ShakerMortRate',
                                  'SizeLimits',
                                  'SLRatio',
                                  'Stock',
                                  'StockFisheryRateScaler',
                                  'StockRecruit',
                                  'TAAETRSList',
                                  'TerminalFisheryFlag',
                                  'TimeStep'
                                )
                              )
  } else {
    table_name <- rlang::arg_match(table_name,
                                   c(
                                     'BackwardsFRAM',
                                     'BaseID',
                                     'Cohort',
                                     'Escapement',
                                     'FisheryMortality',
                                     'FisheryScalers',
                                     'Mortality',
                                     'NonRetention',
                                     'PSCMaxER',
                                     'RunID',
                                     'SizeLimits',
                                     'SLRatio',
                                     'StockFisheryRateScaler',
                                     'StockRecruit',
                                     'TAAETRSList'
                                   )
    )
  }

  if(DBI::dbIsValid(fram_db$fram_db_connection)){
    output_table <- DBI::dbGetQuery(fram_db$fram_db_connection,
                    glue::glue('SELECT * FROM {table_name};')) |>
      fram_clean_tables()

    attr(output_table, 'species') <- fram_db$fram_db_species

    return(output_table)

  } else {
    rlang::abort('Connect to a FRAM database first...')
  }

}
