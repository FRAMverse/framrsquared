#' Fetches a complete table from a FRAM database. Returns a cleaned
#' tibble.
#' @param fram_db FRAM database object
#' @param table_name Table to be fetched
#' @export
#' @examples
#' \dontrun{fram_db |> fetch_table('Mortality')}
#'

fetch_table <- function(fram_db, table_name = NULL){
  ## adding input checking
  if(!rlang::is_list(fram_db) |  !"fram_db_connection" %in% names(fram_db)){
    cli::cli_code('fram_db <- connect_fram_db(file_path)\nfram_db |> fetch_table(\'Mortality\')')
    cli::cli_abort('Invalid database type, try code above')
  }
  if(!DBI::dbIsValid(fram_db$fram_db_connection)){
    cli::cli_abort("Invalid database connection")
  }
  all.tables = c(
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
  limited.tables = c(
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
  if (is.null(table_name)) {
    if (fram_db$fram_db_type == 'full') {
      cli::cli_alert_info('A table name must be provided, see available options:')
      fmt <- cli::ansi_columns(
        all.tables,
        fill = "rows",
        max_cols = 4,
        align = "center",
        sep = ""
      )
      cli::boxx(
        fmt,
        padding = c(0, 1, 0, 1),
        header = cli::col_cyan("FRAM tables (full database)")
      )

    } else{

      fmt <- cli::ansi_columns(
        limited.tables,
        fill = "rows",
        max_cols = 4,
        align = "center",
        sep = "   "
      )
      cli::boxx(
        fmt,
        padding = c(0, 1, 0, 1),
        header = cli::col_cyan("FRAM tables (transfer database)")
      )

    }
  } else{
    if (fram_db$fram_db_type == 'full') {
      table_name <- rlang::arg_match(table_name,
                                     all.tables)
    } else {
      table_name <- rlang::arg_match(table_name,
                                     limited.tables)
    }
    output_table <- DBI::dbGetQuery(fram_db$fram_db_connection,
                                    glue::glue('SELECT * FROM {table_name};')) |>
      fram_clean_tables()
    attr(output_table, 'species') <- fram_db$fram_db_species
    return(output_table)
  }
}
