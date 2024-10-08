#' Fetches a complete table from a FRAM database. Returns a cleaned
#' tibble.
#' @param fram_db FRAM database object
#' @param table_name Table to be fetched. If not given, a list of options will be printed
#' @export
#' @examples
#' \dontrun{fram_db |> fetch_table('Mortality')}
#'

fetch_table <- function(fram_db, table_name = NULL){
  ## adding input checking
  validate_fram_db(fram_db)
  all_tables = provide_table_names(is_full = TRUE)
  limited_tables = provide_table_names(is_full = FALSE)

  if (is.null(table_name)) {
    if (fram_db$fram_db_type == 'full') {
      cli::cli_alert_info('A table name must be provided, see available options:')
      fmt <- cli::ansi_columns(
        all_tables,
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
        limited_tables,
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
                                     all_tables)
    } else {
      table_name <- rlang::arg_match(table_name,
                                     limited_tables)
    }
    output_table <- DBI::dbGetQuery(fram_db$fram_db_connection,
                                    glue::glue('SELECT * FROM {table_name};')) |>
      fram_clean_tables()
    attr(output_table, 'species') <- fram_db$fram_db_species
    return(output_table)
  }
}


#' List names of FRAM table
#'
#' Provides list of FRAm database names, typically useful for internal functions.
#'
#' @param is_full Logical. Provide names for a full FRAM database (TRUE) or a model transfer (FALSE)?
#'
#' @return Character string of the names of FRAM tables
#' @export
#'
#' @examples
#' provide_table_names(is_full = FALSE)

provide_table_names <- function(is_full = TRUE){
  if(!is.logical(is_full)){
    cli::cli_abort("`is_full` must be TRUE or FALSE, not `{is_full}`")
  }
 if(is_full){ ## list of possible table names from a full table
   c('AEQ',
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
 } else {
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
 }
}

