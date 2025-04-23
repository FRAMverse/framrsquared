#' Fetches a complete table from a FRAM database. Returns a cleaned
#' tibble.
#' @param fram_db FRAM database object
#' @param table_name Table to be fetched. If not given, a list of options will be printed
#' @param warn Print a warning when fetching BackwardsFRAM table from a Chinook database? Defaults to `TRUE`.
#' @export
#' @examples
#' \dontrun{fram_db |> fetch_table('Mortality')}
#'

fetch_table <- function(fram_db, table_name = NULL, warn = TRUE){
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

    if(warn & fram_db$fram_db_species == "CHINOOK" & table_name == "BackwardsFRAM"){
      cli::cli_alert_danger("Chinook BackwardsFRAM tables use different numbering for stock_id!\n This can cause problems when merging with other tables!\n Recommend fetch_table_bkchin() instead.")
    }

    return(output_table)
  }
}

#' Safely fetch Chinook BackwardsFRAM table
#'
#' The BackwardsFRAM table uses a stock_id different numbering system from all other tables, and includes
#' sums of joint stocks (e.g. for a marked and unmarked pair of stocks, BackwardsFRAM will typically have an additional
#' stock which represents the sum of those two). Because the numbering is different but the column name is the same,
#' joining the Chinook BackwardsFRAM table with other tables can cause problems.
#'
#' This function augments fetch_table by renaming the `stock_id` column to `bk_stock_id`, and
#' then adding on the associated stock_id (with NAs when the bkfram stock is one of these new "sum" stocks
#' and the associated bkfram stock names). This function only works for Chinook databases.
#'
#' **The resulting dataframe will necessarily NOT be an exact match to the BackwardsFRAM table in the FRAM database. The stock_id column will differ (containing normal stock ID values instead of bk stock ID values), and there will be two additional columns.**
#'
#' @param fram_db FRAM database object
#'
#' @export
#'
#' @examples
#' #' @examples
#' \dontrun{
#' ##Potentially problematic stock_id won't align with other tables
#' fram_db |> fetch_table('BackwardsFRAM')
#' ## "safe" version of the table; stock_id WILL align with other tables
#' fram_db |> fetch_table_bkchin()
#' }
fetch_table_bkchin <- function(fram_db){
  validate_fram_db(fram_db)

  if(fram_db$fram_db_species != "CHINOOK"){
    cli::cli_abort("`fetch_table_bkchin()` only appropriate for CHINOOK databases, not {fram_db$fram_db_species} database.")
  }

  output_table <- fetch_table(fram_db, table_name = "BackwardsFRAM",
                              warn = FALSE) |>
    dplyr::rename(bk_stock_id = .data$stock_id) |>
    dplyr::left_join(framrosetta::bk_lookup_chin, by = "bk_stock_id")

  return(output_table)
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

