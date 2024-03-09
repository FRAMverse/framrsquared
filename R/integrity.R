#' Identifies the FRAM database type - Full or Transfer
#' @param con Connection to FRAM database
#' @export
#' @examples
#' \dontrun{fram_database_type(con)}
#'
fram_database_type <- function(con) {
  table_names <- DBI::dbListTables(con)
  if (all(
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
    ) %in% table_names
  ))
  {
    return(list(type = 'full'))
  } else if (all(
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
    ) %in% table_names
  )) {
    return(list(type = 'transfer'))
  }
  else {
    DBI::dbDisconnect(con)
    rlang::abort('This is not a valid FRAM Database')
  }
}

#' Identifies the FRAM database species focus - Chinook or Coho
#' @param con Connection to FRAM database
#' @export
#' @examples
#' \dontrun{fram_database_species(con)}
fram_database_species <- function(con){
  run_id_table <- DBI::dbGetQuery(con, 'SELECT * FROM RunID;') |>
    fram_clean_tables()

  unique(run_id_table$species_name)
}


#' Cleans the names of FRAM tables and coverts to a tibble
#' @param .data Dataframe
#' @export
#' @examples
#' \dontrun{fram_dataframe |> fram_clean_tables()}
#'
fram_clean_tables <- function(.data) {
  .data |>
  tibble::as_tibble() |>
  janitor::clean_names()
}


#' Finds tables that contain a specific column name
#' @param fram_db FRAM database object
#' @param column_name Name of a column
#' @examples
#' \dontrun{fram_db |> find_tables_by_column_('RunID')}
#'
find_tables_by_column_ <- function(fram_db, column_name) {
  if (!DBI::dbIsValid(fram_db$fram_db_connection)) {
    rlang::abort('Connect to a FRAM database first...')
  }


  tables <- DBI::dbListTables(fram_db$fram_db_connection) |>
    tibble::as_tibble()

  tables |>
    dplyr::rowwise() |>
    dplyr::mutate(columns = purrr::map(
      .data$value,
      \(table) DBI::dbListFields(fram_db$fram_db_connection, table)
    )) |>
    tidyr::unnest(.data$columns) |>
    dplyr::filter(.data$columns == .env$column_name)
}



#' Changes a run's ID number in a FRAM database
#' @param fram_db FRAM database object
#' @param old_run_id FRAM run ID to be changed
#' @param new_run_id New FRAM run ID
#' @export
#' @examples
#' \dontrun{fram_db |> change_run_id(old_run_id = 132, new_run_id = 300)}
#'
change_run_id <- function(fram_db, old_run_id, new_run_id){

  run_id_tables <- find_tables_by_column_(fram_db, 'RunID')

  run_id_tables$value |>
    purrr::walk(.f = \(value) tryCatch(
      suppressWarnings(DBI::dbSendQuery(
        fram_db$fram_db_connection,
        glue::glue(
          'UPDATE {value}
           SET RunID = {new_run_id}
           WHERE RunID = {old_run_id};'
        )
      )),
      error = function(e) {} # dead end
    ))

}




#' Removes a run in a FRAM database
#' @param fram_db FRAM database object
#' @param run_id FRAM run ID to be deleted
#' @export
#' @examples
#' \dontrun{fram_db |> delete_run(run_id = 132)}
#'
remove_run <- function(fram_db, run_id){

  run_id_tables <- find_tables_by_column_(fram_db, 'RunID')

  run_id_tables$value |>
    purrr::walk(.f = \(value) tryCatch(
      suppressWarnings(DBI::dbSendQuery(
        fram_db$fram_db_connection,
        glue::glue(
          'DELETE FROM {value}
           WHERE RunID = {run_id};'
        )
      )),
      error = function(e) {} # dead end
    ))

}

#' Provides a print out of Run ID information
#' @param fram_db FRAM database object
#' @param run_id FRAM run ID
#' @export
#' @examples
#' \dontrun{fram_db |> run_info(run_id = 132)}
#'

run_info <- function(fram_db, run_id = NULL) {
  if (is.null(run_id)) {rlang::abort('A run ID must be provided')}

  run_info <- fram_db |>
    fetch_table('RunID') |>
    dplyr::filter(.data$run_id == .env$run_id)

  cli::cli_h1('FRAM Run Information')
  cli::cli_text(cat(cli::col_blue('Species: '), cli::col_grey(run_info$species_name[[1]])))
  cli::cli_text(cat(cli::col_blue('Database Type: '), cli::col_grey(fram_db$fram_db_type)))
  cli::cli_text(cat(cli::col_blue('Run Name: '), cli::col_grey(run_info$run_name[[1]])))
  cli::cli_text(cat(cli::col_blue('Run Title: '), cli::col_grey(run_info$run_title[[1]])))
  cli::cli_text(cat(cli::col_blue('Run Date: '), cli::col_grey(run_info$run_time_date[[1]])))
  cli::cli_text(cat(cli::col_blue('Modify Date: '), cli::col_grey(run_info$modify_input_date[[1]])))
  cli::cli_text(cat(cli::col_blue('TAMM: '), cli::col_grey(run_info$tamm_name[[1]])))
  cli::cli_text(cat(cli::col_blue('Coast Iterations: '), cli::col_grey(run_info$coastal_iterations[[1]])))
  cli::cli_text(cat(cli::col_blue('Run Comments: '), '\n', cli::col_grey(run_info$run_comments[[1]])))

}

