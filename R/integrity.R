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

#' Changes a run's ID number in a FRAM database
#' @param fram_db FRAM database object
#' @param old_run_id FRAM run ID to be changed
#' @param new_run_id New FRAM run ID
#' @export
#' @examples
#' \dontrun{fram_db |> change_run_id(old_run_id = 132, new_run_id = 300)}
#'
change_run_id <- function(fram_db, old_run_id, new_run_id){

  if (!DBI::dbIsValid(fram_db$fram_db_connection)) {
    rlang::abort('Connect to a FRAM database first...')
  }


  tables <- DBI::dbListTables(fram_db$fram_db_connection) |>
    tibble::as_tibble()

  run_id_tables <- tables |>
    dplyr::rowwise() |>
    dplyr::mutate(columns = purrr::map(
      .data$value,
      \(table) DBI::dbListFields(fram_db$fram_db_connection, table)
    )) |>
    tidyr::unnest(.data$columns) |>
    dplyr::filter(.data$columns == 'RunID')

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
