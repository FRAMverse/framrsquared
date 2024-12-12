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
    cli::cli_abort('This is not a valid FRAM Database')
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
  validate_data_frame(.data)
  .data |>
    tibble::as_tibble() |>
    janitor::clean_names()
}

#' Gets all run_ids of FRAM database
#' @param fram_db Fram database object
#' @export
#' @examples
#' \dontrun{fram_dataframe |> get_run_ids()}
get_run_ids <- function(fram_db){
  fram_db |>
    fetch_table('RunID') |>
    dplyr::pull(.data$run_id)
}

#' Finds tables that contain a specific column name
#' @param fram_db FRAM database object
#' @param column_name Name of a column
#' @examples
#' \dontrun{fram_db |> find_tables_by_column_('RunID')}
#'
find_tables_by_column_ <- function(fram_db, column_name) {
  if (!DBI::dbIsValid(fram_db$fram_db_connection)) {
    cli::cli_abort('Connect to a FRAM database first...')
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

run_info <- function(fram_db, run_id) {
  if (!is.numeric (run_id)){cli::cli_abort('run_id must be numeric')}
  if (length(run_id) > 1) {cli::cli_abort('Provide only one run ID')}
  validate_run_id(fram_db, run_id)

  if (! run_id %in% get_run_ids(fram_db)){
    cli::cli_abort(paste0('run_id is not present in database. Available run ids: ',
                          paste0(get_run_ids(fram_db), collapse = ", ")))
  }else{
    run_info <- fram_db |>
      fetch_table('RunID') |>
      dplyr::filter(.data$run_id == .env$run_id)
  }


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

#' Welcome message, summarizing database information
#' @param con FRAM database connection
#' @examples
#' \dontrun{welcome(con)}
#'
welcome <- function(con){
  runs <- DBI::dbReadTable(con, 'RunID') |>
    tibble::as_tibble() |>
    janitor::clean_names()

  species <- unique(runs$species_name)
  run_count <- nrow(runs)
  run_ids = paste0(sort(runs$run_id), collapse = ", ")
  last_run <- format(max(runs$run_time_date), '%a, %B %d, %Y %I:%M %p')
  last_modify_input <- format(max(runs$modify_input_date), '%a, %B %d, %Y %I:%M %p')
  last_run_name <- runs |>
    dplyr::filter(.data$run_time_date == max(runs$run_time_date)) |>
    dplyr::pull(.data$run_name)

  cli::cli_text(cat(cli::col_blue('Database Species: '), cli::col_grey(species)))
  cli::cli_text(cat(cli::col_blue('Run Count: '), cli::col_grey(run_count)))
  cli::cli_text(cat(cli::col_blue('Run IDs: '), cli::col_grey(run_ids)))
  cli::cli_text(cat(cli::col_blue('Last Run Date: '), cli::col_grey(last_run)))
  cli::cli_text(cat(cli::col_blue('Last Run Name: '), cli::col_grey(last_run_name)))
  cli::cli_text(cat(cli::col_blue('Last Modify Date: '), cli::col_grey(last_modify_input)))

}

#' Convenience function to check fram_db input
#' @param fram_db FRAM database object
#' @param db_type Enforcement of a database type 'full' or 'transfer'
#' @param db_species Enforcement of a species 'COHO' or 'CHINOOK'
#' @param call internal use: identify name of function that called this function (for informative error message)
validate_fram_db <- function(fram_db,
                             db_type = NULL,
                             db_species = NULL,
                             call = rlang::caller_env()) {
  # check if fram_db object is a list
  if (!rlang::is_list(fram_db) |
      !"fram_db_connection" %in% names(fram_db)) {
    cli::cli_code('fram_db <- connect_fram_db(file_path)\nfram_db |> fetch_table(\'Mortality\')')
    cli::cli_abort('Invalid database type, try code above', call = call)
  }
  # check if it's a valid connection
  if (!DBI::dbIsValid(fram_db$fram_db_connection)) {
    cli::cli_abort("Invalid database connection", call = call)
  }

  # enforcement of a certain database type
  if (!is.null(db_type)) {
    db <- rlang::arg_match(db_type, c('full', 'transfer'))
    if (fram_db$fram_db_type != db) {
      cli::cli_abort("This function requires as {db} database, you're using a {fram_db$fram_db_type} database.",
                     call = call)
    }
  }

  # enforcement of a certain database species
  if (!is.null(db_species)) {
    species <- rlang::arg_match(db_species, c('COHO', 'CHINOOK'))
    if (fram_db$fram_db_species != species) {
      cli::cli_abort(
        "This function is specifically for {species}, you're using a {fram_db$fram_db_species} database.", call = call
      )
    }
  }
}

#' Convenience function to check run_id input
#' @param fram_db FRAM database object
#' @param run_id one or more run_ids
#' @param call internal use: identify name of function that called this function (for informative error message)
validate_run_id <- function(fram_db, run_id, call = rlang::caller_env()){
  validate_numeric(run_id)
  available_run_ids <- get_run_ids(fram_db)
  if (! all(run_id %in% available_run_ids)){
    cli::cli_abort(paste0('run_id(s) not present in database. Available run_ids: ',
                          paste0(available_run_ids, collapse = ", ")),
                   call = call)
  }
}


validate_data_frame <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  # checks for data frame, stolen from the tidyr package
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data frame, not {.obj_type_friendly {x}}.", ..., call = call)
  }
}

validate_numeric <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be a numeric, not {class(x)}.", ..., call = call)
  }
}
