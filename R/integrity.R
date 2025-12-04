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

  if(inherits(con, "ACCESS")){
    run_id_table <- DBI::dbGetQuery(con, 'SELECT * FROM RunID;') |>
      fram_clean_tables()
  } else if(is.list(con) & "fram_db_connection" %in% names(con)){
    run_id_table <- DBI::dbGetQuery(con$fram_db_connection, 'SELECT * FROM RunID;') |>
      fram_clean_tables()
  } else {
    cli::cli_abort("`con` must be a connection to a fram database (either the connection itself, or the output of `connect_fram_db()`).")
  }

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

#' Gets all fishery_ids of FRAM database
#' @param fram_db Fram database object
#' @export
#' @examples
#' \dontrun{fram_dataframe |> get_run_ids()}
get_fishery_ids <- function(fram_db){
  fram_db |>
    fetch_table('Fishery') |>
    dplyr::pull(.data$fishery_id)
}

#' Gets all stock_id of FRAM database
#' @param fram_db Fram database object
#' @export
#' @examples
#' \dontrun{fram_dataframe |> get_run_ids()}
get_stock_ids <- function(fram_db){
  fram_db |>
    fetch_table('Stock') |>
    dplyr::pull(.data$stock_id)
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
  validate_fram_db(fram_db)
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
  cli::cli_text(cat(cli::col_blue('Run ID: '), cli::col_grey(run_info$run_id[[1]])))
  cli::cli_text(cat(cli::col_blue('Run Name: '), cli::col_grey(run_info$run_name[[1]])))
  cli::cli_text(cat(cli::col_blue('Run Title: '), cli::col_grey(run_info$run_title[[1]])))
  cli::cli_text(cat(cli::col_blue('Run Date: '), cli::col_grey(run_info$run_time_date[[1]])))
  cli::cli_text(cat(cli::col_blue('Modify Date: '), cli::col_grey(run_info$modify_input_date[[1]])))
  cli::cli_text(cat(cli::col_blue('TAMM: '), cli::col_grey(run_info$tamm_name[[1]])))
  cli::cli_text(cat(cli::col_blue('Coast Iterations: '), cli::col_grey(run_info$coastal_iterations[[1]])))
  cli::cli_text(cat(cli::col_blue('Run Comments: '), '\n', cli::col_grey(stringr::str_remove_all(run_info$run_comments[[1]], "[[:punct:]]"))))

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

#' Convenience function to check fishery input
#'
#' No error checking for transfer databases
#'
#' @param fram_db FRAM database object
#' @param fishery_id one or more fishery_ids
#' @param call internal use: identify name of function that called this function (for informative error message)
validate_fishery_ids <- function(fram_db, fishery_id, call = rlang::caller_env()){
  validate_numeric(fishery_id)
  if(fram_db$fram_db_type == "full"){
    available_fishery_ids <- get_fishery_ids(fram_db)
    if (! all(fishery_id %in% available_fishery_ids)){
      cli::cli_abort('fishery_id(s) not present in this {fram_db$fram_db_species} database.
                     Available fisheries: {min(available_fishery_ids)}:{max(available_fishery_ids)}',
                     call = call)
    }
  }
}

#' Convenience function to check fishery input
#'
#' No error checking for transfer databases
#'
#' @param fram_db FRAM database object
#' @param stock_id one or more stock_ids
#' @param call internal use: identify name of function that called this function (for informative error message)
validate_stock_ids <- function(fram_db, stock_id, call = rlang::caller_env()){
  validate_numeric(stock_id)
  if(fram_db$fram_db_type == "full"){
    available_stock_ids <- get_stock_ids(fram_db)
    if (! all(stock_id %in% available_stock_ids)){
      cli::cli_abort('stock_id(s) not present in this {fram_db$fram_db_species} database.
                     Available stocks: {min(available_stock_ids)}:{max(available_stock_ids)}',
                     call = call)
    }
  }
}



validate_data_frame <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  # checks for data frame, stolen from the tidyr package
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data frame, not {.obj_type_friendly {x}}.", ..., call = call)
  }
}

validate_numeric <- function(x, n = NULL, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be a numeric, not {class(x)}.", ..., call = call)
  }
  if(!is.null(n)){
    if(length(x) != n){
      cli::cli_abort("{.arg {arg}} must be a numeric of length {n}.", ..., call = call)
    }
  }
}

validate_character <- function(x, n = NULL, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.character(x)) {
    cli::cli_abort("{.arg {arg}} must be a character, not {class(x)}.", ..., call = call)
  }
  if(!is.null(n)){
    if(length(x) != n){
      cli::cli_abort("{.arg {arg}} must be a character of length {n}.", ..., call = call)
    }
  }
}

validate_flag <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if (!is.logical(x) | length(x) != 1) {
    cli::cli_abort("{.arg {arg}} must be a a logical of length 1.", ..., call = call)
  }
}

#' Handle species identification for filters
#'
#' Convenience function to condense code. `filter_*` either uses the "species" attr, or the optional `species` argument, and must provide informative errors when both are missing or both are present and mismatch.
#'
#' @param .data Dataframe
#' @param species Optional, either "COHO" or "CHINOOK".
#'
#' @return Character vector "species"
#' @keywords internal
validate_species <- function(.data,
                             species = NULL){
  if(!is.null(species)){
    species = standardize_species(species)
  }
  if(is.null(species)){
    if(!is.null(attr(.data, 'species'))){
      species <- attr(.data, 'species')
    } else {
      cli::cli_abort('Table metadata missing and `species` argument missing.')
    }
  }

  if(!is.null(attr(.data, 'species')) & !is.null(species)){
    if(species != attr(.data, 'species')){
      cli::cli_abort('`species` argument ("{species}") should not differ from species attribute of data ("{attr(.data, "species")}"). Consider dropping `species` argument.')
    }
  }
  return(species)
}



#' Allow multiple species identifiers
#'
#' framrsquared functions are written around fram database species labels, "COHO" and "CHINOOK". This function translates alternate designations (lowercase, "chin" for "chinook") into those two forms.
#'
#' @param species Character atomic, either "COHO", "CHIN", or "CHINOOK", with any capitalization
#'
#' @return Character atomic, either "COHO" or "CHINOOK"
#' @keywords internal
standardize_species <- function(species){
  species = toupper(species)
  coho_names = c("COHO")
  chinook_names = c("CHINOOK", "CHIN")
  species <- rlang::arg_match(species, c(coho_names, chinook_names))
  if(species %in% coho_names){
    species = "COHO"
  } else if(species %in% chinook_names){
    species = "CHINOOK"
  }
  return(species)
}

validate_table <- function(fram_db, table_name){
  if (fram_db$fram_db_type == 'full') {
    table_name <- rlang::arg_match(table_name,
                                   provide_table_names(is_full = TRUE))
  } else {
    table_name <- rlang::arg_match(table_name,
                                   provide_table_names(is_full = FALSE))
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
