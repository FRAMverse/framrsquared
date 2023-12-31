
#' Produces the MSF screen report numbers for mortalities. Returns different
#' format depending database.
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @export
#' @examples
#' \dontrun{fram_db |> msf_mortalities_coho_(run_id = 101)}
msf_mortalities <- function(fram_db, run_id = NULL){

  # checks on the run id
  if(!is.numeric(run_id) && !is.null(run_id)) {rlang::abort('Run ID must be and integer')}

  if(DBI::dbIsValid(fram_db$fram_db_connection) && is.null(run_id)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_mortalities_chinook_(fram_db),
      'COHO' = msf_mortalities_coho_(fram_db)
    )
  } else if(DBI::dbIsValid(fram_db$fram_db_connection) && !is.null(run_id)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_encounters_chinook_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id),
      'COHO' = msf_encounters_coho_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id)
    )
  } else {
    rlang::abort('Connect to a FRAM database first...')
  }
}

#' Produces the MSF screen report numbers for encounters. Returns different
#' format depending database.
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @export
#' @examples
#' \dontrun{fram_db |> msf_encounters(run_id = 101)}
msf_encounters <- function(fram_db, run_id = NULL){
  # checks on the run id
  if(!is.numeric(run_id) && !is.null(run_id)) {rlang::abort('Run ID must be and integer')}
  if(fram_db$species == 'CHINOOK' && fram_db$fram_db_type == 'transfer'){
    rlang::abort('
                 Cannot be connected to a transfer database to use this function
                  for Chinook.
                 ')
  }

  if(DBI::dbIsValid(fram_db$fram_db_connection) && is.null(run_id)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_encounters_chinook_(fram_db),
      'COHO' = msf_encounters_coho_(fram_db)
    )
  } else if(DBI::dbIsValid(fram_db$fram_db_connection) && !is.null(run_id)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_encounters_chinook_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id),
      'COHO' = msf_encounters_coho_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id)
    )
  } else {
    rlang::abort('Connect to a FRAM database first...')
  }
}

#' Produces the MSF screen report numbers for landed catch. Returns different
#' format depending database.
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @export
#' @examples
#' \dontrun{fram_db |> msf_landed_catch(run_id = 101)}
msf_landed_catch <- function(fram_db, run_id=NULL){
  # checks on the run id
  #if(is.null(run_id)){rlang::abort('Run ID must be provided.')}
  if(!is.numeric(run_id) && !is.null(run_id)) {rlang::abort('Run ID must be and integer')}

  if(DBI::dbIsValid(fram_db$fram_db_connection) && is.null(run_id)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_landed_catch_chinook_(fram_db),
      'COHO' = msf_landed_catch_coho_(fram_db)
    )
  } else if(DBI::dbIsValid(fram_db$fram_db_connection) && !is.null(run_id)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_landed_catch_chinook_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id),
      'COHO' = msf_landed_catch_coho_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id)
    )
  } else {
    rlang::abort('Connect to a FRAM database first...')
  }
}






