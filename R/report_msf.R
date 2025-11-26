
#' Reproduce MSF mortalities screen
#'
#' Produces the MSF screen report numbers for mortalities. Returns different
#' format depending database.
#'
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @export
#'
#' @seealso [msf_encounters()], [msf_landed_catch()]
#'
#' @examples
#' \dontrun{fram_db |> msf_mortalities_coho_(run_id = 101)}
msf_mortalities <- function(fram_db, run_id = NULL){
  validate_fram_db(fram_db)
  if(!is.null(run_id)) {validate_run_id(fram_db, run_id)}

  if(is.null(run_id)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_mortalities_chinook_(fram_db),
      'COHO' = msf_mortalities_coho_(fram_db)
    )
  } else {
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_mortalities_chinook_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id),
      'COHO' = msf_mortalities_coho_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id)
    )
  }
}

#' Reproduce MSF encounters screen
#'
#' Produces the MSF screen report numbers for encounters. Returns different
#' format depending database.
#'
#' @inheritParams msf_mortalities
#' @export
#'
#' @seealso [msf_mortalities()], [msf_landed_catch()]
#'
#' @examples
#' \dontrun{fram_db |> msf_encounters(run_id = 101)}
msf_encounters <- function(fram_db, run_id = NULL){
  validate_fram_db(fram_db)
  if(!is.null(run_id)) {validate_run_id(fram_db, run_id)}

  if(is.null(run_id)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_encounters_chinook_(fram_db),
      'COHO' = msf_encounters_coho_(fram_db)
    )
  } else {
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_encounters_chinook_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id),
      'COHO' = msf_encounters_coho_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id)
    )
  }
}

#' Reproduce MSF landed catch screen
#'
#' Produces the MSF screen report numbers for landed catch. Returns different
#' format depending database.
#'
#' @inheritParams msf_mortalities
#' @export
#'
#' @seealso [msf_encounters()], [msf_mortalities()]
#'
#' @examples
#' \dontrun{fram_db |> msf_landed_catch(run_id = 101)}
msf_landed_catch <- function(fram_db, run_id=NULL){
  validate_fram_db(fram_db)
  if(!is.null(run_id)) {validate_run_id(fram_db, run_id)}

  if(is.null(run_id)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_landed_catch_chinook_(fram_db),
      'COHO' = msf_landed_catch_coho_(fram_db)
    )
  } else {
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_landed_catch_chinook_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id),
      'COHO' = msf_landed_catch_coho_(fram_db) |>
        dplyr::filter(.data$run_id %in% .env$run_id)
    )
  }
}






