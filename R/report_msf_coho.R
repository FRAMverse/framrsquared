#' Replicate MSF screen report mortalities for COHO
#'
#' Returns a tibble matching the MSF screen report mortalities for Coho. This is
#' specific for Coho and in most cases [msf_mortalities()] is preferable.
#' @param fram_db FRAM database object
#' @export
#'
#' @seealso [msf_mortalities()], [msf_encounters_coho_()], [msf_landed_catch_coho_()]
#'
#' @examples
#' \dontrun{fram_db |> msf_mortalities_coho_()}
#'

msf_mortalities_coho_ <- function(fram_db){

  validate_fram_db(fram_db)

  mortalities_ <- fram_db |>
    fetch_table('Mortality')#|>
    #dplyr::filter(.data$run_id == .env$run_id)

  mortalities_ <- mortalities_ |>
    dplyr::select(.data$run_id:.data$time_step, dplyr::starts_with('msf_'))  |>
    dplyr::mutate(
      mark_status = dplyr::if_else(.data$stock_id %% 2 == 0, 'marked', 'unmarked')
    )

  mortalities_ |>
    tidyr::pivot_longer(c(.data$msf_landed_catch:.data$msf_drop_off)) |>
    dplyr::group_by(.data$run_id, .data$fishery_id, .data$time_step, .data$mark_status) |>
    dplyr::summarize(value = sum(.data$value), .groups = 'drop') |>
    tidyr::pivot_wider(names_from = .data$mark_status, values_from = .data$value) |>
    `attr<-`('species', fram_db$fram_db_species)


}

#' Replicate MSF screen report landed catch for COHO
#'
#' Returns a tibble matching the MSF screen report landed catch for Coho
#'  This is specific for Coho and in most cases msf_landed_catch() is preferable.
#' @param fram_db FRAM database object
#' @export
#'
#' @seealso [msf_landed_catch()], [msf_encounters_coho_()], [msf_mortalities_coho_()]
#'
#' @examples
#' \dontrun{fram_db |> msf_landed_catch_coho_()}
#'
msf_landed_catch_coho_ <- function(fram_db){

  validate_fram_db(fram_db)

  landed_catch_ <- fram_db |>
    fetch_table('Mortality') #|>
    #dplyr::filter(.data$run_id == .env$run_id)


  landed_catch <- landed_catch_ |>
    dplyr::select(.data$run_id, .data$stock_id, .data$fishery_id, .data$time_step,
                  .data$msf_landed_catch, .data$msf_shaker)  |>
    dplyr::mutate( # needs to happen after legal/sublegal identification
      mark_status = dplyr::if_else(.data$stock_id %% 2 == 0, 'marked',
                                   'unmarked'),
    )

  landed_catch |>
    dplyr::group_by(.data$run_id, .data$mark_status, .data$fishery_id,
                    .data$time_step) |>
    dplyr::summarize(msf_landed_catch = sum(.data$msf_landed_catch), .groups='drop') |>
    tidyr::pivot_wider(names_from = .data$mark_status, values_from = .data$msf_landed_catch) |>
    `attr<-`('species', fram_db$fram_db_species)
}

#' Replicate MSF screen report encounters for COHO
#'
#' Returns a tibble matching the MSF screen report encounters for Coho
#'  This is specific for Coho and in most cases msf_encounters() is preferable.
#' @param fram_db FRAM database object
#' @export
#'
#' @seealso [msf_encounters()], [msf_landed_catch_coho_()], [msf_mortalities_coho_()]
#'
#' @examples
#' \dontrun{fram_db |> msf_encounters_coho_()}
#'
msf_encounters_coho_ <- function(fram_db){

  validate_fram_db(fram_db)

  encounters_ <- fram_db |>
    fetch_table('Mortality') #|>
   # dplyr::filter(.data$run_id == .env$run_id)

  encounters <- encounters_ |>
    dplyr::select(.data$run_id, .data$stock_id, .data$fishery_id, .data$time_step,
                  .data$msf_shaker, .data$msf_encounter)  |>
    dplyr::mutate( # needs to happen after legal/sublegal identification
      mark_status = dplyr::if_else(.data$stock_id %% 2 == 0, 'marked',
                                   'unmarked'),
    )

  encounters |>
    dplyr::group_by(.data$run_id, .data$mark_status, .data$fishery_id,
                    .data$time_step) |>
    dplyr::summarize(msf_encounter = sum(.data$msf_encounter), .groups='drop') |>
    tidyr::pivot_wider(names_from = .data$mark_status,
                       values_from = .data$msf_encounter) |>
    `attr<-`('species', fram_db$fram_db_species)
}
