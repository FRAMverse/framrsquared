#' Replicate MSF screen report mortalities for Chinook
#'
#' Returns a tibble matching the MSF screen report mortalities for Chinook. This is
#' specific for Chinook and in most cases [msf_mortalities()] is preferable.
#'
#' @param fram_db FRAM database object
#'
#' @returns Tibble
#'
#' @export
#' @keywords internal
#'
#' @seealso [msf_mortalities()], [msf_encounters_chinook_()], [msf_landed_catch_chinook_()]
#'
#' @examples
#' \dontrun{fram_db |> msf_mortalities_chinook_(run_id = 101)}
#'
msf_mortalities_chinook_ <- function(fram_db){

  validate_fram_db(fram_db)

  mortalities_ <- fram_db |>
                    msp_mortality()

  mortalities_ <- mortalities_ |>
    dplyr::select("run_id":"time_step", dplyr::starts_with('msf_'))  |>
    dplyr::mutate( # needs to happen after legal/sublegal identification
      mark_status = dplyr::if_else(.data$stock_id %% 2 == 0, 'marked', 'unmarked')
    )


  mortalities <- mortalities_ |>
    tidyr::pivot_longer(c("msf_landed_catch":"msf_drop_off")) |>
    dplyr::group_by(.data$run_id, .data$fishery_id,
                    .data$time_step, .data$name, .data$mark_status) |> # over stock
    dplyr::summarize(value = sum(.data$value), .groups = 'drop') |>
    dplyr::mutate(
      legality  = dplyr::if_else(
        .data$name %in% c(
          'msf_shaker'
        ),
        'sublegal',
        'legal')
    ) |>
    tidyr::unite(col = 'legal_mark_status',
                 "legality", "mark_status") |>
    dplyr::group_by(.data$run_id, .data$fishery_id,
                    .data$time_step, .data$legal_mark_status) |>
    dplyr::summarize(value = sum(.data$value), .groups = 'drop')

  # output
  mortalities |>
    tidyr::pivot_wider(names_from = "legal_mark_status", values_from = "value") |>
    `attr<-`('species', fram_db$fram_db_species)

}


#' Replicate MSF screen report encounters for Chinook
#'
#' Returns a tibble matching the MSF screen report encounters for Chinook.
#'  This is specific for Chinook and in most cases [msf_encounters()] is preferable.
#'
#' @param fram_db FRAM database object
#'
#' @returns Tibble.
#'
#' @export
#' @keywords internal
#'
#' @seealso [msf_encounters()], [msf_mortalities_chinook_()], [msf_landed_catch_chinook_()]
#'
#' @examples
#' \dontrun{fram_db |> msf_encounters_chinook_(run_id = 101)}
#'

msf_encounters_chinook_ <- function(fram_db){

  validate_fram_db(fram_db)

  encounters_ <- fram_db |>
    msp_mortality()

  # ensure the correct rates are always used
  sublegal_mortality_rates <- DBI::dbGetQuery(
    fram_db$fram_db_connection,
    glue::glue('SELECT
                    RunID.RunID, ShakerMortRate.FisheryID,
                    ShakerMortRate.TimeStep,
                    ShakerMortRate.ShakerMortRate
                FROM RunID INNER JOIN
               ShakerMortRate ON RunID.BasePeriodID = ShakerMortRate.BasePeriodID;')
  ) |> fram_clean_tables()

  encounters <- encounters_ |>
    dplyr::select("run_id", "stock_id", "fishery_id",
                  "time_step", "msf_shaker", "msf_encounter")  |>
    dplyr::mutate( # needs to happen after legal/sublegal identification
      mark_status = dplyr::if_else(.data$stock_id %% 2 == 0, 'marked', 'unmarked'),
    )

  encounters |>
    dplyr::group_by(.data$run_id, .data$mark_status, .data$fishery_id, .data$time_step) |>
    dplyr::summarize(msf_encounter = sum(.data$msf_encounter), msf_shaker = sum(.data$msf_shaker), .groups='drop') |>
    dplyr::inner_join(sublegal_mortality_rates, by=c('fishery_id', 'time_step', 'run_id')) |>
    dplyr::mutate(
      shaker_encounters = .data$msf_shaker / .data$shaker_mort_rate
    ) |>
    dplyr::select(-c("msf_shaker":"shaker_mort_rate"), legal = "msf_encounter", sublegal = "shaker_encounters") |>
    tidyr::pivot_longer("legal":"sublegal") |>
    tidyr::unite(col = 'legal_mark_status',
                 "name", "mark_status") |>
    tidyr::pivot_wider(names_from = "legal_mark_status", values_from = "value") |>
    `attr<-`('species', fram_db$fram_db_species)
}

#' Replicate MSF screen report landed catch for Chinook
#'
#' Returns a tibble matching the MSF screen report landed catch for Chinook.
#'  This is specific for Chinook and in most cases [msf_landed_catch()] is preferable.
#' @param fram_db FRAM database object
#'
#' @returns tibble
#'
#' @export
#' @keywords internal
#'
#' @seealso [msf_landed_catch()], [msf_mortalities_chinook_()], [msf_encounters_chinook_()]
#'
#' @examples
#' \dontrun{fram_db |> msf_landed_catch_chinook_(run_id = 101)}
#'
msf_landed_catch_chinook_ <- function(fram_db){

  validate_fram_db(fram_db)

  landed_catch_ <- fram_db |>
    msp_mortality()
  landed_catch <- landed_catch_ |>
    dplyr::select("run_id", "stock_id", "fishery_id", "time_step",
                  "msf_landed_catch", "msf_shaker")  |>
    dplyr::mutate( # needs to happen after legal/sublegal identification
      mark_status = dplyr::if_else(.data$stock_id %% 2 == 0, 'marked',
                                   'unmarked'),
    )


  landed_catch |>
    dplyr::group_by(.data$run_id, .data$mark_status, .data$fishery_id,
                    .data$time_step) |>
    dplyr::summarize(msf_landed_catch = sum(.data$msf_landed_catch), .groups='drop') |>
    dplyr::select(dplyr::everything(), legal = "msf_landed_catch") |>
    tidyr::pivot_longer("legal") |>
    tidyr::unite(col = 'legal_mark_status',
                 "name", "mark_status") |>
    tidyr::pivot_wider(names_from = "legal_mark_status", values_from = "value") |>
    `attr<-`('species', fram_db$fram_db_species)
}
