#'  `r lifecycle::badge("experimental")`
#' Returns a tibble displaying predicted FRAMencounter mark rates by fishery,
#' fishery type, and time-step.
#' @param fram_db FRAM database object
#' @param run_id Run ID (optional)
#' @export
#' @examples
#' \dontrun{
#' fram_db |> coho_mark_rates(run_id)
#' }
coho_mark_rates <- function(fram_db, run_id=NULL) {

  if(fram_db$fram_db_species != 'COHO') {
    cli::cli_abort('This function currently only works with coho.')
  }

  cli::cli_alert_warning('Coho mark rates calculated via encounters')

  mortality <- fram_db |>
    fetch_table('Mortality')

  fisheries <- fram_db |>
    fetch_table('Fishery') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)

  runs <- fram_db |>
    fetch_table('RunID') |>
    dplyr::select(.data$run_id, .data$run_year, .data$base_period_id)

  fishery_type <- fram_db |>
    fetch_table('FisheryScalers') |>
    dplyr::select(.data$run_id, .data$fishery_id, .data$time_step, .data$fishery_flag)

  mark_rates <- mortality |>
    dplyr::inner_join(fishery_type, by = c('run_id', 'fishery_id', 'time_step')) |>
    dplyr::mutate(
      mark = dplyr::if_else(.data$stock_id %% 2 == 0, 'AD', 'UM'), # identify mark
      # zero out encounters based on fishery flag, can probably save a few lines
      # but this is explicit
      msf_encounters = dplyr::if_else(.data$fishery_flag %in% c(7:8, 17:18, 27:28), .data$msf_encounter, 0),
      ns_encounters = dplyr::if_else(.data$fishery_flag %in% c(1:2, 17:18, 27:28), .data$encounter, 0)
    ) |>
    dplyr::select(.data$run_id, .data$fishery_id, .data$stock_id, .data$time_step,
                  .data$mark, .data$msf_encounters, .data$ns_encounters) |>
    tidyr::pivot_longer(.data$msf_encounters:.data$ns_encounters) |>
    tidyr::pivot_wider(names_from = .data$mark, values_from = .data$value) |>
    dplyr::group_by(.data$run_id, .data$fishery_id, .data$time_step, .data$name) |>
    dplyr::summarize(dplyr::across(c(.data$AD, .data$UM), \(x) sum(x, na.rm = TRUE)), .groups = 'drop') |>
    dplyr::mutate(
      fishery_type = stringr::str_remove(.data$name, '_encounters'),
      mark_rate = .data$AD / (.data$AD + .data$UM)
    ) |>
    dplyr::select(.data$run_id, .data$fishery_id, .data$AD, .data$UM,
                  .data$time_step, .data$fishery_type, .data$mark_rate) |>
    dplyr::inner_join(runs, by='run_id') |>
    dplyr::inner_join(fisheries, by = 'fishery_id')

  if(!is.null(run_id)) {
    mark_rates |> dplyr::filter(.data$run_id == .env$run_id)
  } else {
    mark_rates
  }

}

