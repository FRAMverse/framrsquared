#' Returns a tibble displaying mark rates by
#' @param fram_db FRAM database object
#' @param run_id Run ID (optional)
#' @export
#' @examples
#' \dontrun{
#' fram_db |> coho_mark_rates(run_id)
#' }
coho_mark_rates <- function(fram_db, run_id=NULL) {


  mortality <- fram_db |>
    fetch_table('Mortality')

  fisheries <- fram_db |>
    fetch_table('Fishery') |>
    dplyr::filter(species == fram_db$fram_db_species) |>
    dplyr::select(fishery_id, fishery_name)

  runs <- fram_db |>
    fetch_table('RunID') |>
    dplyr::select(run_id, run_year, base_period_id)

  fishery_type <- fram_db |>
    fetch_table('FisheryScalers') |>
    dplyr::select(run_id, fishery_id, time_step, fishery_flag)


  mortality |>
    dplyr::inner_join(fishery_type, by = c('run_id', 'fishery_id', 'time_step')) |>
    dplyr::mutate(
      mark = dplyr::if_else(stock_id %% 2 == 0, 'AD', 'UM'), # identify mark
      # zero out encounters based on fishery flag, can probably save a few lines
      # but this is explicit
      msf_encounters = dplyr::if_else(fishery_flag %in% c(7:8, 17:18, 27:28), msf_encounter, 0),
      ns_encounters = dplyr::if_else(fishery_flag %in% c(1:2, 17:18, 27:28), encounter, 0)
    ) |>
    dplyr::select(run_id, fishery_id, stock_id, time_step, mark, msf_encounters, ns_encounters) |>
    tidyr::pivot_longer(msf_encounters:ns_encounters) |>
    tidyr::pivot_wider(names_from = mark, values_from = value) |>
    dplyr::group_by(run_id, fishery_id, time_step, name) |>
    dplyr::summarize(dplyr::across(c(AD, UM), \(x) sum(x, na.rm = TRUE)), .groups = 'drop') |>
    dplyr::mutate(
      fishery_type = stringr::str_remove(name, '_encounters'),
      mark_rate = AD / (AD + UM)
    ) |>
    #dplyr::select(run_id, fishery_id, time_step, fishery_type, mark_rate) |>
    dplyr::inner_join(runs, by='run_id') |>
    dplyr::inner_join(fisheries, by = 'fishery_id')

}


