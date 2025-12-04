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

  validate_fram_db(fram_db, db_type = "full")

  if(!is.null(run_id)){validate_run_id(fram_db, run_id)}

  if(fram_db$fram_db_species != 'COHO') {
    cli::cli_abort('This function currently only works with coho.')
  }

  cli::cli_alert_warning('Coho mark rates calculated via encounters')

  mortality <- fram_db |>
    fetch_table('Mortality', label = FALSE)

  fisheries <- fram_db |>
    fetch_table('Fishery', label = FALSE) |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)

  runs <- fram_db |>
    fetch_table('RunID', label = FALSE) |>
    dplyr::select(.data$run_id, .data$run_year, .data$base_period_id)

  fishery_type <- fram_db |>
    fetch_table('FisheryScalers', label = FALSE) |>
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
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    label_fisheries(species = "COHO")

  if(!is.null(run_id)) {
    mark_rates |> dplyr::filter(.data$run_id == .env$run_id)
  } else {
    mark_rates
  }

}

#'  `r lifecycle::badge("experimental")`
#'  Calculate starting cohort abundance
#'
#' The starting cohort abundance listed in the database can be wrong. This function
#' calculates the value by multipying the Stock Recruit Scalar by the base period abundance.
#' @param fram_db FRAM database object
#' @param run_id Run ID (optional)
#' @export
#' @examples
#' \dontrun{
#' fram_db |>  cohort_abundance(run_id = 145)
#' }
cohort_abundance <- function(fram_db, run_id = NULL){

  validate_fram_db(fram_db, db_type = 'full')

  if(!is.null(run_id)){validate_run_id(fram_db, run_id)}

  # pull run table, cross walk from stockrecruit table to bp table
  runs <- fram_db |>
    fetch_table('RunID', label = FALSE) |>
    dplyr::select(.data$run_id, .data$base_period_id)

  stock_recruit <- fram_db |>
    fetch_table('StockRecruit', label = FALSE) |>
    dplyr::select(.data$run_id:.data$recruit_scale_factor)

  base_cohort <- fram_db |>
    fetch_table('BaseCohort', label = FALSE)

  abundances <- runs |>
    dplyr::inner_join(stock_recruit, by = 'run_id') |>
    dplyr::inner_join(base_cohort, by = c('base_period_id', 'stock_id', 'age')) |>
    dplyr::mutate(
      recruit_cohorts = .data$recruit_scale_factor * .data$base_cohort_size
    ) |>
    dplyr::select(.data$run_id,
                  .data$stock_id,
                  .data$age,
                  .data$recruit_cohorts)

  if(!is.null(run_id)) {
    abundances |> dplyr::filter(.data$run_id == run_id)  |>
      `attr<-`('species', fram_db$fram_db_species) |>
      label_stocks()
  } else {
    abundances  |>
      `attr<-`('species', fram_db$fram_db_species)|>
      label_stocks()
  }

}



#'  `r lifecycle::badge("experimental")`
#' Summarizes the three true outcomes of a stocks abundance, where it
#' dies to fishery related mortality, natural mortality, or reaches
#' some sort of escapement. When run against the coho database
#' spawning escapement will be displayed, when run against the
#' Chinook database escapement to the river will be dislpayed
#' along with recruits to the next year 'age_up'
#' @param fram_db FRAM database object
#' @param run_id Run ID (optional)
#' @param units 'fish' or 'percentage'. Percentage is proportion of starting adundance
#' @export
#' @examples
#' \dontrun{
#' fram_db |> stock_fate(run_id = 145)
#' }
stock_fate <- function(fram_db, run_id = NULL, units = c('fish', 'percentage')) {
  validate_fram_db(fram_db)
  if(!is.null(run_id)){validate_run_id(fram_db, run_id)}

  switch(
    fram_db$fram_db_species,
    'CHINOOK' = stock_fate_chinook(fram_db, run_id, units),
    'COHO' = stock_fate_coho(fram_db, run_id, units)
  )
}


#'  `r lifecycle::badge("experimental")`
#' Chinook flavor of the stock fate function
#' @param fram_db FRAM database object
#' @param run_id Run ID (optional)
#' @param units 'fish' or 'percentage'. Percentage is proportion of starting abundance
#' @export
#' @seealso [stock_fate()]
#' @examples
#' \dontrun{
#' fram_db |> stock_fate_chinook(run_id = 145)
#' }
stock_fate_chinook <- function(fram_db, run_id = NULL, units = c('fish', 'percentage')){
  validate_fram_db(fram_db)
  if(!is.null(run_id)){validate_run_id(fram_db, run_id)}
  units <- rlang::arg_match(units)

  # pull fishery mortality
  mortality <- fram_db |> fetch_table('Mortality', label = FALSE) |>
    dplyr::select(-.data$primary_key) |>
    dplyr::filter(.data$time_step != 4) |>
    add_total_mortality() |>
    dplyr::group_by(.data$run_id, .data$stock_id, .data$age) |>
    dplyr::summarize(
      fishery_mortality = sum(.data$total_mortality),
      .groups = 'drop'
    )

  pop_stats <- fram_db |> population_statistics(run_id) |>
    dplyr::filter(.data$time_step != 4) |>
    dplyr::mutate(
      natural_mortality = .data$starting_cohort - .data$post_nat_mort,
    ) |>
    dplyr::arrange(.data$stock_id, .data$age) |>
    dplyr::group_by(.data$run_id, .data$stock_id, .data$age) |>
    dplyr::summarize(
      natural_mortality = sum(.data$natural_mortality),
      escapement_to_river = sum(.data$escapement),
      age_up = min(.data$post_pre_terminal),
      .groups = 'drop'
      ) |>
    dplyr::inner_join(mortality, by = c('run_id', 'stock_id', 'age')) |>
    dplyr::select(
      .data$run_id:.data$natural_mortality,
      .data$fishery_mortality, .data$age_up,.data$escapement_to_river,

    )

  if(units == 'percentage') {
    pop_stats <- pop_stats |>
      dplyr::mutate(
        dplyr::across(.data$natural_mortality:.data$escapement_to_river,
                      \(x) x / (.data$natural_mortality
                                + .data$escapement_to_river
                                + .data$fishery_mortality
                                + .data$age_up)
                      )
      )
  }

  if(!is.null(run_id)) {
    pop_stats |> dplyr::filter(.data$run_id == run_id)  |>
      `attr<-`('species', fram_db$fram_db_species)
  } else {
    pop_stats  |>
      `attr<-`('species', fram_db$fram_db_species)
  }

}

#'  `r lifecycle::badge("experimental")`
#' Coho flavor of the stock fate function
#' @param fram_db FRAM database object
#' @param run_id Run ID (optional)
#' @param units 'fish' or 'percentage'. Percentage is proportion of starting adbundace
#' @export
#' @seealso [stock_fate()]
#' @examples
#' \dontrun{
#' fram_db |> stock_fate_coho(run_id = 145)
#' }
stock_fate_coho <- function(fram_db, run_id = NULL, units = c('fish', 'percentage')){
  validate_fram_db(fram_db)
  if(!is.null(run_id)){validate_run_id(fram_db, run_id)}
  units <- rlang::arg_match(units)

  # pull fishery mortality
  mortality <- fram_db |> fetch_table('Mortality', label = FALSE) |>
    dplyr::select(-.data$primary_key) |>
    dplyr::mutate(
      fishery_mortality = .data$landed_catch + .data$non_retention
      + .data$drop_off + .data$shaker
      + .data$msf_landed_catch + .data$msf_non_retention
      + .data$msf_drop_off + .data$msf_shaker
    ) |>
    dplyr::group_by(.data$run_id, .data$stock_id, .data$age) |>
    dplyr::summarize(
      fishery_mortality = sum(.data$fishery_mortality),
      .groups = 'drop'
    )

  pop_stats <- fram_db |> population_statistics(run_id) |>
    dplyr::mutate(
      natural_mortality = .data$starting_cohort - .data$post_nat_mort,
    ) |>
    dplyr::arrange(.data$stock_id, .data$age) |>
    dplyr::group_by(.data$run_id, .data$stock_id, .data$age) |>
    dplyr::summarize(
      natural_mortality = sum(.data$natural_mortality),
      escapement_spawning = sum(.data$escapement),
      .groups = 'drop'
    ) |>
    dplyr::inner_join(mortality, by = c('run_id', 'stock_id', 'age')) |>
    dplyr::select(
      .data$run_id:.data$natural_mortality,
      .data$fishery_mortality, .data$escapement_spawning,

    )

  if(units == 'percentage') {
    pop_stats <- pop_stats |>
      dplyr::mutate(
        dplyr::across(.data$natural_mortality:.data$escapement_spawning,
                      \(x) x / (.data$natural_mortality
                                + .data$escapement_spawning
                                + .data$fishery_mortality
                                )
        )
      )
  }

  if(!is.null(run_id)) {
    pop_stats |> dplyr::filter(.data$run_id == run_id)  |>
      `attr<-`('species', fram_db$fram_db_species)
  } else {
    pop_stats  |>
      `attr<-`('species', fram_db$fram_db_species)
  }

}



