
#' Replicate Population Statistics screen
#'
#' Returns a tibble matching the Population Statistics screen.
#'
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @export
#' @examples
#' \dontrun{fram_db |> population_statistics(run_id = 101)}
#'
population_statistics <- function(fram_db, run_id = NULL) {
  validate_fram_db(fram_db)
  if(!is.null(run_id)){validate_run_id(fram_db, run_id)}

  cohort <- fram_db |>
    fetch_table_('Cohort') |>
    dplyr::select(
      .data$run_id,
      .data$stock_id,
      .data$age,
      .data$time_step,
      starting_cohort = .data$start_cohort,
      post_nat_mort = .data$working_cohort,
      post_pre_terminal = .data$cohort,
      maturation = .data$mature_cohort
    )

  escapement <- fram_db |>
    fetch_table_('Escapement') |>
    dplyr::select(-.data$primary_key)

  pop_stat <- cohort |>
    dplyr::left_join(escapement,
                     by = c('run_id',
                            'stock_id',
                            'age',
                            'time_step')) |>
    dplyr::mutate(
      dplyr::across(.data$escapement, \(x) tidyr::replace_na(x, 0))
      ) |>
    dplyr::arrange(.data$stock_id, .data$time_step)

  if (is.null(run_id)) {
    pop_stat |> # returns pop stat for all runs in db
      `attr<-`('species', fram_db$fram_db_species)
  } else {
    pop_stat |>
      dplyr::filter(.data$run_id %in% .env$run_id) |>
        `attr<-`('species', fram_db$fram_db_species)
  }
}
