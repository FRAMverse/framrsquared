
#' Replicate Population Statistics screen
#'
#' Returns a tibble matching the Population Statistics screen.
#'
#' @param fram_db FRAM database object
#' @param run_id Run ID
#'
#' @returns Tibble identifying run, stock, age, timestep. Then provides the number of fish present at each substep within a timestep: before any mortalities (`$starting_cohort`), after natural mortalities but before marine fishing mortalities `$post_pre_terminal`, the number of fish reaching maturation (`$maturation`; only relevant for Chinook), and the number of fish reaching escapement (`$escapement`)
#'
#' @export
#'
#' @examples
#' \dontrun{fram_db |> population_statistics(run_id = 101)}
#'
population_statistics <- function(fram_db, run_id = NULL) {
  validate_fram_db(fram_db)
  if(!is.null(run_id)){validate_run_id(fram_db, run_id)}

  cohort <- fram_db |>
    fetch_table_('Cohort') |>
    dplyr::select(
      "run_id",
      "stock_id",
      "age",
      "time_step",
      starting_cohort = "start_cohort",
      post_nat_mort = "working_cohort",
      post_pre_terminal = "cohort",
      maturation = "mature_cohort"
    )

  escapement <- fram_db |>
    fetch_table_('Escapement') |>
    dplyr::select(-"primary_key")

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
