#' Replicate Stock Mortality screen
#'
#' Returns a tibble matching the Stock Mortality screen.
#'
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @export
#' @examples
#' \dontrun{
#' fram_db |>
#'  stock_mortality(run_id=132) |>
#'  filter(stock_id == 17, fishery_id == 36)
#'
#' }
stock_mortality <- function(fram_db, run_id = NULL) {

  validate_fram_db(fram_db)
  if(!is.null(run_id)){
    validate_run_id(fram_db, run_id)
  }

  stock_mort <- fram_db |>
    fetch_table_("Mortality") |>
    dplyr::group_by(
      .data$run_id,
      .data$age,
      .data$fishery_id,
      .data$stock_id,
      .data$time_step
    ) |>
    dplyr::summarize(
      dplyr::across(
        c(
          .data$landed_catch:.data$drop_off,
          .data$msf_landed_catch:.data$msf_drop_off
        ),
        \(x) sum(x)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(.data$landed_catch:.data$msf_drop_off) |>
    dplyr::mutate(name = stringr::str_remove(.data$name, "msf_")) |>
    dplyr::group_by(
      .data$run_id,
      .data$fishery_id,
      .data$stock_id,
      .data$age,
      .data$time_step,
      .data$name
    ) |>
    dplyr::summarise(value = sum(.data$value), .groups = "drop") |>
    tidyr::pivot_wider() |>
    dplyr::select(
      .data$run_id:.data$time_step, .data$landed_catch,
      .data$non_retention, .data$shaker, .data$drop_off
    ) |>
    dplyr::arrange(.data$run_id, .data$fishery_id, .data$age, .data$time_step)

  if (is.null(run_id)) {
    stock_mort |> # returns fishery mortality for all runs in db
      `attr<-`('species', fram_db$fram_db_species)
  } else {
    stock_mort |>
      dplyr::filter(.data$run_id %in% .env$run_id) |>
      `attr<-`('species', fram_db$fram_db_species)
  }


}
