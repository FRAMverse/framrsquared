#' Replicate Stock Mortality screen
#'
#' Returns a tibble matching the Stock Mortality screen.
#'
#' @param fram_db FRAM database object
#' @param run_id atomic or vector of run_ids to filter to. Can improve speed. Optional, defaults to `NULL`.
#' @param stock_id atomic or vector of stock_id to filter to. Can improve speed. Optional, defaults to `NULL`.
#'
#' @returns Tibble matching the "Stock Mortality" screen of the FRAM interface.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fram_db |>
#'  stock_mortality(run_id=132) |>
#'  filter(stock_id == 17, fishery_id == 36)
#'
#' }
stock_mortality <- function(fram_db, run_id = NULL, stock_id = NULL) {

  validate_fram_db(fram_db)
    validate_run_id(fram_db, run_id, allow_null = TRUE)
    validate_stock_ids(fram_db, stock_id, allow_null = TRUE)

  stock_mort <- fram_db |>
    fetch_table_("Mortality")

  if(!is.null(run_id)){
    stock_mort <- stock_mort |>
      dplyr::filter(.data$run_id %in% .env$run_id)
  }
  if(!is.null(stock_id)){
    stock_mort <- stock_mort |>
      dplyr::filter(.data$stock_id %in% .env$stock_id)
  }


  stock_mort <- stock_mort |>
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
          "landed_catch":"drop_off",
          "msf_landed_catch":"msf_drop_off"
        ),
        \(x) sum(x)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer("landed_catch":"msf_drop_off") |>
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
      "run_id":"time_step", "landed_catch",
      "non_retention", "shaker", "drop_off"
    ) |>
    dplyr::arrange(.data$run_id, .data$fishery_id, .data$age, .data$time_step)

  attr(stock_mort, 'species') <-  fram_db$fram_db_species

  return(stock_mort)


}
