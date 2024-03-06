#' Returns a tibble matching the Fishery Mortality screen.
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @param msp Model Stock Proportion, default TRUE
#' @export
#' @examples
#' \dontrun{
#' fram_db |> fishery_mortality(run_id = 101)
#' }
fishery_mortality <- function(fram_db, run_id = NULL, msp = TRUE) {

  if(!is.numeric(run_id) && !is.null(run_id)) {
    rlang::abort('Run ID must be and integer')
  }

  if(!DBI::dbIsValid(fram_db$fram_db_connection)) {
    rlang::abort('Must connect to a FRAM database first...')
  }

  fishery_mort <- fram_db |>
    fetch_table("Mortality") |>
    dplyr::group_by(
      .data$run_id,
      .data$age,
      .data$fishery_id,
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

  # chinook needs to return model stock proportion estimates
  if (fram_db$fram_db_species == 'CHINOOK' & msp == TRUE){

    runid <- fram_db |>
      fetch_table('RunID')

    msp <- fram_db |>
      fetch_table('FisheryModelStockProportion')

    msp_run_id <- runid |>
      dplyr::inner_join(msp, by = 'base_period_id', relationship = 'many-to-many') |>
      dplyr::select(.data$run_id, .data$fishery_id, .data$model_stock_proportion)

    fishery_mort <- fishery_mort |>
      dplyr::left_join(msp_run_id, by = c('run_id', 'fishery_id')) |>
      dplyr::mutate(
        dplyr::across(
          .data$landed_catch:.data$drop_off,
          \(x) x / .data$model_stock_proportion
        )
      ) |>
      dplyr::select(-.data$model_stock_proportion)
  }


  if (is.null(run_id)) {
    fishery_mort |> # returns fishery mortality for all runs in db
      `attr<-`('species', fram_db$fram_db_species)
  } else {
    fishery_mort |>
      dplyr::filter(.data$run_id %in% .env$run_id) |>
      `attr<-`('species', fram_db$fram_db_species)
  }


}



#' Creates an ordered bar chart with
#' the top number of mortalities per
#' fishery.
#'
#' @export
#'
#' @param fram_db fram database object, supplied through connect_fram_db
#' @param run_id numeric, RunID
#' @param stock_id numeric, ID of focal stock
#' @param top_n numeric, Number of fisheries to display
#'
#' @examples
#' \dontrun{
#' fram_db |> coho_stock_mortality(run_id = 132, stock_id = 17)
#' }
#'

coho_stock_mortality <- function(fram_db, run_id = NULL, stock_id = NULL, top_n = 10){

  # check for null ids
  if (is.null(run_id) | is.null(stock_id)) {
    rlang::abort("Both a run_id and stock_id must be supplied")
  }

  # make sure run ids are integers
  if (!is.numeric(run_id)) {
    rlang::abort("Run ID must be and integer")
  }

  # make sure run ids are integers
  if (!is.numeric(stock_id)) {
    rlang::abort("Stock ID must be and integer")
  }

  # lut for display of stock name
  stocks <- fram_db |>
    fetch_table('Stock') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$stock_id, .data$stock_name)

  # lut for display of fishery
  fisheries <- fram_db |>
    fetch_table('Fishery') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)


  mortality <- fram_db |>
    fetch_table('Mortality') |>
    dplyr::filter(.data$run_id == .env$run_id,
           .data$stock_id == .env$stock_id) |>
    dplyr::group_by(.data$run_id, .data$stock_id, .data$fishery_id) |>
    dplyr::summarize(
      dplyr::across(c(.data$landed_catch:.data$drop_off,
               .data$msf_landed_catch:.data$msf_drop_off), \(x) sum(x)),
      .groups='drop') |>
    dplyr::mutate(
      total_mort = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off +
        .data$msf_landed_catch + .data$msf_non_retention + .data$msf_shaker + .data$msf_drop_off
    ) |>
    dplyr::select(.data$run_id, .data$stock_id, .data$fishery_id, .data$total_mort)

  run_name <- fram_db |>
    fetch_table('RunID') |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(.data$run_name)

  stock_name <- stocks |>
    dplyr::filter(.data$stock_id == .env$stock_id) |>
    dplyr::pull(.data$stock_name)

  mortality |>
    dplyr::slice_max(.data$total_mort, n = top_n) |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    ggplot2::ggplot(ggplot2::aes(.data$total_mort, stats::reorder(.data$fishery_name, .data$total_mort))) +
    ggplot2::geom_col() +
    ggplot2::labs(
      subtitle = glue::glue('Top motality for stock {stock_name} ({run_name})'),
      x = 'Mortalities',
      y = 'Fishery'
    )

}



