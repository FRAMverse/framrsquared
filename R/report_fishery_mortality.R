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


