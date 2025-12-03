
#' Expand Chinook mortality table using Model-Stock Proportion
#'
#' See https://framverse.github.io/fram_doc/calcs_data_chin.html#46_Model-Stock_Proportion.
#'
#' @param fram_db FRAM database object
#' @param run_id One or more run ids (optional)
#' @return Mortality table with mortality values expanded by msp
#' @export
#' @seealso [aeq_mortality()]
#' @examples
#' \dontrun{
#' fram_db |> msp_mortality(run_id = 132)
#' }
msp_mortality = function(fram_db, run_id = NULL){

  validate_fram_db(fram_db, db_type = 'full', db_species = 'CHINOOK')
  if(!is.null(run_id)){
    validate_run_id(fram_db, run_id)
  }

  runid <- fram_db |>
    fetch_table('RunID', label = FALSE)

  msp <- fram_db |>
    fetch_table('FisheryModelStockProportion', label = FALSE)

  mortality <- fram_db |>
    fetch_table('Mortality', label = FALSE)

  msp_run_id <- runid |>
    dplyr::inner_join(msp, by = 'base_period_id', relationship = 'many-to-many') |>
    dplyr::select(.data$run_id, .data$fishery_id, .data$model_stock_proportion)

  msp_mort <- mortality |>
    dplyr::left_join(msp_run_id, by = c('run_id', 'fishery_id')) |>
    dplyr::mutate(
      dplyr::across(
        c(.data$landed_catch:.data$drop_off,
        .data$msf_landed_catch:.data$msf_drop_off),
        \(x) x / .data$model_stock_proportion
      )
    ) |>
    dplyr::select(-.data$model_stock_proportion) |>
    `attr<-`('species', fram_db$fram_db_species)

  if (is.null(run_id)) {
    msp_mort
  } else {
    msp_mort |>
      dplyr::filter(.data$run_id %in% .env$run_id)
  }

}

