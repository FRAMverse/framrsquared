
#' Expand Chinook mortality table using Model-Stock Proportion
#'
#' See https://framverse.github.io/fram_doc/calcs_data_chin.html#46_Model-Stock_Proportion.
#'
#' @param .data Mortality table
#' @param fram_db FRAM database object
#' @return Mortality table with mortality values expanded by msp
#' @export
#' @examples
#' \dontrun{
#' fram_db |> msp_mortality(run_id = 132)
#' }
msp_mortality = function(.data, fram_db){

  is_framdb_check(fram_db)

  if(!attr(.data, 'species') == "CHINOOK"){
    cli::cli_abort("MSP expansion only makes sense for Chinook. See https://framverse.github.io/fram_doc/calcs_data_chin.html#46_Model-Stock_Proportion")
  }
  if(!all(c("run_id", "stock_id", "age", "fishery_id", "time_step",
            "landed_catch", "non_retention", "shaker", "drop_off",
            "encounter", "msf_landed_catch", "msf_non_retention",
            "msf_shaker", "msf_drop_off", "msf_encounter" ) %in% names(.data))){
    cli::cli_abort("Data does not contain all columns of a Mortality table. Did you use the wrong data object?")
  }

  runid <- fram_db |>
    fetch_table('RunID')

  msp <- fram_db |>
    fetch_table('FisheryModelStockProportion')

  msp_run_id <- runid |>
    dplyr::inner_join(msp, by = 'base_period_id', relationship = 'many-to-many') |>
    dplyr::select(.data$run_id, .data$fishery_id, .data$model_stock_proportion)

  .data |>
    dplyr::left_join(msp_run_id, by = c('run_id', 'fishery_id')) |>
    dplyr::mutate(
      dplyr::across(
        .data$landed_catch:.data$drop_off,
        \(x) x / .data$model_stock_proportion
      )
    ) |>
    dplyr::select(-.data$model_stock_proportion)
}

