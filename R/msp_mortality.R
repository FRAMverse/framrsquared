
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
    fetch_table_('RunID')


  msp <- fram_db |>
    fetch_table_('FisheryModelStockProportion')

  if(!is.null(run_id)){
    bps_used <- runid |>
      dplyr::filter(.data$run_id %in% .env$run_id) |>
      dplyr::pull(base_period_id)
    if(!all(bps_used %in% msp$base_period_id)){
      fram_abort("Base Period ID of run must be represented in {.emph FisheryModelStockProportion} table!")
    }

  } else {
    if(!all(runid$base_period_id %in% msp$base_period_id)){
      bad_bp <- setdiff(runid$base_period_id, msp$base_period_id)
      bad_runs <- runid |>
        dplyr::filter(.data$base_period_id %in% .env$bad_bp) |>
        dplyr::pull("run_id")
      cli::cli_warn(c("One or more runs have base_period_ids that are not represented in {.emph FisheryModelStockProportion} table. Those runs will have NAs for all fishery mortalities!",
                      "Problem run_ids: {bad_runs}"),
                    class = "framrsquared_warning")
    }
  }

  mortality <- fram_db |>
    fetch_table_('Mortality')

  msp_run_id <- runid |>
    dplyr::inner_join(msp, by = 'base_period_id', relationship = 'many-to-many') |>
    dplyr::select("run_id", "fishery_id", "model_stock_proportion")

  if(!is.null(run_id) &&
     run_id %in% msp_run_id$run_id &&
     !(run_id %in% unique(mortality$run_id))){
    fram_abort("Run_id must be represented in Mortality table. Has this FRAM run been run? Run IDs available: {.val {unique(mortality$run_id)}}.")
  }

  msp_mort <- mortality |>
    dplyr::left_join(msp_run_id, by = c('run_id', 'fishery_id')) |>
    dplyr::mutate(
      dplyr::across(
        c("landed_catch":"drop_off",
          "msf_landed_catch":"msf_drop_off"),
        \(x) x / .data$model_stock_proportion
      )
    ) |>
    dplyr::select(-"model_stock_proportion") |>
    `attr<-`('species', fram_db$fram_db_species)

  if (is.null(run_id)) {
    msp_mort
  } else {
    msp_mort |>
      dplyr::filter(.data$run_id %in% .env$run_id)
  }

}

