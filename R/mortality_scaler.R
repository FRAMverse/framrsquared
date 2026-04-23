#' Quantify the proportion of fishery mortalities associated with stock(s) of interest
#'
#' Supports guestimating the impact of making changes to a fishery on a particular stock (or group
#' of stocks) by finding the ratio of mortalities in each fishery that can be attributed to the
#' focal stock or stocks. Accounts for *all* sources of mortality (e.g., includes non-retention,
#' dropoff, etc). Chinook runs are returned in units of AEQ.
#'
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @param stock_id A focal stock or stocks.
#' @param msp Should we use MSP (Model Stock Proportion) expansion? Logical, defaults to FALSE. Only
#'   relevant for Chinook.
#'
#' @returns Tibble identify run, fishery, and timestep. `$fishery_mortality` gives the total
#'   mortalities in each fishery x timestep, `$stock_mortality` gives the total mortalities of the
#'   focal stock or stocks, and `$stock_mortality_ratio` gives the fraction of fishery mortalities
#'   that can be attributed to the focal stock or stocks.
#'
#' @export
#'
#' @seealso [plot_impacts_per_catch_heatmap()]
#'
#' @examples
#' \dontrun{fram_db |> mortality_scalers(run_id = 101, stock_id = c(17:18))}
mortality_scalers <- function(fram_db, run_id, stock_id, msp = FALSE) {
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)
  validate_stock_ids(fram_db, stock_id)
  validate_flag(msp)

  switch(
    fram_db$fram_db_species,
    'CHINOOK' = mortality_scalers_chinook_(fram_db, run_id, stock_id, msp = msp),
    'COHO' = mortality_scalers_coho_(fram_db, run_id, stock_id)
  )
}

#' Sum separate mortality columns into new "total_mortality" column
#'
#' Convenience function for combining the separate mortality columns of the Mortality table. Note:
#' this does *not* account for AEQ for Chinook on its own, but can be used on the output of [aeq_mortality()]
#' to give total mortalities in AEQs.
#'
#' @param .data Dataframe with separate mortality columns `landed_catch`, `non_retention`, `shaker`,
#'   `drop_off`, and `msf_` versions of each. Typically comes from `fetch_table("Mortality")` or
#'   [aeq_mortality()].
#'
#' @return `.data` with additional `$total_mortality` column (numeric vector) just before `$landed_catch`.
#' @export
#'
add_total_mortality = function(.data){
  validate_data_frame(.data)

  mort_col_names = c("landed_catch", "non_retention", "shaker", "drop_off",
                     "msf_landed_catch", "msf_non_retention",
                     "msf_shaker", "msf_drop_off")
  if(!all(mort_col_names %in% names(.data))){
    cli::cli_abort("`.data` should be a mortality table or derivative! One or more key columns missing: {setdiff(mort_col_names, names(.data))}")
  }

  .data |>
    dplyr::mutate(
      total_mortality = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off +
        .data$msf_landed_catch + .data$msf_non_retention +
        .data$msf_shaker + .data$msf_drop_off,
      .before = .data$landed_catch
    )
}

#' Coho-specific implementation for mortality scalers
#' @keywords internal
mortality_scalers_coho_ <- function(fram_db, run_id, stock_id) {
  scalers <- fram_db |>
    fetch_table_('Mortality') |>
    dplyr::filter(run_id == .env$run_id)

  scalers |>
    add_total_mortality()|>
    dplyr::group_by(.data$run_id, .data$fishery_id, .data$time_step) |>
    dplyr::summarize(
      fishery_mortality = sum(.data$total_mortality, na.rm = T),
      stock_mortality = sum(.data$total_mortality[stock_id %in% .env$stock_id], na.rm = T),
      .groups = 'drop'
    ) |>
    dplyr::mutate(stock_mortality_ratio = .data$stock_mortality / .data$fishery_mortality) |>
    `attr<-`('species', fram_db$fram_db_species) |>
    framrosetta::label_fisheries()
}


#' Chinook-specific implementation for mortality scalers
#' @keywords internal
mortality_scalers_chinook_ <- function(fram_db, run_id, stock_id, msp) {

 fram_db |>
    aeq_mortality_(run_id = run_id,
                       msp = msp) |>
    add_total_mortality() |>
    dplyr::group_by(.data$run_id, .data$fishery_id, .data$time_step) |>
    dplyr::summarize(
      fishery_mortality = sum(.data$total_mortality, na.rm = T),
      stock_mortality = sum(.data$total_mortality[stock_id %in% .env$stock_id], na.rm = T),
      .groups = 'drop'
    ) |>
    dplyr::mutate(stock_mortality_ratio = .data$stock_mortality / .data$fishery_mortality) |>
    `attr<-`('species', fram_db$fram_db_species)|>
   framrosetta::label_fisheries()

}


