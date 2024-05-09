#' Guestimate the impact on a particular stock by multiplying its mortalities
#' by the `stock_mortality_ratio` produced by these functions.
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @param stock_id A focal stock or stocks
#' @export
#' @examples
#' \dontrun{fram_db |> mortality_scalers(run_id = 101, stock_id = c(17:18))}
mortality_scalers <- function(fram_db, run_id, stock_id) {
  # checks on the run id
  if (is.null(run_id)) {
    cli::cli_abort('Run ID must be provided.')
  }
  if (!is.numeric(run_id)) {
    cli::cli_abort('Run ID must be and integer')
  }

  validate_fram_db(fram_db)
  switch(
    fram_db$fram_db_species,
    'CHINOOK' = mortality_scalers_chinook_(fram_db, run_id, stock_id),
    'COHO' = mortality_scalers_coho_(fram_db, run_id, stock_id)
  )
}



mortality_scalers_coho_ <- function(fram_db, run_id, stock_id) {
  scalers <- fram_db |>
    fetch_table('Mortality') |>
    dplyr::filter(run_id == .env$run_id)

  scalers |>
    dplyr::mutate(
      total_mortality = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off +
        .data$msf_landed_catch + .data$msf_non_retention +
        .data$msf_shaker + .data$msf_drop_off
    ) |>
    dplyr::group_by(.data$run_id, .data$fishery_id, .data$time_step) |>
    dplyr::summarize(
      fishery_mortality = sum(.data$total_mortality, na.rm = T),
      stock_mortality = sum(.data$total_mortality[stock_id %in% .env$stock_id], na.rm = T),
      .groups = 'drop'
    ) |>
    dplyr::mutate(stock_mortality_ratio = .data$stock_mortality / .data$fishery_mortality) |>
    `attr<-`('species', fram_db$fram_db_species)
}


mortality_scalers_chinook_ <- function(fram_db, run_id, stock_id) {
  scalers <- fram_db |>
    fetch_table('Mortality') |>
    dplyr::filter(run_id == .env$run_id)

  AEQ <- fram_db |>
    fetch_table('AEQ')
  cli::cli_text(c('Output in terms of ', cli::col_yellow('AEQs')))
  scalers |>
    dplyr::inner_join(AEQ,  by = c('age', 'stock_id', 'time_step')) |>
    dplyr::mutate(dplyr::across(
      c(
        .data$landed_catch:.data$drop_off,
        .data$msf_landed_catch:.data$msf_drop_off
      ),
      \(x) x * .data$aeq
    )) |>
    dplyr::mutate(
      total_mortality = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off +
        .data$msf_landed_catch + .data$msf_non_retention +
        .data$msf_shaker + .data$msf_drop_off
    ) |>
    dplyr::group_by(.data$run_id, .data$fishery_id, .data$time_step) |>
    dplyr::summarize(
      fishery_mortality = sum(.data$total_mortality, na.rm = T),
      stock_mortality = sum(.data$total_mortality[stock_id %in% .env$stock_id], na.rm = T),
      .groups = 'drop'
    ) |>
    dplyr::mutate(stock_mortality_ratio = .data$stock_mortality / .data$fishery_mortality) |>
    `attr<-`('species', fram_db$fram_db_species)

}
