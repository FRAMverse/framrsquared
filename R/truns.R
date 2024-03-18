#' Returns a dataframe with stocks defined
#' by the TRuns report driver
#' @param db_path Path to a FRAM database.
#' @export
#' @examples
#' \dontrun{truns <- truns_stocks(fram_db)}
#'
truns_stocks <- function(fram_db){
  fram_db |>
    fetch_table('ReportDriver') |>
    dplyr::filter(driver_name == 'PSCTRuns.DRV') |>
    dplyr::select(stock_id = option1, stock_name = option5) |>
    tidyr::separate_longer_delim(stock_id, ',') |>
    dplyr::mutate(across(stock_id, as.numeric))
}

#' Returns a dataframe with fisheries defined
#' by the TRuns report driver
#' @param db_path Path to a FRAM database.
#' @export
#' @examples
#' \dontrun{truns <- truns_fisheries(fram_db)}
#'
truns_fisheries <- function(fram_db){
  fram_db |>
    fetch_table('ReportDriver') |>
    dplyr::filter(driver_name == 'PSCTRuns.DRV') |>
    dplyr::select(fishery_id = option2, stock_name = option5) |>
    tidyr::separate_longer_delim(fishery_id, ',') |>
    dplyr::mutate(across(fishery_id, as.numeric))
}

