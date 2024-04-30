#' Returns a dataframe with stocks defined
#' by the TRuns report driver
#' @param fram_db FRAM database object
#' @export
#' @examples
#' \dontrun{truns <- truns_stocks(fram_db)}
#'
truns_stocks <- function(fram_db){
  validate_fram_db(fram_db, 'full', 'COHO')
  fram_db |>
    fetch_table('ReportDriver') |>
    dplyr::filter(.data$driver_name == 'PSCTRuns.DRV') |>
    dplyr::select(stock_id = .data$option1, stock_name = .data$option5) |>
    tidyr::separate_longer_delim(.data$stock_id, ',') |>
    dplyr::mutate(dplyr::across(.data$stock_id, as.numeric))
}

#' Returns a dataframe with fisheries defined
#' by the TRuns report driver
#' @param fram_db FRAM database object
#' @export
#' @examples
#' \dontrun{truns <- truns_fisheries(fram_db)}
#'
truns_fisheries <- function(fram_db){
  validate_fram_db(fram_db, 'full', 'COHO')
  fram_db |>
    fetch_table('ReportDriver') |>
    dplyr::filter(.data$driver_name == 'PSCTRuns.DRV') |>
    dplyr::select(fishery_id = .data$option2, stock_name = .data$option5) |>
    tidyr::separate_longer_delim(.data$ishery_id, ',') |>
    dplyr::mutate(dplyr::across(.data$fishery_id, as.numeric))
}

