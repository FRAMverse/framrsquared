#' Stocks defined by TRuns report driver
#'
#' Returns a dataframe with stocks defined by the TRuns report driver. Only relevant for Coho databases.
#'
#' @param fram_db FRAM database object
#'
#' @returns Tibble with stock ID and TRUN stock name (`stock_name`).
#'
#' @export
#'
#' @seealso [truns_fisheries()]
#'
#' @examples
#' \dontrun{truns <- truns_stocks(fram_db)}
#'
truns_stocks <- function(fram_db){
  validate_fram_db(fram_db, 'full', 'COHO')
  fram_db |>
    fetch_table_('ReportDriver') |>
    dplyr::filter(.data$driver_name == 'PSCTRuns.DRV') |>
    dplyr::select(stock_id = "option1", stock_name = "option5") |>
    tidyr::separate_longer_delim(.data$stock_id, ',') |>
    dplyr::mutate(dplyr::across("stock_id", as.numeric))
}

#' Fisheries defined by TRuns report driver
#'
#' Returns a dataframe with fisheries defined by the TRuns report driver. Only relevant for Coho databases.
#'
#' @param fram_db FRAM database object
#'
#' @returns Tibble with fishery ID and TRUN stock name (`stock_name`).
#'
#' @export
#'
#' @seealso [truns_stocks()]
#'
#' @examples
#' \dontrun{truns <- truns_fisheries(fram_db)}
#'
truns_fisheries <- function(fram_db){
  validate_fram_db(fram_db, 'full', 'COHO')
  fram_db |>
    fetch_table_('ReportDriver') |>
    dplyr::filter(.data$driver_name == 'PSCTRuns.DRV') |>
    dplyr::select(fishery_id = "option2", stock_name = "option5") |>
    tidyr::separate_longer_delim(.data$fishery_id, ',') |>
    dplyr::mutate(dplyr::across(.data$fishery_id, as.numeric))
}

