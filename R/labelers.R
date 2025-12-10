#' Label stocks based on FRAM database
#'
#' Like [label_stocks()], but uses an active FRAM database to label stocks, rather than
#' the look-up table present in the framrosetta package. Primarily used in `fetch_table()`,
#' useful if working with databases with unusual stock or fishery tables.
#'
#' @inheritParams framrosetta::label_stocks
#' @param fram_db FRAM database connection
#'
#' @seealso [label_stocks()], [label_fisheries_db()]
#'
#' @return `.data` with additional column, `$stock_label`
#' @export
label_stocks_db <- function(.data, fram_db, stocks_col = "stock_id"){
  validate_data_frame(.data)
  validate_fram_db(fram_db, db_type = "full")

  if(!stocks_col %in% colnames(.data)){
    cli::cli_abort('`stocks_col` ({stocks_col}) column must be present in dataframe.')
  }

  lut_use <- fetch_table_(fram_db, "Stock")

  label_map <- "stock_id"
  names(label_map) = stocks_col

  res <- .data |>
    dplyr::left_join(lut_use |>
                       dplyr::select("stock_id",
                                     "stock_label" = "stock_long_name"),
                     by = label_map)|>
    dplyr::relocate(.data$stock_label,
                    .after = {{stocks_col}})
  attr(res, "species") <- fram_db$fram_db_species
  return(res)
}


#' Label fisheries based on FRAM database
#'
#' Like [label_fisheries()], but uses an active FRAM database to label fisheries, rather than
#' the look-up table present in the framrosetta package. Primarily used in `fetch_table()`,
#' useful if working with databases with unusual stock or fishery tables.
#'
#' @inheritParams framrosetta::label_fisheries
#' @param fram_db FRAM database connection
#'
#' @seealso [label_fisheries()], [label_stocks_db()]
#'
#' @return `.data` with additional column, `$fishery_label`
#' @export
label_fisheries_db <- function(.data, fram_db, fisheries_col = "fishery_id"){
  validate_data_frame(.data)
  validate_fram_db(fram_db, db_type = "full")
  ## check if .dat has fisheries_col column. Error if not.
  if(!fisheries_col %in% colnames(.data)){
    cli::cli_abort('`fisheries_col` ({fisheries_col}) column must be present in dataframe.')
  }


  lut_use <- fetch_table_(fram_db, "Fishery")

  label_map <- "fishery_id"
  names(label_map) = fisheries_col

  ## use fram_rosetta join to add fishery_label column

  res <- .data |>
    dplyr::left_join(lut_use |>
                       dplyr::select("fishery_id",
                                     "fishery_label" = "fishery_title"),
                     by = label_map)|>
    dplyr::relocate(.data$fishery_label,
                    .after = {{fisheries_col}})
  attr(res, "species") <- fram_db$fram_db_species
  return(res)
}
