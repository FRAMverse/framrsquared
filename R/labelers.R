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
label_stocks_db <- function(.data, fram_db){
  validate_data_frame(.data)
  validate_fram_db(fram_db, db_type = "full")

  ## labeling
  run_id_luts = fetch_table_(fram_db, "RunID") |>
    dplyr::select("run_id", "base_period_id")
  bp_lut <- fetch_table_(fram_db, "BaseID") |>
    dplyr::select("stock_version", "species_name", "base_period_id")

  run_id_luts <- run_id_luts |>
    dplyr::full_join(bp_lut, by = "base_period_id") |>
    dplyr::filter(!is.na(.data$run_id))

  stock_lut <- fetch_table_(fram_db, "Stock")

  lut_use <- run_id_luts |>
    dplyr::left_join(stock_lut, by = c("stock_version", "species_name" = "species"),
              relationship = "many-to-many") |>
    dplyr::select("stock_id", "run_id", stock_label = "stock_long_name")


  res <- .data |>
    dplyr::left_join(lut_use,
                     by = c("run_id", "stock_id"))|>
    dplyr::relocate(.data$stock_label,
                    .after = "stock_id")
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
label_fisheries_db <- function(.data, fram_db){
  validate_data_frame(.data)
  validate_fram_db(fram_db, db_type = "full")

  ## labeling
  run_id_luts = fetch_table_(fram_db, "RunID") |>
    dplyr::select("run_id", "base_period_id")
  bp_lut <- fetch_table_(fram_db, "BaseID") |>
    dplyr::select("fishery_version", "species_name", "base_period_id")

  run_id_luts <- run_id_luts |>
    dplyr::full_join(bp_lut, by = "base_period_id") |>
    dplyr::filter(!is.na(.data$run_id))

  fishery_lut <- fetch_table_(fram_db, "Fishery") |>
    dplyr::rename(fishery_version = "version_number")

  lut_use <- run_id_luts |>
    dplyr::left_join(fishery_lut,
                     by = c("fishery_version", "species_name" = "species"),
              relationship = "many-to-many") |>
    dplyr::select("fishery_id", "run_id", fishery_label = "fishery_title")


  ## use fram_rosetta join to add fishery_label column

  res <- .data |>
    dplyr::left_join(lut_use,
                     by = c("fishery_id", "run_id"))|>
    dplyr::relocate(.data$fishery_label,
                    .after = "fishery_id")
  attr(res, "species") <- fram_db$fram_db_species
  return(res)
}
