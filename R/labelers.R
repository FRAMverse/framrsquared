#' Label stocks based on FRAM database
#'
#' Like [framrosetta::label_stocks()], but uses an active FRAM database to label stocks, rather than
#' the look-up table present in the framrosetta package. Primarily used in `fetch_table()`, robust to changes in base period.
#'
#' @inheritParams framrosetta::label_stocks
#' @param fram_db FRAM database connection
#'
#' @family labelers
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
    dplyr::relocate("stock_label",
                    .after = "stock_id")
  attr(res, "species") <- fram_db$fram_db_species
  return(res)
}


#' Label fisheries based on FRAM database
#'
#' Like [framrosetta::label_fisheries()], but uses an active FRAM database to label fisheries, rather than
#' the look-up table present in the framrosetta package. Primarily used in `fetch_table()`, robust to changes
#' in base period.
#'
#' @inheritParams framrosetta::label_fisheries
#' @param fram_db FRAM database connection
#'
#' @family labelers
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
    dplyr::relocate("fishery_label",
                    .after = "fishery_id")
  attr(res, "species") <- fram_db$fram_db_species
  return(res)
}

#' Label timesteps based on FRAM database
#'
#' @param .data Database with `time_step` column.
#' @param fram_db FRAM database connection
#'
#' @family labelers
#'
#' @return `.data` with additional column, `$time_step_label`
#' @export
label_timesteps_db = function(.data, fram_db){
  validate_data_frame(.data)
  validate_fram_db(fram_db, db_type = "full")

  time_step_lut = fram_db |>
    fetch_table_("TimeStep") |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::mutate(time_step_label = as.factor(glue::glue("{time_step_id} ({time_step_name})"))) |>
    dplyr::select(time_step = "time_step_id",
                  "time_step_label")
  .data |>
    dplyr::left_join(time_step_lut,
                     by = "time_step")
}



#' Provide flag translations to dataframe
#'
#' Adds a column with a text version of flags for either non-retention or fishery scalers.
#'
#'
#' @param .data Dataframe with either `$fishery_flag` or `$non_retention_flag` columns. Typically a fetched FisheryScalers or NonRetentions table.
#' @param species Optional, identifying species if `.data` doesn't. If provided, should be "CHINOOK" or "COHO" (or variants)
#' @param warn Logical, defaults to TRUE. Warn if neither flag column is present in dataframe?
#'
#' @returns dataframe `.data` with additional character vector columns `$fishery_flag_label` and/or `non_retention_flag_label` depending on the presence of `$fishery_flag` and `$non_retention`.
#'
#' @export
#'
#' @family labelers
#'
#' @examples
#' \dontrun{ fram_db |>
#' fetch_table("FisheryScalers") |>
#' label_flags()
#' }
label_flags = function(.data,
                       species = NULL,
                       warn = TRUE) {
  validate_data_frame(.data)
  species = validate_species(.data, species)
  if(!any(c("fishery_flag", "non_retention_flag") %in% names(.data))){
    if(warn){
      cli::cli_alert_warning("Missing 'fishery_flag' or 'non_retention_flag' column in data")
    }
  } else {
    if ("fishery_flag" %in% names(.data)) {
      .data <- .data |>
        dplyr::mutate(fishery_flag_label = translate_scalers_flag(.data$fishery_flag),
                      .after = "fishery_flag")
    }
    if("non_retention_flag" %in% names(.data)){
      if(species == "CHINOOK"){
        .data <- .data |>
          dplyr::mutate(non_retention_flag_label = translate_nr_flag(.data$non_retention_flag),
                        .after ="non_retention_flag")
      } else {
        .data <- .data |>
          dplyr::mutate(non_retention_flag_label = "Total dead fish",
                        .after ="non_retention_flag")
      }
    }
  }
  return(.data)
}

