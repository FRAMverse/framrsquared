#' Calculate Terminal Run Sizes (for COHO reporting)
#'
#' Calculate the Terminal Area Abundance (TAA) and Extreme Terminal Run Size (ETRS) values used in COHO model output reporting. These match the values of column B of the `TRunsPRN` sheet of the TAMM. **WARNING!** These are NOT the same terminal run size definitions used by FRAM to interpret the TAMI rate inputs (for that, see [calculate_tami_trs()]).
#'
#' Terminal runs are defined in the "ReportDriver" table of the FRAM database. Terminal run name ("Option5") and terminal run number ("Option6") are used in combination to uniquely identify the terminal run group in these calculations. The FRAM database does not guarantee uniqueness of these combinations, and `calculate_report_trs()` will error if there are multiple rows of "ReportDriver" with duplicate Option5 x Option6. `calculate_report_trs()` *will* correctly produce output if there are duplicates in one or the other columns of the table; in this case the output of this function may have two rows with the same `$trs_id` or `$stock_group_name`. In the event of multiple rows with the same `$stock_group_name`, this function will provide a warning. This is the case for many of the long-standing databases. Be careful when using the output of this function; do not assume that `$trs_id` or `$stock_group_name` alone will uniquely identify a single row of output.
#'
#' @param fram_db FRAM database connection
#' @param run_id One or more run ids
#' @param trs_definition_number Optional. One or more identifiers for terminal run groups (`TaaNum` of TAAETRSList for `calculate_report_trs()` or `Option5` of ReportDriver for `calculate_tami_trs()`). If provided, results will be filtered to only those terminal run groups.
#'
#' @returns A data frame with one row per stock group per run, containing the
#'   following columns:
#'   \describe{
#'     \item{`run_id`}{Integer. Unique identifier for the model run.}
#'     \item{`trs_id`}{Numeric. Identifier for the terminal run size record. Matches `Option6` of FRAM table "ReportDriver".}
#'     \item{`stock_group_name`}{Character. Name of the stock group. Corresponds to `Option5` of FRAM table "ReportDriver".}
#'     \item{`escapement`}{Numeric. Escapement estimate for the stock group. Already incorporated in `terminal_run_size`, provided as additional reference.}
#'     \item{`terminal_run_size`}{Numeric. Final terminal run size value, either TAA or ETRS depending on the run type.}
#'     \item{`terminal_run_type`}{Character. Either "ETRS" or "TAA", identifying what type of terminal run definition is being used.}
#'     \item{`terminal_run_category`}{Reminder that this is the TAMI version of terminal runs, NOT the version used in reporting.}
#'   }
#'
#' @export
#'
#' @family calculate terminal run size
#'
#' @examples
#' \dontrun{
#' fram_db <- connect_fram_db("coho_postseason.mdb")
#' calculate_report_trs(fram_db, run_id = 51:53)
#' ## What if we just want the HC Unmarked ETRS? ReportDriver table shows that's 73
#' calculate_report_trs(fram_db, run_id = 51:53,
#'     trs_definition_number = 73)
#' }
calculate_report_trs <- function(fram_db, ## fram database connection
                                 run_id, ## one or more run ids
                                 trs_definition_number = NULL){  ## Optional, definition number from the TAMM

  validate_fram_db(fram_db, db_species = "COHO", db_type = "full")
  validate_run_id(fram_db, run_id = run_id)
  validate_numeric(trs_definition_number, allow_null = TRUE)

  trun_stock_df <- truns_stocks(fram_db)
  trun_fishery_df <- truns_fisheries(fram_db)


  truns <- fram_db |>
    fetch_table_("ReportDriver") |>
    dplyr::filter(.data$driver_name == 'PSCTRuns.DRV',
                  .data$species_name == "COHO") |>
    dplyr::mutate(option6 = as.numeric(.data$option6))

  if(!is.null(trs_definition_number)){
    truns <- truns |>
      dplyr::filter(.data$option6 %in% trs_definition_number)
  }

  rows_unidentifiable <- truns |>
    dplyr::select("option5", "option6") |>
    duplicated()

  if(sum(rows_unidentifiable) > 0){

    bad_names <- as.character(truns[rows_unidentifiable, "option5"])

    fram_abort(c("One or more rows of the ReportDrivers table in the FRAM database have duplicate stock group names (`Option5`) and TRS ID # (`Option6`) entries!",
                 "This will lead to incorrect calculations of TRS. **At a minimum, the Option6 entries should modified to be unique!**",
                 "!" = "Check for rows with the following Option5 entries and delete or resolve duplicated rows:",
                 setNames(bad_names, rep("*", length(bad_names))))
    )
  }

  duplicated_names <- truns |>
    dplyr::pull("option5")

  duplicated_names <- duplicated_names[duplicated(duplicated_names)]

  if(length(duplicated_names) > 0){
    cli::cli_warn(c("One or more rows of the ReportDrivers table in the FRAM database have duplicate Stock Group Names (`Option5`)!",
                 "Output of this function should be correct, but be aware that there will be multiple rows for the following stock groups:",
                 setNames(duplicated_names, rep("*", length(duplicated_names))))
    )
  }


  truns_df <- truns |>
    dplyr::mutate(stock_id = purrr::map(stringr::str_split(.data$option1, pattern = ","),
                                         as.numeric),
                  fishery_id = purrr::map(stringr::str_split(.data$option2, pattern = ","),
                                           as.numeric)
    ) |>
    dplyr::select("stock_id", "fishery_id", trun_type = "option4",
                  stock_name = "option5",
                  trs_id = "option6") |>
    dplyr::filter(!is.na(.data$trs_id)) |>
    tidyr::unnest("stock_id") |>
    tidyr::unnest("fishery_id")


  trun_stock_x_fishery <- truns_df |>
    dplyr::select("stock_name", "trs_id", "stock_id", "fishery_id") |>
    dplyr::distinct()

  trun_stock_df <- truns_df |>
    dplyr::select("stock_name", "trs_id", "stock_id") |>
    dplyr::distinct()

  trun_fishery_df <- truns_df |>
    dplyr::select("stock_name", "trs_id", "fishery_id") |>
    dplyr::distinct()

  trun_types <- truns_df |>
    dplyr::select("stock_name", "trs_id", "trun_type") |>
    dplyr::distinct()

  if(length(run_id)>1){
    expected_relationship = "many-to-many"
  } else {
    expected_relationship = NULL
  }

  escapements_df <- fram_db |>
    fetch_table_("Escapement") |>
    dplyr::filter(.data$run_id %in% .env$run_id) |>
    dplyr::inner_join(trun_stock_df,
                      by = "stock_id",
                      relationship = expected_relationship) |>
    dplyr::summarize(escapement = sum(.data$escapement, na.rm = T),
                     .by = c("run_id", "stock_name", "trs_id"))

  mort_tbl <- fram_db |>
    fetch_table_("Mortality") |>
    ## COHO is ALWAYS timesteps 4:5!
    dplyr::filter(.data$run_id %in% .env$run_id,
                  .data$time_step %in% 4:5) |>
    dplyr::mutate(catch = .data$landed_catch + .data$msf_landed_catch)


  mort_tbl_fisherywise <- mort_tbl |>
    dplyr::summarize(catch = sum(.data$catch),
                     .by = c("fishery_id", "run_id"))

  catch_taa_df <- trun_fishery_df |>
    dplyr::inner_join(mort_tbl_fisherywise,
                      by = "fishery_id",
                      relationship = expected_relationship) |>
    dplyr::summarize(catch_taa = sum(.data$catch),
                     .by = c("run_id", "stock_name", "trs_id"))

  catch_etrs_df <- trun_stock_x_fishery |>
    dplyr::inner_join(mort_tbl,
                      by = c("fishery_id", "stock_id"),
                      relationship = "many-to-many") |>
    dplyr::summarize(catch_etrs = sum(.data$catch),
                     .by = c("run_id", "stock_name", "trs_id"))


  out <- dplyr::inner_join(escapements_df,
                           catch_taa_df,
                           by = c("run_id", "stock_name", "trs_id"),
                           relationship = expected_relationship) |>
    dplyr::inner_join(catch_etrs_df,
                      by = c("run_id", "stock_name", "trs_id"),
                      relationship = expected_relationship) |>
    dplyr::mutate(taa = .data$escapement + .data$catch_taa,
                  etrs = .data$escapement + .data$catch_etrs) |>
    dplyr::left_join(trun_types,
                     by = c("stock_name", "trs_id")) |>
    dplyr::rename(stock_group_name = "stock_name") |>
    dplyr::arrange(.data$trs_id) |>
    dplyr::select(-"catch_taa", -"catch_etrs") |>
    dplyr::mutate(terminal_run_size = dplyr::recode_values(.data$trun_type,
                                                           "TAA" ~ .data$taa,
                                                           "ETRS" ~ .data$etrs,
                                                           default = NA_real_
                                                           ))

  out <- out |>
    dplyr::select(
      "run_id",
      taa_num = "trs_id",
      taa_name = "stock_group_name",
      "escapement",
      "terminal_run_size",
      terminal_run_type = "trun_type"
    ) |>
    dplyr::mutate(terminal_run_category = "for reporting; uses ReportDrivers definitions")
  return(out)
}

#' Calculate terminal run sizes as used by the tami
#'
#' Calculate the Terminal Area Abundance (TAA) and Extreme Terminal Run Size (ETRS) values used in Coho FRAM calculations to translate TAMI rates into units of fish / effort.  **WARNING!** These are NOT the same terminal run size definitions used by FRAM in the reporting process (e.g., column B of the `TRunsPRN` in the TAMM). For that, see [calculate_report_trs()].
#'
#' Terminal runs are defined in the "TAAETRSList" table of the FRAM database. Terminal run name ("TaaName") and terminal run number ("TaaNum") are used in combination to uniquely identify the terminal run group in these calculations. The FRAM database does not guarantee uniqueness of these combinations, and `calculate_tami_trs()` will error if there are multiple rows of "TAAETRSList" with duplicate Option5 x Option6. `calculate_tami_trs()` *will* correctly produce output if there are duplicates in one or the other columns of the table; in this case the output of this function may have two rows with the same `$trs_id` or `$stock_group_name`. In the event of multiple rows with the same `$stock_group_name`, this function will provide a warning. Be careful when using the output of this function; do not assume that `$trs_id` or `$stock_group_name` alone will uniquely identify a single row of output.
#'
#' @inheritParams calculate_report_trs
#'
#' @returns A data frame with one row per stock group per run, containing the
#'   following columns:
#'   \describe{
#'     \item{`run_id`}{Integer. Unique identifier for the model run.}
#'     \item{`trs_id`}{Numeric. Identifier for the terminal run size record. Matches `TaaNum` of FRAM table "TAAETRSList".}
#'     \item{`stock_group_name`}{Character. Name of the stock group. Corresponds to `TaaName` of FRAM table "TAAETRSList".}
#'     \item{`escapement`}{Numeric. Escapement estimate for the stock group. Already incorporated in `terminal_run_size`, provided as additional reference.}
#'     \item{`terminal_run_size`}{Numeric. Final terminal run size value, either TAA or ETRS depending on the run type.}
#'     \item{`terminal_run_type`}{Character. Either "ETRS" or "TAA", identifying what type of terminal run definition is being used.}
#'     \item{`terminal_run_category`}{Reminder that this is the REPORTING version of terminal runs, NOT the set that the TAMI sheet is using.}
#'   }
#'
#'
#' @export
#'
#' @family calculate terminal run size
#'
#' @examples
#' \dontrun{
#' fram_db <- connect_fram_db("coho_postseason.mdb")
#' calculate_tami_trs(fram_db, run_id = 51:53)
#' ## What if we just want the Nooksack TAA? TAAETRSList table shows that's number 18
#' calculate_tami_trs(fram_db, run_id = 51:53,
#'     trs_definition_number = 18)
#' }
calculate_tami_trs <- function(fram_db, ## fram database connection
                               run_id, ## one or more run ids
                               trs_definition_number = NULL){  ## Optional, definition number from the TAMM

  validate_fram_db(fram_db, db_species = "COHO", db_type = "full")
  validate_run_id(fram_db, run_id = run_id)
  validate_numeric(trs_definition_number, allow_null = TRUE)

  taa_etrs <- fram_db |>
    fetch_table_("TAAETRSList")

  rows_unidentifiable <- taa_etrs |>
    dplyr::select("taa_num", "taa_name") |>
    duplicated()

  if(sum(rows_unidentifiable) > 0){

    bad_names <- as.character(taa_etrs[rows_unidentifiable, "taa_name"])

    fram_abort(c("One or more rows of the TAAETRSList table in the FRAM database have duplicated `taa_num` and `taa_name` entries!",
    "!" = "Check taa groups with the following names, and delete or resolve duplicated rows:",
                 setNames(bad_names, rep("*", length(bad_names))))
    )
  }

  taa_etrs_df <- taa_etrs |>
    dplyr::mutate(stock_vec = purrr::map(stringr::str_split(.data$taa_stk_list, pattern = ","),
                                         as.numeric),
                  fishery_vec = purrr::map(stringr::str_split(.data$taa_fish_list, pattern = ","),
                                           as.numeric),
                  time_step_vec = purrr::map2(.x = .data$taa_time_step1,
                                              .y = .data$taa_time_step2,
                                              .f = \(x, y) x:y)
    ) |>
    dplyr::select("taa_num", "taa_name", "stock_vec", "fishery_vec", "time_step_vec", is_type_taa = "taa_type")

  taa_etrs_stock <- taa_etrs_df |>
    dplyr::select("taa_num", "taa_name", "stock_vec") |>
    tidyr::unnest("stock_vec") |>
    dplyr::rename("stock_id" = "stock_vec")

  taa_etrs_fishery <- taa_etrs_df |>
    dplyr::select("taa_num", "taa_name", "fishery_vec") |>
    tidyr::unnest("fishery_vec") |>
    dplyr::rename("fishery_id" = "fishery_vec")

  taa_etrs_stock_x_fishery <- taa_etrs_df |>
    dplyr::select("taa_num", "taa_name", "stock_vec", "fishery_vec") |>
    tidyr::unnest("stock_vec") |>
    tidyr::unnest("fishery_vec") |>
    dplyr::rename("stock_id" = "stock_vec") |>
    dplyr::rename("fishery_id" = "fishery_vec")

  taa_etrs_type <- taa_etrs_df |>
    dplyr::select("taa_num", "taa_name", "is_type_taa")



  if(length(run_id)>1){
    expected_relationship = "many-to-many"
  } else {
    expected_relationship = NULL
  }

  escapements_df <- fram_db |>
    fetch_table_("Escapement") |>
    dplyr::filter(.data$run_id %in% .env$run_id) |>
    dplyr::inner_join(taa_etrs_stock,
                      by = "stock_id",
                      relationship = expected_relationship) |>
    dplyr::summarize(escapement = sum(.data$escapement, na.rm = T),
                     .by = c("run_id", "taa_name", "taa_num"))

  mort_tbl <- fram_db |>
    fetch_table_("Mortality") |>
    ## COHO is ALWAYS timesteps 4:5!
    dplyr::filter(.data$run_id %in% .env$run_id,
                  .data$time_step %in% 4:5) |>
    dplyr::mutate(catch = .data$landed_catch + .data$msf_landed_catch)


  mort_tbl_fisherywise <- mort_tbl |>
    dplyr::summarize(catch = sum(.data$catch),
                     .by = c("fishery_id", "run_id"))

  catch_taa_df <- taa_etrs_fishery |>
    dplyr::inner_join(mort_tbl_fisherywise,
                      by = "fishery_id",
                      relationship = expected_relationship) |>
    dplyr::summarize(catch_taa = sum(.data$catch),
                     .by = c("run_id", "taa_name", "taa_num"))

  catch_etrs_df <- taa_etrs_stock_x_fishery |>
    dplyr::inner_join(mort_tbl,
                      by = c("fishery_id", "stock_id"),
                      relationship = "many-to-many") |>
    dplyr::summarize(catch_etrs = sum(.data$catch),
                     .by = c("run_id", "taa_name", "taa_num"))


  out <- dplyr::inner_join(escapements_df,
                           catch_taa_df,
                           by = c("run_id", "taa_name", "taa_num"),
                           relationship = expected_relationship) |>
    dplyr::inner_join(catch_etrs_df,
                      by = c("run_id", "taa_name", "taa_num"),
                      relationship = expected_relationship) |>
    dplyr::mutate(taa = .data$escapement + .data$catch_taa,
                  etrs = .data$escapement + .data$catch_etrs) |>
    dplyr::left_join(taa_etrs_type, by = c("taa_name", "taa_num"),
                     relationship = expected_relationship) |>
    dplyr::relocate("taa_num", .after = "run_id") |>
    dplyr::arrange(.data$taa_num) |>
    dplyr::select(-"catch_taa", -"catch_etrs") |>
    dplyr::mutate(trs_value = dplyr::recode_values(.data$is_type_taa,
                                             1 ~ .data$taa,
                                             0 ~ .data$etrs,
                                             default = NA_real_),
                  trs_type = dplyr::recode_values(.data$is_type_taa,
                                            1 ~ "TAA",
                                            0 ~ "ETRS",
                                            default = "UNKNOWN")) |>
    dplyr::select("run_id", "taa_num", "taa_name", "escapement", terminal_run_size = "trs_value", terminal_run_type = "trs_type") |>
    dplyr::mutate(terminal_run_category = "for HR inputs / TAMI; uses TAAETRSList definitions")

  if(!is.null(trs_definition_number)){
    out <- out |>
      dplyr::filter(.data$taa_num %in% trs_definition_number)
  }

  return(out)
}

