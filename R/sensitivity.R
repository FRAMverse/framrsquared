#' Generate sensitivity analyses runs scaled by template values
#'
#' From a template FRAM run, for a single vector of scaling factors (e.g., `c(0.5, 2)` would test halving and doubling), generate sensitivity analyses which rescale the columns specied in (`cols_to_vary`) for rows which match the conditions specified in `match_df`. Optionally creates corresponding tamms from a template TAMM, labeled to work with folder loading option in the FRAM multi-run fork.
#'
#' Dev note: update to allow match_df to NOT start with "match_" -- it's implied.
#'
#' @param fram_db Fram database
#' @param template_run Run ID of the run that should be used as a template for the sensitivity analyses.
#' @param table_name Name of FRAM table that will be modified for the sensitivity analyses
#' @param match_df dataframe that defines which rows should be modified during sensitivity analyses. To modify some values for marked and unmarked Stillaguamish stocks, we would use data.frame(StockID = c(17, 18)). To modify values only for Stillaguamish age 2s, we would use expand_grid(StockID = c(17, 19), Age = 2). Unlike match/replace dataframes for `modify_table()`, column names do not need to start with "match_" (but this function will still work if they do).
#' @param scale_values Numeric vector of the scaling factors to be be used, one per sensitivity analysis run. Defines the number of runs generated. For example, `scale_values = 2:10` would generate 9 runs. The first would multiply the values of interest by 2, the second by 3, etc.
#' @param cols_to_vary Character or character vector of column names of FRAM table `table_name` that should be rescaled.
#' @param tamm_template Optional; character string of filepath of a TAMM to be used as a template. If provided (and tamm_target_folder provided), `sensitivity_scaled` will make a tamm for each sensitivity analysis run, using names that work with the FRAM multirun fork "Use folder" option.
#' @param label Label added to each of the generated run names to identify this sequence of sensitivity analyses. String, defaults to "sensitivity"
#' @param tamm_target_folder Folder to copy TAMMs into. Will create if it does not exist.
#' @param save_log Should a log .csv of the specifics used (row ids, match criterion, scaling or replacement values) be saved in the same folder as the FRAM database? Logical, defaults to TRUE.
#'
#' @return Invisibly returns a list of dataframes.
#'   $scales_by_runs contains a row for each sensitivity run and maps the scaling factors to run ids.
#'   $full_df is the full match/scale factor used by calc_fram_scaling, and shows the match conditions and scaling used for each run.
#' @export
#'
#' @examples
#' \dontrun{
#' # Testing sensitivity_scaled
#' library(here)
#' fram_db <- connect_fram_db(here("Valid2024_sens_test.mdb"))
#'
#' tamm_template <- "ChinValidrunTest.xlsx"
#' tamm_target_folder <- here("sens_test/")
#' fram_db |>
#'   sensitivity_scaled(
#'     template_run = 28,
#'     table_name = "StockRecruit",
#'     match_df = data.frame(match_StockID = c(17, 19)),
#'     scale_values = seq(0.1, 2, length = 10),
#'     cols_to_vary = c("RecruitScaleFactor", "RecruitCohortSize"),
#'     tamm_template = tamm_template,
#'     tamm_target_folder = tamm_target_folder
#'   )
#' disconnect_fram_db(fram_db)
#' }
sensitivity_scaled <- function(fram_db,
                               template_run,
                               table_name,
                               match_df,
                               scale_values,
                               cols_to_vary,
                               tamm_template = NULL,
                               tamm_target_folder = NULL,
                               label = "sensitivity",
                               save_log = TRUE) {
  ## error checking:
  validate_fram_db(fram_db)
  ## check that template_run exists in db
  validate_run_id(fram_db, template_run)
  ## check table name
  validate_table(fram_db, table_name)
  ## check that match_df colnames match names in table
  colnames_provided <- names(match_df) |>
    stringr::str_remove("match_")
  colnames_available <- fetch_table_colnames(fram_db, table_name)

  if (!all(colnames_provided %in% colnames_available)) {
    cli::cli_abort(c("Column names of `match_df` must match columns of table `table_name`!",
      "The following do not match: {setdiff(colnames_provided, colnames_available)}.",
      "i" = "Did you use framrsquared style naming (snake_case) instead of FRAM db style naming (CamelCase)?",
      "i" = "Did you identify the right table with `table_name`?"
    ))
  }
  ## check scale_values are numeric, positive
  if (!is.numeric(scale_values)) {
    cli::cli_abort("`scale_values` must be a numeric vector!")
  }
  if (any(scale_values < 0)) {
    cli::cli_abort("`scale_values` must not include negative values!")
  }
  ## check that tamm_template exists
  if (!file.exists(tamm_template)) {
    cli::cli_abort("`tamm_template` does not exist! Make sure argument includes full file path. ({{here}} package makes it easy to convert relative to absolute filepath)")
  }
  ## check that both or neither TAMM object exists
  if (is.null(tamm_template) + is.null(tamm_target_folder) == 1) {
    cli::cli_abort("TAMM copying only works if both `tamm_template` and `tamm_target_folder` are provided; only one of those arguments is defined!")
  }


  ## relabel match_df headers as needed
  ind.nomatch <- grep("^match_", names(match_df), invert = TRUE)
  if (length(ind.nomatch) > 0) {
    names(match_df)[ind.nomatch] <- paste0("match_", names(match_df)[ind.nomatch])
  }


  ## make copies of template run
  run_ids <- fram_db |>
    copy_run(
      target_run = template_run,
      times = length(scale_values),
      label = "sensitivity"
    )

  cli::cli_alert("Generating {length(run_ids)} new runs for sensitivity analysis, with RunIDs from {min(run_ids)} to {max(run_ids)}.")

  ## if appropriate, copy TAMMs
  if (!is.null(tamm_template) & !is.null(tamm_target_folder)) {
    cli::cli_alert("Making copies of {tamm_template} into {tamm_target_folder}.")
    copy_tamms(
      tamm_name = tamm_template,
      target_folder = tamm_target_folder,
      run_id_vec = run_ids
    )
  } else {
    cli::cli_alert("No tamm specifications, skipping copying TAMMs.")
  }

  scale_df <- as.data.frame(scale_values)
  scale_df <- scale_df[, rep(1, length(cols_to_vary)), drop = FALSE]
  names(scale_df) <- glue::glue("scale_{cols_to_vary}")



  scale_df <- cbind(
    match_RunID = run_ids,
    scale_df
  )
  df <- tidyr::expand_grid(scale_df, match_df)
  df_replace <- fram_db |>
    calc_fram_scaling(
      table_name = table_name,
      df = df
    )
  modified <- modify_table(fram_db, table_name, df_replace)
  cli::cli_alert_success("Successfully generated sensitivity analyses!")
  invisible(list(scales_by_runs = scale_df, full_df = df))
  if (save_log) {
    db_path <- dirname(fram_db$fram_db_connection@info$dbname)
    log_name <- get_unique_filename(paste0(db_path, "/sensitivity_log - ", label, ".csv"))
    readr::write_csv(df, file = log_name)
    cli::cli_alert_success("Log for run: {log_name}.")
  }
}

get_unique_filename <- function(base_name, counter = 1, pad_width = 3) {
  # Split the base name into name and extension
  name_parts <- tools::file_path_sans_ext(base_name)
  extension <- tools::file_ext(base_name)

  # Otherwise, find the next available number
  repeat {
    new_name <- sprintf("%s_%0*d.%s", name_parts, pad_width, counter, extension)
    if (!file.exists(new_name)) {
      return(new_name)
    }
    counter <- counter + 1
  }
}

#' Generate sensitivity analyses runs based on exact values
#'
#' As `sensitivity_scaled`, but provide exact values for the sensitivity analyses (in argument `exact_values`) instead of
#' scaling factors.
#'
#' @inheritParams sensitivity_scaled
#' @param exact_values numeric vector of values to exact values to use for sensitivity analyses.
#'
#' @return Invisibly returns a list of dataframes.
#'   $values_by_run contains a row for each sensitivity run and maps the values used to run ids.
#'   $full_df is the full match/scale factor used by calc_fram_scaling, and shows the match conditions and scaling used for each run.
#'   If `cols_to_vary` has length 1, the two dataframes will contain the same information.
#' @export
#'
#' @examples
#' \dontrun{
# # Scenario: we want to see the effects of Stilly having RecruitScale Factor
# values of 0.5, 1, 1.5, ..., 5.
#' fram_db <- connect_fram_db(here("Valid2024_sens_test.mdb"))
#'
#' tamm_template <- here("Coho2513NOF-165.xlsx")
#' tamm_target_folder <- here("sens_test_exact/")
#' fram_db |>
#'   sensitivity_exact(
#'     template_run = 28,
#'     table_name = "StockRecruit",
#'     match_df = data.frame(match_StockID = 1:2),
#'     exact_values = seq(0.5, 5, by = 0.5),
#'     cols_to_vary = c("RecruitScaleFactor"),
#'     tamm_template = tamm_template,
#'     tamm_target_folder = tamm_target_folder,
#'     label = "Stilly sensitivity exact"
#'   )
#' disconnect_fram_db(fram_db)
#' }
sensitivity_exact <- function(fram_db,
                              template_run,
                              table_name,
                              match_df,
                              exact_values,
                              cols_to_vary,
                              tamm_template = NULL,
                              tamm_target_folder = NULL,
                              label = "sensitivity",
                              save_log = TRUE) {
  ## error checking:
  validate_fram_db(fram_db)
  ## check that template_run exists in db
  validate_run_id(fram_db, template_run)
  ## check table name
  validate_table(fram_db, table_name)
  ## check that match_df colnames match names in table
  colnames_provided <- names(match_df) |>
    stringr::str_remove("match_")
  colnames_available <- fetch_table_colnames(fram_db, table_name)

  if (!all(colnames_provided %in% colnames_available)) {
    cli::cli_abort(c("Column names of `match_df` must match columns of table `table_name`!",
      "The following do not match: {setdiff(colnames_provided, colnames_available)}.",
      "i" = "Did you use framrsquared style naming (snake_case) instead of FRAM db style naming (CamelCase)?",
      "i" = "Did you identify the right table with `table_name`?"
    ))
  }
  ## check exact_values are numeric
  if (!is.numeric(exact_values)) {
    cli::cli_abort("`exact_values` must be a numeric vector!")
  }
  ## check that tamm_template exists
  if (!file.exists(tamm_template)) {
    cli::cli_abort("`tamm_template` does not exist! Make sure argument includes full file path. ({{here}} package makes it easy to convert relative to absolute filepath)")
  }
  ## check that both or neither TAMM object exists
  if (is.null(tamm_template) + is.null(tamm_target_folder) == 1) {
    cli::cli_abort("TAMM copying only works if both `tamm_template` and `tamm_target_folder` are provided; only one of those arguments is defined!")
  }


  ## relabel match_df headers as needed
  ind.nomatch <- grep("^match_", names(match_df), invert = TRUE)
  if (length(ind.nomatch) > 0) {
    names(match_df)[ind.nomatch] <- paste0("match_", names(match_df)[ind.nomatch])
  }

  ## make copies of template run
  run_ids <- fram_db |>
    copy_run(
      target_run = template_run,
      times = length(exact_values),
      label = "sensitivity"
    )

  cli::cli_alert("Generating {length(run_ids)} new runs for sensitivity analysis, with RunIDs from {min(run_ids)} to {max(run_ids)}.")

  ## if appropriate, copy TAMMs
  if (!is.null(tamm_template) & !is.null(tamm_target_folder)) {
    cli::cli_alert("Making copies of {tamm_template} into {tamm_target_folder}.")
    copy_tamms(
      tamm_name = tamm_template,
      target_folder = tamm_target_folder,
      run_id_vec = run_ids
    )
  } else {
    cli::cli_alert("No tamm specifications, skipping copying TAMMs.")
  }

  replace_df <- as.data.frame(exact_values)
  replace_df <- replace_df[, rep(1, length(cols_to_vary)), drop = FALSE]
  names(replace_df) <- glue::glue("replace_{cols_to_vary}")



  replace_df <- cbind(
    match_RunID = run_ids,
    replace_df
  )
  df_replace <- tidyr::expand_grid(replace_df, match_df)

  modified <- modify_table(fram_db, table_name, df_replace)
  cli::cli_alert_success("Successfully generated sensitivity analyses!")
  invisible(list(values_by_runs = replace_df, full_df = df_replace))
  if (save_log) {
    db_path <- dirname(fram_db$fram_db_connection@info$dbname)
    log_name <- get_unique_filename(paste0(db_path, "/sensitivity_log - ", label, ".csv"))
    readr::write_csv(replace_df, file = log_name)
    cli::cli_alert_success("Log for run: {log_name}.")
  }
}


#' Generate sensitivity analyses runs based on a list of match/replace dataframes
#'
#' For complex sensitivity analyses, it may be easiest to programmatically create a series of
#' match/replace dataframes (`?modify_table`), one for each sensitivity run. `sensitivity_custom()` uses a list of
#' these dataframes to create a series of sensitivity analyses runs. Otherwise behaves as `sensitivity_exact()` or `sensitivity_scaled()`. Saved log is a .rds file that contains `scenario_list` but with each list item named with the matching RunID.
#'
#' Current framework does not support automating creation of sensitivity analyses in which changes are being made to multiple tables for a single run.
#'
#' @inheritParams sensitivity_scaled
#' @param scenario_list List of match/replace dataframes as described in documentation of `modify_table()`. If present, list item names are assumed to identify the table to be changed.
#' @param table_name Name of FRAM table that will be modified for the sensitivity analyses. For list items that are named, be ignored in favor of item name.
#'
#' @return Invisibly returns object `scenario_list`, but with list items named with the corresponding RunID.
#' @export
#'
#' @examples
#' \dontrun{
#' ## silly quick-and-dirty example: try these
#' ## two scenarios: mark release rates of 0.05 and 0.01 for fisheries 1 and 2
#' ## for timestep 1, or flipping those. Modifications to FisheryScalers table
#' fram_db <- connect_fram_db(here("Valid2024_sens_test.mdb"))
#'
#'custom_scenarios = list(data.frame(match_FisheryID = c(1, 2),
#'                                   match_TimeStep = c(1, 1),
#'                                   replace_MarkReleaseRate = c(.05, .01)),
#'                        data.frame(match_FisheryID = c(1, 2),
#'                                   match_TimeStep = c(1, 1),
#'                                   replace_MarkReleaseRate = c(.01, .05))
#')
#'
#'tamm_template = here("Coho2513NOF-165.xlsx")
#'tamm_target_folder = here("sens_test_custom/")
#'fram_db |>
#'  sensitivity_custom(template_run = 28,
#'                     table_name = 'FisheryScalers',
#'                     scenario_list = custom_scenarios,
#'                     tamm_template = tamm_template,
#'                     tamm_target_folder = tamm_target_folder,
#'                     label = "markrelease custom")
#'disconnect_fram_db(fram_db)
#' }

sensitivity_custom <- function(fram_db,
                               template_run,
                               table_name,
                               scenario_list,
                               tamm_template = NULL,
                               tamm_target_folder = NULL,
                               label = "sensitivity",
                               save_log = TRUE) {
  ## error checking:
  validate_fram_db(fram_db)
  ## check that template_run exists in db
  validate_run_id(fram_db, template_run)
  ## check table name
  validate_table(fram_db, table_name)
  ## check that scenario_list is at least the right structure
  if (!all(purrr::map_lgl(scenario_list, is.data.frame))) {
    cli::cli_abort("`scenario_list` must contain match/replace dataframes or tibbles!")
  }

  ## check that tamm_template exists
  if (!file.exists(tamm_template)) {
    cli::cli_abort("`tamm_template` does not exist! Make sure argument includes full file path. ({{here}} package makes it easy to convert relative to absolute filepath)")
  }
  ## check that both or neither TAMM object exists
  if (is.null(tamm_template) + is.null(tamm_target_folder) == 1) {
    cli::cli_abort("TAMM copying only works if both `tamm_template` and `tamm_target_folder` are provided; only one of those arguments is defined!")
  }

  ## make copies of template run
  run_ids <- fram_db |>
    copy_run(
      target_run = template_run,
      times = length(scenario_list),
      label = "sensitivity"
    )

  cli::cli_alert("Generating {length(run_ids)} new runs for sensitivity analysis, with RunIDs from {min(run_ids)} to {max(run_ids)}.")

  ## if appropriate, copy TAMMs
  if (!is.null(tamm_template) & !is.null(tamm_target_folder)) {
    cli::cli_alert("Making copies of {tamm_template} into {tamm_target_folder}.")
    copy_tamms(
      tamm_name = tamm_template,
      target_folder = tamm_target_folder,
      run_id_vec = run_ids
    )
  } else {
    cli::cli_alert("No tamm specifications, skipping copying TAMMs.")
  }


  for (i in 1:length(scenario_list)) {
    if (is.null(names(scenario_list[i]))) {
      cur_table_name <- table_name
    } else {
      cur_table_name <- names(scenario_list[i])
    }
    cur_df_replace <- scenario_list[[i]] |>
      dplyr::mutate(match_RunID = run_ids[i])
    modified <- modify_table(fram_db, cur_table_name, cur_df_replace)
    scenario_list[[i]] <- cur_df_replace
  }
  cli::cli_alert_success("Successfully generated sensitivity analyses!")
  invisible(scenario_list)
  if (save_log) {
    db_path <- dirname(fram_db$fram_db_connection@info$dbname)
    log_name <- get_unique_filename(paste0(db_path, "/sensitivity_log - ", label, ".rds"))
    saveRDS(scenario_list, file = log_name)
    cli::cli_alert_success("Log for run: {log_name}.")
  }
}
