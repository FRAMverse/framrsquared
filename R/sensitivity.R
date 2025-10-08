#' Generate sensitivity analyses runs
#'
#' From a template FRAM run, for a single vector of scaling factors (e.g., `c(0.5, 2)` would test halving and doubling), generate sensitivity analyses which rescale the columns specied in (`cols_to_vary`) for rows which match the conditions specified in `match_df`. Optionally creates corresponding tamms from a template TAMM, labeled to work with folder loading option in the FRAM multi-run fork.
#'
#' Dev note: update to allow match_df to NOT start with "match_" -- it's implied.
#'
#' @param fram_db Fram database
#' @param template_run Run ID of the run that should be used as a template for the sensitivity analyses.
#' @param table_name Name of FRAM table that will be modified for the sensitivity analyses
#' @param match_df dataframe that defines which rows should be modified during sensitivity analyses. To modify some values for marked and unmarked Stillaguamish stocks, we would use data.frame(match_StockID = c(17, 18)). To modify values only for Stillaguamish age 2s, we would use expand_grid(match_StockID = c(17, 19), match_Age = 2). Dev Note: Will update so that we can just use expand_grid(StockID = c(17, 19), Age = 2)
#' @param scale_values Numeric vector of the scaling factors to be be used, one per sensitivity analysis run. Defines the number of runs generated. For example, `scale_values = 2:10` would generate 9 runs. The first would multiply the values of interest by 2, the second by 3, etc.
#' @param cols_to_vary Character or character vector of column names of FRAM table `table_name` that should be rescaled.
#' @param tamm_template Optional; character string of filepath of a TAMM to be used as a template. If provided (and tamm_target_folder provided), `sensitivity_scaled` will make a tamm for each sensitivity analysis run, using names that work with the FRAM multirun fork "Use folder" option.
#' @param tamm_target_folder Folder to copy TAMMs into. Will create if it does not exist.
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
#' fram_db = connect_fram_db(here("Valid2024_sens_test.mdb"))
#'
#' tamm_template = "ChinValidrunTest.xlsx"
#' tamm_target_folder = here("sens_test/")
#' fram_db |>
#'   sensitivity_scaled(template_run = 28,
#'                      table_name = 'StockRecruit',
#'                      match_df = data.frame(match_StockID = 1:2),
#'                      scale_values = seq(0.1, 2, length = 10),
#'                      cols_to_vary = c("RecruitScaleFactor", "RecruitCohortSize"),
#'                      tamm_template = tamm_template,
#'                      tamm_target_folder = tamm_target_folder)
#' disconnect_fram_db(fram_db)
#' }

sensitivity_scaled <- function(fram_db,
                               template_run,
                               table_name,
                               match_df,
                               scale_values,
                               cols_to_vary,
                               tamm_template = NULL,
                               tamm_target_folder = NULL){

  ## make copies to start with

  run_ids = fram_db |>
    copy_run(target_run = template_run,
             times = length(scale_values),
             label = "sensitivity")

  cli::cli_alert("Generating {length(run_ids)} new runs for sensitivity analysis, with RunIDs from {min(run_ids)} to {max(run_ids)}.")

  if(!is.null(tamm_template) & !is.null(tamm_target_folder)){
    cli::cli_alert("Making copies of {tamm_template} into {tamm_target_folder}.")
    copy_tamms(tamm_name = tamm_template,
               target_folder = tamm_target_folder,
               run_id_vec = run_ids)
  } else {
    cli::cli_alert("No tamm specifications, skipping copying TAMMs.")
  }

  scale_df = as.data.frame(scale_values)
  scale_df = scale_df[, rep(1, length(cols_to_vary))]
  names(scale_df) = glue::glue("scale_{cols_to_vary}")


  scale_df = cbind(match_RunID = run_ids,
                   scale_df)
  df = tidyr::expand_grid(scale_df, match_df)
  df_replace <-  fram_db |>
    calc_fram_scaling(table_name = table_name,
                      df = df)
  modified <- modify_table(fram_db, table_name, df_replace)
  cli::cli_alert_success("Successfully generated sensitivity analyses!")
  invisible(list(scales_by_runs = scale_df, full_df = df))
}

