#'  `r lifecycle::badge("experimental")`
#' Modify FRAM database based on match/replace dataframe
#'
#' Uses a special match/replace dataframe to modify values in a FRAM table.
#'
#' At a high level, modifying a FRAM table requires identify which rows to change, and then replacing the values of one or more of the columns of those row with new values. We often want to make multiple changes at once, and `modify_table` is written around using a dataframe to define the matching and replacing, so that it is relatively easy to check all of the changes being made. This dataframe (hereafter the "match/replace dataframe") should have column names starting with "match_" and "replace_", and ending with the exact match of column names in the FRAM table identified with argument `table_name`. For each row of argument `df`, `modify_table()` will use columns starting with "match_" as conditions to identify rows in the FRAM database to modify, and then for those rows will replace the values of columns identified with "replace_" with the corresponding values in the `df` columns.
#'
#' As a simple example, imagine we want to see how modifying the size limits for Area 7 Sport (chinook fishery id 36) affect our ERs. We would probably start by using copy_run to create multiple duplicate runs, and then we can use `modify_table` to change just the `MinimumSize` values of the "SizeLimits" table for just those rows for which fishery id was 36. If our run ids were 100, 101, and 102, and we wanted to look at minimum sizes of 450, 550, and 650, our `df` argument might look like `data.frame(match_RunID = c(100, 101, 102), match_FisheryID = c(36, 36, 36), replace_MinimumSize = c(450, 550, 650))`. Notably, we might create `df` programmatically to combine different run ids with multiple changes at once or to apply some kind of randomized parameter sampling scheme. Or we could even use an excel sheet to write out the experiment in a `df` format and then read in the sheet and feed it into `modify_table`.
#'
#' @param fram_db FRAM database
#' @param table_name Name of FRAM table
#' @param df The match/replace dataframe or tibble with specially named columns. Columns must start with either "match_" or "replace_", and should otherwise match the names of columns in `table`. For example, modifications to the Cohort table might be achieved with columns "match_RunID", "match_StockID", "match_age", "match_TimeStep", "replace_StartCohort". See Details.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df_total <- tibble(match_Age = c(3, 4, 5))
#' df_total$match_StockID <- 100
#' df_total$match_RunID <- 396
#' df_total$replace_RecruitScaleFactor <- 1:3
#' df_total$replace_RecruitCohortSize <- 100:102
#' fram_db |> modify_db(table_name = "StockRecruit", df = df_total)
#' }
modify_table <- function(fram_db, table_name, df) {
  ## check format of names:
  if (length(grep("^match_.*|^replace_.*", names(df), invert = TRUE)) > 0) {
    cli::cli_abort("`df` must have named columns starting with 'match_' or 'replace_'")
  }

  ## get column names
  table_columns = fetch_table_colnames(fram_db, table_name)

  match_names <- grep("^match_", names(df), value = TRUE)
  match_names <- gsub("^match_", "", match_names)

  replace_names <- grep("^replace_", names(df), value = TRUE)
  replace_names <- gsub("^replace_", "", replace_names)

  ## error checking:
  ##  match_names and replace_names should have no overlap
  if (length(intersect(match_names, replace_names)) > 1) {
    cli::cli_abort("'replace_' and 'match_' column types in `df` must be unique. One or more variable is assigned to both matching and replacing!")
  }

  if (length(match_names) == 0 | length(replace_names) == 0) {
    cli::cli_abort("`df` must have both 'match_' and 'replace_' columns!")
  }

  if (any(!c(match_names, replace_names) %in% table_columns)) {
    cli::cli_abort("`df` points to columns that are not in in `table_name`!")
  }

  glue_conditions <- glue::glue("{match_names} = {{match_{match_names}}}")
  glue_conditions <- paste0(glue_conditions, collapse = " AND ")

  glue_changes <- glue::glue("{replace_names} = {{replace_{replace_names}}}")
  glue_changes <- paste0(glue_changes, collapse = ", ")

  glue_statement <- glue::glue("UPDATE {{table_name}}
                            Set {glue_changes}
                            Where {glue_conditions};
                            ")
  results <- df |>
    dplyr::rowwise() |>
    dplyr::mutate(db_call = glue::glue(glue_statement)) |>
    dplyr::mutate(rows_affected = DBI::dbExecute(fram_db$fram_db_connection,
                                                 statement = .data$db_call
    )) |>
    dplyr::mutate(db_call = as.list(.data$db_call))
  return(results)
}


#'  `r lifecycle::badge("experimental")`
#' Calculate match/replace df based on scaling
#'
#' Uses a match/replace-style table like in `modify_table()`, but allows user to specify
#' scaling factors for individual columns rather than absolute values, and returns
#' the corresponding match/replace df to be used in `modify_table()`. This is intended to support
#' sensitivity analyses structured as "carry out 100 runs, with stock recruit scalers for stock X
#' running from 5% to 500% of the current value" (`calc_fram_scaling()` is only one part
#' of the pipeline for this). See `modify_table()` for details of setting up a match/replace dataframe;
#' the only difference here is that the columns to be scaled should start with "scale_" instead
#' of "replace_", and should contain the scalers.
#'
#' Note: In the StockRecruit table, RecruitScaleFactor and RecruitCohortSize should have a fixed relationship (scale factor = cohort size / base period size). For this reason, if applying scaling to only one of those columns, `calc_fram_scaling` will automatically apply the same scaling to the other. If the scaling for both is provided and they do not match, `calc_fram_scaling` will error out.
#'
#' @param fram_db FRAM database
#' @param table_name name of FRAM table
#' @param df As the match/replace dataframe of `modify_table`, but with "scale_" columns instead of "replace_" columns. Columns must start with either "match_" or "scale_", and should otherwise match the names of columns in `table`. Columns starting with "scale_" define the scaling factor to be applied to values in that column (for rows matched with the "match_" columns). For example, scaling the StartCohort values to 50% in the Cohort table might be achieved with columns "match_RunID", "match_StockID", "match_age", "match_TimeStep", "scale_StartCohort", with values of 0.5 in scale_Startcohort.
#'
#' @return A match/replace df for use in `modify_table()`, with "replace_" values
#' generated by scaling the corresponding values in the FRAM database. Includes additional "match_"
#' columns for all columns except "PrimaryKey"
#' @export
#'
#' @examples
#' \dontrun{
#' ## in run 31, decrease stock 1's recruit numbers by 50% and double 2's recruit numbers
#' library(here)
#' fram_db <- connect_fram_db(here("example_fram_db.mdb"))
#'
#' df <- data.frame(match_RunID = c(31, 31), match_StockID = 1:2, scale_RecruitScaleFactor = c(.5, 2))
#'
#' df_scaled <- calc_fram_scaling(fram_db, "StockRecruit", df)
#' ## here's what the values become:
#' df_scaled
#'
#' ## we can then modify the database
#' modified <- modify_table(fram_db, "StockRecruit", df_scaled)
#'
#' disconnect_fram_db(fram_db)
#' }
calc_fram_scaling <- function(fram_db, table_name, df) {
  ## Complication: input and output

  tab <- fram_db |>
    fetch_table(table_name)
  db_names <- DBI::dbGetQuery(
    fram_db$fram_db_connection,
    glue::glue("SELECT * FROM {table_name} where false;")
  ) |>
    colnames()
  names(tab) <- db_names

  ## finding scale and match column names
  scale_names <- names(df) |>
    stringr::str_subset("^scale_") |>
    stringr::str_remove("^scale_")

  match_names <- names(df) |>
    stringr::str_subset("^match_") |>
    stringr::str_remove("^match_")

  ## safety check specifically for StockRecruit
  terms_included <- intersect(
    c("RecruitScaleFactor", "RecruitCohortSize"),
    scale_names
  )
  terms_excluded <- setdiff(c("RecruitScaleFactor", "RecruitCohortSize"), scale_names)

  if (length(terms_included) == 1) {
    cli::cli_alert_warning("Adding scaling for {terms_excluded} equal to {terms_included}.")
    df <- df |>
      dplyr::mutate("scale_{terms_excluded}" := .data[[glue::glue("scale_{terms_included}")]])
    scale_names <- c(scale_names, terms_excluded)
  }

  if (length(terms_included == 1)) {
    if (!all(df$scale_RecruitCohortSize ==
             df$scale_RecruitScaleFactor)) {
      cli::cli_abort("scale_RecruitCohortSize and scale_RecruitScaleFactor must match!")
    }
  }

  ## creating mergeable version of df
  df_merge <- df
  ind.match <- grep("^match_", names(df))
  names(df_merge)[ind.match] <- match_names
  ind.scale <- grep("^scale_", names(df))
  names(df_merge)[ind.scale] <- glue::glue(".{scale_names}")

  ## creating modified version of df; this will be our final result
  df_mod <- dplyr::left_join(df_merge, tab, by = match_names) |>
    dplyr::select(-.data$PrimaryKey)

  for (cur_scale in scale_names) {
    df_mod[[cur_scale]] <- df_mod[[cur_scale]] *
      df_mod[[paste0(".", cur_scale)]]
  }

  df_mod <- df_mod |>
    dplyr::select(-dplyr::starts_with("."))

  df_mod <- df_mod |>
    dplyr::rename_with(~ paste0("match_", .),
                       .cols = !dplyr::any_of(scale_names)
    ) |>
    dplyr::rename_with(~ paste0("replace_", .),
                       .cols = dplyr::any_of(scale_names)
    )

  ## Shoudn't end up with NAs, but if something goes wrong with joins, want it to be obvious
  na_check <- df_mod |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), is.na))

  if (nrow(na_check) > 0) {
    cli::cli_warn("{nrow(na_check)} rows of results contain NAs. This is unexpected! Examine carefully.")
  }

  return(df_mod)
}


#' Changes a run's ID number in a FRAM database
#' @param fram_db FRAM database object
#' @param old_run_id FRAM run ID to be changed
#' @param new_run_id New FRAM run ID
#' @export
#' @examples
#' \dontrun{fram_db |> change_run_id(old_run_id = 132, new_run_id = 300)}
#'
change_run_id <- function(fram_db, old_run_id, new_run_id){

  validate_fram_db(fram_db)
  validate_run_id(fram_db, old_run_id)

  if(fram_db$fram_read_only){
    cli::cli_abort('This database connection is designated read-only!! If you are certain this database can be modified, create a new connection using `connect_fram_db()` with `read_only = TRUE`')
  }

  run_id_tables <- find_tables_by_column_(fram_db, 'RunID')

  run_id_tables$value |>
    purrr::walk(.f = \(value) tryCatch(
      suppressWarnings(DBI::dbSendQuery(
        fram_db$fram_db_connection,
        glue::glue(
          'UPDATE {value}
           SET RunID = {new_run_id}
           WHERE RunID = {old_run_id};'
        )
      )),
      error = function(e) {} # dead end
    ))

}




#' Removes a run in a FRAM database
#' @param fram_db FRAM database object
#' @param run_id FRAM run ID to be deleted
#' @export
#' @examples
#' \dontrun{fram_db |> delete_run(run_id = 132)}
#'
remove_run <- function(fram_db, run_id){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)

  if(fram_db$fram_read_only){
    cli::cli_abort('This database connection is designated read-only!! If you are certain this database can be modified, create a new connection using `connect_fram_db()` with `read_only = TRUE`')
  }

  run_id_tables <- find_tables_by_column_(fram_db, 'RunID')

  run_id_tables$value |>
    purrr::walk(.f = \(value) tryCatch(
      suppressWarnings(DBI::dbSendQuery(
        fram_db$fram_db_connection,
        glue::glue(
          'DELETE FROM {value}
           WHERE RunID = {run_id};'
        )
      )),
      error = function(e) {} # dead end
    ))

}


#' Experimental copying scaler inputs from
#' one run to another DANGEROUS
#' @param fram_db FRAM database object
#' @param from_run Run ID to be copied from
#' @param to_run Run ID to be copied to
#' @param fishery_id ID or IDs for specific fishery(s) to copy inputs to/from. If not provided, interactive option to copy inputs for all fisheries.
#' @export
#' @examples
#' \dontrun{framdb |> copy_fishery_scalers(132, 133, 87)}
#'
copy_fishery_scalers <- function(fram_db, from_run, to_run, fishery_id = NULL){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, c(to_run, from_run))

  if(!is.null(fishery_id)){
    validate_fishery_ids(fram_db, fishery_id)
  }


  if(fram_db$fram_read_only){
    cli::cli_abort('This database connection is designated read-only!! If you are certain this database can be modified, create a new connection using `connect_fram_db()` with `read_only = TRUE`')
  }

  if (is.null(fishery_id)) {
    cli::cli_alert_warning('A fishery ID is not set, this will copy all the fishery scalers!')
    input <- tolower(readline(prompt = ('Continue? (y/n): ')))
    if (input != 'y') {
      stop('Aborting')
    }
  }

  copy_scalers <- fram_db |>
    fetch_table('FisheryScalers') |>
    dplyr::filter(.data$run_id == .env$from_run)

  if (!is.null(fishery_id)) {
    copy_scalers <-
      copy_scalers |> dplyr::filter(.data$fishery_id %in% .env$fishery_id)
  }

  updated_inputs <- copy_scalers |>
    dplyr::rowwise() |>
    dplyr::mutate(rows_affected = DBI::dbExecute(
      fram_db$fram_db_connection,
      glue::glue(
        'UPDATE FisheryScalers
                          SET FisheryFlag = {fishery_flag},
                              FisheryScaleFactor = {fishery_scale_factor},
                              Quota = {quota},
                              MSFFisheryScaleFactor = {msf_fishery_scale_factor},
                              MSFQuota = {msf_quota},
                              MarkReleaseRate = {mark_release_rate},
                              MarkMisIDRate = {mark_mis_id_rate},
                              UnMarkMisIDRate = {un_mark_mis_id_rate},
                              MarkIncidentalRate = {mark_incidental_rate}
                         WHERE RunID = {.env$to_run} AND
                               TimeStep = {time_step} AND
                               FisheryID = {fishery_id};'
      )
    ))

  original_notes <- fram_db |>
    fetch_table('RunID') |>
    dplyr::filter(.data$run_id == .env$to_run) |>
    dplyr::pull(.data$run_comments)
  update_notes <- paste0(original_notes,
                         "\n\n FISHERY SCALERS COPIED PROGRAMMATICALLY FROM RUN ",
                         from_run, " for ",
                         ifelse(is.null(fishery_id),
                                "ALL FISHERIES",
                                paste0("FISHERIES ", paste0(fishery_id, collapse =", "))),
                         " ON ", round(Sys.time()), "\n"
  )
  DBI::dbExecute(fram_db$fram_db_connection,
                 glue::glue_sql(
                   'UPDATE RunID
                                SET RunComments = {update_notes}
                                WHERE RunID = {to_run};',
                   .con = fram_db$fram_db_connection
                 )
  )
  if (nrow(updated_inputs |> dplyr::filter(.data$rows_affected == 0)) > 0) {
    cli::cli_alert_warning('Some rows were not changed.')
    updated_inputs |> dplyr::filter(.data$rows_affected  == 0)
  } else if (nrow(updated_inputs |> dplyr::filter(.data$rows_affected > 1 )) > 0) {
    cli::cli_alert_danger('Multiple rows were effected by one query... DON\'T USE')
    updated_inputs |> dplyr::filter(.data$rows_affected > 1)
  } else {
    rows <- updated_inputs |> dplyr::filter(.data$rows_affected == 1) |> nrow()
    cli::cli_alert_success('Successfully updated {rows} row{?s}')
  }


}

#'  `r lifecycle::badge("experimental")`
#' Copies a run a number of times
#'
#' FRAM is stored in an access database; these have hard size limits of 2GB. Chinook and Coho are expected to reach this limit with ~540 runs. This function includes a failsafe to prevent databases from exceeding 500 runs. This can be overridden with optional `force_many_runs` argument.
#'
#' @param fram_db FRAM database object
#' @param target_run Run ID to be copied from
#' @param times Number of copies
#' @param force_many_runs `copy_runs` has failsafe to keep total run number no more than 500. This is expected to be the approximate limit for .mdb file size after runs have been run. When `force_man_runs` is `TRUE`, ignore this failsafe.
#' @param verbose Show warning message about run count? Official FRAM is hard-coded to only handle databases with <= 150 runs in them. If `TRUE` (default), provides alert when updated database will exceed this.
#' @param label Label of each copy e.g. copy 1, copy 2
#'
#' @return Invisibly returns the run ids of the copied runs, for use in other functions.
#' @export
#' @examples
#' \dontrun{framdb |> copy_run(target_run = 141, times = 1)}
#'
copy_run <- function(fram_db, target_run, times = 1, label = 'copy', force_many_runs = FALSE, verbose = TRUE){

  validate_fram_db(fram_db)
  validate_run_id(fram_db, target_run)
  if(!is.numeric(times) || length(times) != 1) {
    cli::cli_abort("`times` must be a single integer")
  }

  if(!is.character(label) || length(label) != 1) {
    cli::cli_abort("`label` must be a single character string")
  }

  if(!is.logical(force_many_runs) || length(force_many_runs) != 1) {
    cli::cli_abort("`force_many_runs` must be a single logical value")
  }

  if(!is.logical(verbose) || length(verbose) != 1) {
    cli::cli_abort("`verbose` must be a single logical value")
  }

  if(fram_db$fram_read_only){
    cli::cli_abort('This database connection is designated read-only!! If you are certain this database can be modified, create a new connection using `connect_fram_db()` with `read_only = TRUE`')
  }

  run_count_current = fram_db |> fetch_table("RunID") |> nrow()
  if((run_count_current + times > 150) & verbose){
      cli::cli_alert("Official FRAM cannot currently read databases with >150 run ids.\n  Use FRAM_Automation (https://github.com/FRAMverse/FRAM_automation)\n  or change FRAM source code declaration of vectors `RunID`, `RunIDName`, and `RunBasePeriodID` in `FVS_ModelRunSelection.vb`.")
  }
  if(run_count_current + times > 500){
    if(force_many_runs){
      cli::cli_alert("FRAM databases expected to exceed .mdb memory limits at ~500 runs, currently would update database to {run_count_current + times} run. `force_many_runs` is TRUE, so overriding this failsafe.")
    } else {
      cli::cli_abort("FRAM databases expected to exceed .mdb memory limits at ~500 runs, currently would update database to {run_count_current + times} run. Aborting copy; set `force_many_runs = TRUE` to override this failsafe.")
    }
  }

  run_table <- DBI::dbReadTable(fram_db$fram_db_connection, 'RunID')

  max_run_id <- run_table |>
    dplyr::pull(.data$RunID) |>
    max()

  # collect tables
  stock_recruit <- DBI::dbGetQuery(fram_db$fram_db_connection,
                                   glue::glue(
                                     "SELECT * FROM StockRecruit
                                    WHERE RunID = {target_run}
                                    "))

  fishery_scalers <- DBI::dbGetQuery(fram_db$fram_db_connection,
                                     glue::glue(
                                       "SELECT * FROM FisheryScalers
                                    WHERE RunID = {target_run}
                                    "))

  sfrs <- DBI::dbGetQuery(fram_db$fram_db_connection,
                          glue::glue(
                            "SELECT * FROM StockFisheryRateScaler
                                    WHERE RunID = {target_run}
                                    "))

  non_retention <- DBI::dbGetQuery(fram_db$fram_db_connection,
                                   glue::glue(
                                     "SELECT * FROM NonRetention
                                    WHERE RunID = {target_run}
                                    "))

  if(fram_db$fram_db_species == 'CHINOOK') {
    size_limits <- DBI::dbGetQuery(fram_db$fram_db_connection,
                                   glue::glue(
                                     "SELECT * FROM SizeLimits
                                    WHERE RunID = {target_run}
                                    "))
  }

  run_target <- run_table |>
    dplyr::filter(.data$RunID == .env$target_run)


  cli::cli_progress_bar(total = times,
                        format = 'Copying run {i}/{times} | {cli::pb_bar} {cli::pb_percent} {cli::pb_eta}'
  )
  # store run ids
  run_id_vec = numeric(times)
  for(i in seq_along(1:times)) {

    run_id_vec[i] = max_run_id + i

    # RunID Table
    run_insert <- run_target |>
      dplyr::mutate(
        RunID = .env$max_run_id + .env$i,
        RunName = glue::glue(.data$RunName, ' {label} {i}')
      ) |>
      dplyr::select(-.data$PrimaryKey)


    # send to db
    DBI::dbAppendTable(fram_db$fram_db_connection,
                       name = 'RunID',
                       value = run_insert,
                       batch_rows = 1)


    # stock recruit table
    stock_recruit_insert <- stock_recruit |>
      dplyr::mutate(
        RunID = .env$max_run_id + .env$i
      ) |>
      dplyr::select(-.data$PrimaryKey)

    # send to db
    DBI::dbAppendTable(fram_db$fram_db_connection,
                       name = 'StockRecruit',
                       value = stock_recruit_insert,
                       batch_rows = 1)

    # fishery scalers
    fishery_scalers_insert <- fishery_scalers |>
      dplyr::mutate(
        RunID = .env$max_run_id + .env$i
      )|>
      dplyr::select(-.data$PrimaryKey)

    # send to db
    DBI::dbAppendTable(fram_db$fram_db_connection,
                       name = 'FisheryScalers',
                       value = fishery_scalers_insert,
                       batch_rows = 1)

    # stock fishery rate scalers
    if(nrow(sfrs) > 0){
      sfrs_insert <- sfrs |>
        dplyr::mutate(
          RunID = .env$max_run_id + .env$i
        )

      # send to db
      DBI::dbAppendTable(fram_db$fram_db_connection,
                         name = 'StockFisheryRateScaler',
                         value = sfrs_insert,
                         batch_rows = 1)
    }


    # non retention
    non_retention_insert <- non_retention |>
      dplyr::mutate(
        RunID = .env$max_run_id + .env$i
      )|>
      dplyr::select(-.data$PrimaryKey)

    # send to db
    DBI::dbAppendTable(fram_db$fram_db_connection,
                       name = 'NonRetention',
                       value = non_retention_insert,
                       batch_rows = 1)

    if (fram_db$fram_db_species == 'CHINOOK') {
      size_limits_insert <- size_limits |>
        dplyr::mutate(RunID = .env$max_run_id + .env$i) |>
        dplyr::select(-.data$PrimaryKey)


      # send to db
      DBI::dbAppendTable(
        fram_db$fram_db_connection,
        name = 'SizeLimits',
        value = size_limits_insert,
        batch_rows = 1
      )

    }


    cli::cli_progress_update()
  }

  cli::cli_process_done()
  invisible(run_id_vec)
}

#'  `r lifecycle::badge("experimental")`
#' Copy TAMM for FRAM batch runs
#'
#' Preps a folder for batch running in the 'Run Multiple Runs' screen of the FRAM automation fork (https://github.com/FRAMverse/FRAM_automation), for use with the `advanced` approach to identify multiple runs. One TAMM file will be copied multiple times in the `target_folder` with suffixes that identify each of the run_ids. The "Use folder" button on the "Run Multiple Runs" screen can then use the target folder to set up large batch runs. Typically users should use `make_batch_runs()` instead (this first copies runs and then uses `copy_tamms` to create tamms that match the new runs).
#'
#' @param tamm_name TAMM file to copy, including file path. Character string
#' @param target_folder directory to put new batch TAMM files into. Character string
#' @param run_id_vec vector of run_ids (numeric or character), corresponding to run ids in a FRAM database.
#' @param overwrite If one or more files already exist in `target_folder` with names matching the combination of `tamm_name` and run ids, overwrite (`TRUE`) or leave those files untouched (`FALSE`). Defaults to `FALSE` for safety; recommend setting to `TRUE` to avoid confusion when iterating on work.
#'
#' @return invisibly returns logical vector of `file.copy()` success.
#' @export
#'
#' @examples
#' \dontrun{copy_tamms(tamm_name = "C:/TAMMs/Chin2020.xlsx",
#' target_folder = "C:/Batch_run_5", run_id_vec = 10:20)}

copy_tamms <- function(tamm_name, target_folder, run_id_vec, overwrite = FALSE){
  if(!is.numeric(run_id_vec) & all(!is.na(as.numeric(run_id_vec)))){
    cli::cli_abort("argument `run_id_vec` must be either integers or character strings of integers.")
  }
  ## does file_name exist
  if(!file.exists(tamm_name)){
    cli::cli_abort("File `tamm_name` must exist!")
  }
  ## is file_name a legal TAMM?
  if(!tools::file_ext(tamm_name) %in% c("xlsx", "xls", "xlsm")){
    cli::cli_abort("`tamm_name` must be a TAMM file (ending in `.xlsx`, `.xls`, or `.xlsm`)!")
  }
  ## If dir does not exist, create.
  if(!dir.exists(target_folder)){
    creation_successful <- dir.create(target_folder)
    if(!creation_successful){
      cli::cli_abort("Directory `target_folder` does not exist, and `copy_tamms()` was unable to create it! Parent directory might not exist?")
    }
  }

  file_extension <- glue::glue(".{tools::file_ext(tamm_name)}")
  file_name_clean <- gsub(".*[/]", "", tamm_name)
  file_name_clean <- gsub(glue::glue("{file_extension}$"), "", file_name_clean)

  ## copy file_name to target_path multiple times with unique suffixes -{run_id}
  new_files = glue::glue("{target_folder}/{file_name_clean}-{run_id_vec}{file_extension}")
  if(any(file.exists(new_files))){
    cli::cli_alert("One or more of the new TAMM files already exists in target folder!")
    if(overwrite){
      cli::cli_alert("`overwrite` set to `TRUE`! Overwriting existing TAMM files in target folder as needed.")
    } else {
      cli::cli_alert("`overwrite` set to `FALSE`! Any missing TAMM files in target folder will be added, existing files will be untouched.")
    }
  }
  invisible(file.copy(tamm_name, new_files, overwrite = overwrite))
}


make_batch_runs <- function(fram_db, target_run, times = 1, label = 'copy', tamm_name, target_folder, force_many_runs = FALSE, verbose = TRUE){
  new_run_ids <- fram_db |>
    copy_run(target_run = target_run, times = times, label = label, force_many_runs, verbose)
  copy_tamms(tamm_name, target_folder, run_id_vec = new_run_ids, overwrite = TRUE)
  cli::cli_alert_success(glue::glue("Batch runs ready! New Run Ids range from {min(new_run_ids)} to {max(new_run_ids)}.\nTAMMs are in {target_folder}."))
}
