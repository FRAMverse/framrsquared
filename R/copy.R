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

  # target_run = 139
  # times = 1
  #
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
