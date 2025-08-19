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
#' @param fram_db FRAM database object
#' @param target_run Run ID to be copied from
#' @param times Number of copies
#' @param label Label of each copy e.g. copy 1, copy 2
#' @export
#' @examples
#' \dontrun{framdb |> copy_run(target_run = 141, times = 1)}
#'
copy_run <- function(fram_db, target_run, times = 1, label = 'copy'){

  # target_run = 139
  # times = 1

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

  for(i in seq_along(1:times)) {
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
      )

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
      )

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
}





