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
  validate_framdb(fram_db)
  validate_runid(fram_db, c(to_run, from_run))
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
