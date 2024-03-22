#' Experimental copying scaler inputs from
#' one run to another DANGEROUS
#' @param fram_db FRAM database object
#' @param from_run Run ID to be copied from
#' @param to_run Run ID to be copied to
#' @param fishery_id Specific fishery's scalers to be copied
#' @export
#' @examples
#' \dontrun{truns <- truns_stocks(fram_db)}
#'


copy_fishery_scalers <- function(fram_db, from_run, to_run, fishery_id = NULL){
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
      copy_scalers |> dplyr::filter(.data$fishery_id == .env$fishery_id)
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
