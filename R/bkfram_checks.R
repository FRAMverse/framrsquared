#' Performs error checks of a backwards FRAM run
#' <list checks here>
#'
#' @export
#'
#' @param fram_db fram database object, supplied through connect_fram_db
#' @param backward_run_id numeric, RunID
#' @param forward_run_id numeric, RunID
#'
#' @examples
#' \dontrun{
#' fram_db |> bkfram_checks_coho(backward_run_id = 132, forward_run_id = 133)
#' }
#'
bkfram_checks_coho <- function(fram_db, backward_run_id = NULL, forward_run_id = NULL){

  # let user know what's going on
  cli::cli_alert_info('These are a suite of checks to check a post-season
                  (bkFRAM) run for errors.')


  # check for null ids
  if (is.null(backward_run_id) | is.null(forward_run_id)){
    rlang::abort("Both a backward and forward run ids must be supplied")
  }

  # make sure run ids are integers
  if (!is.numeric(backward_run_id) | !is.numeric(forward_run_id)) {
    rlang::abort("Run ID(s) must be and integers")
  }
  # run ids are called so much in this package probably worth it
  # to add a validate validate_run_id() function to integrity.R

  # send an update to the console
  cli::cli_h1('Gathering Data')

  # pull out the target escapements
  run <- fram_db |>
    fetch_table('RunID')
  cli::cli_alert_success('Imported RunID Table')

  # pull out the fishery scalers table
  fishery_scaler <- fram_db |>
    fetch_table('FisheryScalers')
  cli::cli_alert_success('Imported FisheryScalers Table')
  # pull out the escapement table
  escapement <- fram_db |>
    fetch_table('Escapement')
  cli::cli_alert_success('Imported Escapement Table')
  # pull out the target escapements
  target_escapement <- fram_db |>
    fetch_table('BackwardsFRAM')
  cli::cli_alert_success('Imported BackwardsFRAM Table')

  # pull out fishery mortality (queets quilly ETRS)
  mortality <- fram_db |>
    fetch_table('FisheryMortality')
  cli::cli_alert_success('Imported FisheryMortality Table')

  # save run names to refer to them in output
  bk_run_name <- run |>
    dplyr::filter(.data$run_id == .env$backward_run_id) |>
    dplyr::pull(.data$run_name)

  fwd_run_name <- run |>
    dplyr::filter(.data$run_id == .env$forward_run_id) |>
    dplyr::pull(.data$run_name)

  bk_run_name <- 'test1'
  fwd_run_name <- 'test2'

  # split out into separate dataframes
  # backward
  bk_fishery_scalers <- fishery_scaler |>
    dplyr::filter(.data$run_id == .env$backward_run_id)

  bk_escapement <- escapement |>
    dplyr::filter(.data$run_id == .env$backward_run_id)

  bk_target_escapement <- fram_db |>
    dplyr::filter(.data$run_id == .env$backward_run_id)

  # forward
  fwd_fishery_scalers <- fishery_scaler |>
    dplyr::filter(.data$run_id == .env$forward_run_id)

  fwd_escapement <- escapement |>
    dplyr::filter(.data$run_id == .env$forward_run_id)

  fwd_target_escapement <- fram_db |>
    dplyr::filter(.data$run_id == .env$forward_run_id)

  # checks
  cli::cli_h1('Running checks')

  # flag checks
  cli::cli_alert_info('Checking all scaler flags are set to quotas')


  bk_bad_flags <- bk_fishery_scalers |>
    dplyr::filter(
      !.data$fishery_flag %in% c(2,8,28) & # check for influential scalers
        (.data$fishery_scale_factor != 0 |
           .data$msf_fishery_scale_factor != 0)
    ) |>
    dplyr::select(.data$fishery_id, .data$time_step, .data$fishery_flag,
                  .data$msf_fishery_scale_factor, .data$fishery_scale_factor)


  fwd_bad_flags <- fwd_fishery_scaler |>
    dplyr::filter(
      .data$run_id == .env$forward_run_id,
      !.data$fishery_flag %in% c(2,8,28) & # check for influential scalers
        (.data$fishery_scale_factor != 0 |
           .data$msf_fishery_scale_factor != 0)
    ) |>
    dplyr::select(.data$fishery_id, .data$time_step, .data$fishery_flag,
                  .data$msf_fishery_scale_factor, .data$fishery_scale_factor)

  if(nrow(bk_bad_flags) > 0 | nrow(fwd_bad_flags) > 0){
    cli::cli_alert_success('Flags from backward run ({bk_run_name}) and forward run ({fwd_run_name}) set correctly')
  } else if(nrow(bk_bad_flags) > 0){
    cli::cli_alert_danger('Scaler flags found in the backwards run ({bk_run_name})')
    bk_bad_flags
  } else if(nrow(fwd_bad_flags) > 0){
    cli::cli_alert_danger('Scaler flags found in the forward run ({fwd_run_name})')
    fwd_bad_flags
  }


  # buoy 10 sport
  cli::cli_alert_info('Checking the Buoy 10 Sport is zeroed out in the backward
                      run and present in the forward run')







}
