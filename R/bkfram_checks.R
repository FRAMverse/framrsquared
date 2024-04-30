#' Performs error checks of a backwards FRAM run
#' Returns nested tibble with diagnostics
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
bkfram_checks_coho <-
  function(fram_db,
           backward_run_id = NULL,
           forward_run_id = NULL) {

    validate_fram_db(fram_db, db_type = 'full', db_species = 'COHO')
    # let user know what's going on
    cli::cli_alert_info('These are a suite of checks to find errors a post-season (bkFRAM) run.')


    # check for null ids
    if (is.null(backward_run_id) | is.null(forward_run_id)) {
      cli::cli_abort("Both a backward and forward run ids must be supplied")
    }

    validate_run_id(fram_db, backward_run_id)
    validate_run_id(fram_db, forward_run_id)

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
      fetch_table('Mortality')
    cli::cli_alert_success('Imported Mortality Table')

    # pull out stocks for lookups
    stocks <- fram_db |>
      fetch_table('Stock') |>
      dplyr::filter(.data$species == 'COHO') |>
      dplyr::select(.data$stock_id, .data$stock_name)
    cli::cli_alert_success('Imported Stock Look-up Table')

    # pull out fisheries for lookups
    fisheries <- fram_db |>
      fetch_table('Fishery') |>
      dplyr::filter(.data$species == 'COHO') |>
      dplyr::select(.data$fishery_id, .data$fishery_name)
    cli::cli_alert_success('Imported Fishery Look-up Table')

    # save run names to refer to them in output
    bk_run_name <- run |>
      dplyr::filter(.data$run_id == .env$backward_run_id) |>
      dplyr::pull(.data$run_name)

    fwd_run_name <- run |>
      dplyr::filter(.data$run_id == .env$forward_run_id) |>
      dplyr::pull(.data$run_name)

    # bk_run_name <- 'test1'
    # fwd_run_name <- 'test2'

    # split out into separate dataframes
    # backward
    bk_fishery_scalers <- fishery_scaler |>
      dplyr::filter(.data$run_id == .env$backward_run_id)

    bk_escapement <- escapement |>
      dplyr::filter(.data$run_id == .env$backward_run_id)

    bk_target_escapement <- target_escapement |>
      dplyr::filter(.data$run_id == .env$backward_run_id)

    bk_mortality <- mortality |>
      dplyr::filter(.data$run_id == .env$backward_run_id)

    # forward
    fwd_fishery_scalers <- fishery_scaler |>
      dplyr::filter(.data$run_id == .env$forward_run_id)

    fwd_escapement <- escapement |>
      dplyr::filter(.data$run_id == .env$forward_run_id)

    fwd_target_escapement <- target_escapement |>
      dplyr::filter(.data$run_id == .env$forward_run_id)

    fwd_mortality <- mortality |>
      dplyr::filter(.data$run_id == .env$forward_run_id)

    # checks
    cli::cli_h1('Running checks')

    # checking flags ----------------------------------------------------------

    # flag checks
    cli::cli_h2('Checking all scaler flags are set to quotas')


    bk_bad_flags <- bk_fishery_scalers |>
      dplyr::filter(
        !.data$fishery_flag %in% c(2, 8, 28) &
          # check for influential scalers
          (
            .data$fishery_scale_factor != 0 |
              .data$msf_fishery_scale_factor != 0
          )
      ) |>
      dplyr::select(
        .data$fishery_id,
        .data$time_step,
        .data$fishery_flag,
        .data$msf_fishery_scale_factor,
        .data$fishery_scale_factor
      )


    fwd_bad_flags <- fwd_fishery_scalers |>
      dplyr::filter(
        .data$run_id == .env$forward_run_id,!.data$fishery_flag %in% c(2, 8, 28) &
          # check for influential scalers
          (
            .data$fishery_scale_factor != 0 |
              .data$msf_fishery_scale_factor != 0
          )
      ) |>
      dplyr::select(
        .data$fishery_id,
        .data$time_step,
        .data$fishery_flag,
        .data$msf_fishery_scale_factor,
        .data$fishery_scale_factor
      )

    if (nrow(bk_bad_flags) == 0 & nrow(fwd_bad_flags) == 0) {
      cli::cli_alert_success(
        'Flags from backward run ({bk_run_name}) and forward run ({fwd_run_name}) set correctly'
      )
    } else if (nrow(bk_bad_flags) > 0) {
      cli::cli_alert_danger('Scaler flags found in the backwards run ({bk_run_name})')
      print(bk_bad_flags)
    } else if (nrow(fwd_bad_flags) > 0) {
      cli::cli_alert_danger('Scaler flags found in the forward run ({fwd_run_name})')
      print(fwd_bad_flags)
    }

    # buoy 10 check -----------------------------------------------------------
    cli::cli_h2(
      'Checking that Buoy 10 Sport is zeroed out in the backward run and present in the forward run'
    )

    bk_buoy <- bk_fishery_scalers |>
      dplyr::filter(.data$fishery_id == 23, # buoy 10 sport
                    (.data$quota > 0 | .data$msf_quota > 0)) |>
      dplyr::select(.data$run_id,
                    .data$time_step,
                    .data$fishery_flag,
                    .data$quota,
                    .data$msf_quota)

    fwd_buoy <- fwd_fishery_scalers |>
      dplyr::filter(.data$fishery_id == 23, # buoy 10 sport
                    (.data$quota > 0 | .data$msf_quota > 0)) |>
      dplyr::select(.data$run_id,
                    .data$time_step,
                    .data$fishery_flag,
                    .data$quota,
                    .data$msf_quota)

    if (nrow(bk_buoy) > 0) {
      cli::cli_alert_danger('There are quotas in the backward run ({bk_run_name}). This likely needs to be fixed.')
      print(bk_buoy)
    } else if (nrow(fwd_buoy) == 0) {
      cli::cli_alert_danger(
        'Quotas are zeroed out in the forward run ({fwd_run_name}). This likely needs to be fixed.'
      )
      print(fwd_buoy)
    } else {
      cli::cli_alert_success('Buoy 10 inputs appear to be correct')
    }


    # make sure quil and queets inputs are zeroed, should be coming fr --------
    cli::cli_h2('Checking for inputs in Quilly and Queets (should be coming from TAMM)')
    bk_qq <- bk_fishery_scalers |>
      dplyr::filter(.data$fishery_id %in% 65:72, # buoy 10 sport
                    (.data$quota > 0 | .data$msf_quota > 0)) |>
      dplyr::select(.data$run_id,
                    .data$time_step,
                    .data$fishery_flag,
                    .data$quota,
                    .data$msf_quota,
                    .data$fishery_id) |>
      dplyr::inner_join(fisheries, by = 'fishery_id')

    fwd_qq <- fwd_fishery_scalers |>
      dplyr::filter(.data$fishery_id %in% 65:72, # buoy 10 sport
                    (.data$quota > 0 | .data$msf_quota > 0)) |>
      dplyr::select(.data$run_id,
                    .data$time_step,
                    .data$fishery_flag,
                    .data$quota,
                    .data$msf_quota,
                    .data$fishery_id) |>
      dplyr::inner_join(fisheries, by = 'fishery_id')

    if (nrow(bk_qq) > 0) {
      cli::cli_alert_danger('There are quotas in the backward run ({bk_run_name}). This likely needs to be fixed.')
      print(bk_qq)
    } else {
      cli::cli_alert_success('Quilly and Queets inputs set to zero in backward run')
    }

    if (nrow(fwd_qq) > 0) {
      cli::cli_alert_success('Quilly and Queets inputs set in forward run')
    } else {
      cli::cli_alert_warning('Quilly and Queets quotas not set in the forward run ({fwd_run_name}).')
      print(fwd_qq)
    }

    # differences in flagging between runs ------------------------------------
    cli::cli_h2('Checking for differences in flagging between backward and forward runs')

    bk_flags <- bk_fishery_scalers |>
      dplyr::select(.data$fishery_id, .data$time_step, .data$fishery_flag)

    fwd_flags <- fwd_fishery_scalers |>
      dplyr::select(.data$fishery_id, .data$time_step, .data$fishery_flag)

    mismatch_flags <- bk_flags |>
      dplyr::inner_join(
        fwd_flags,
        by = c('fishery_id', 'time_step'),
        suffix = c('_bk', '_fwd')
      ) |>
      dplyr::filter(.data$fishery_flag_bk != .data$fishery_flag_fwd) |>
      dplyr::inner_join(fisheries, by = 'fishery_id')

    if (nrow(mismatch_flags) == 0) {
      cli::cli_alert_success(
        'Fishery flags match between the backward run ({bk_run_name}) and forward run ({fwd_run_name})'
      )
    } else {
      cli::cli_alert_danger('Mismatch of fishery flags below is a list')
      print(mismatch_flags)
    }



    # differences in fishery quotas ------------------------------------------
    cli::cli_h2('Checking for differences in fishery quotas between backward and forward runs')

    bk_flags <- bk_fishery_scalers |>
      dplyr::mutate(total_quota = .data$quota + .data$msf_quota) |>
      dplyr::select(.data$fishery_id, .data$time_step, .data$total_quota)

    fwd_flags <- fwd_fishery_scalers |>
      dplyr::mutate(total_quota = .data$quota + .data$msf_quota) |>
      dplyr::select(.data$fishery_id, .data$time_step, .data$total_quota)

    mismatch_quotas <- bk_flags |>
      dplyr::inner_join(
        fwd_flags,
        by = c('fishery_id', 'time_step'),
        suffix = c('_bk', '_fwd')
      ) |>
      dplyr::filter(.data$total_quota_bk != .data$total_quota_fwd,
                    !.data$fishery_id %in% c(
                      23, # buoy 10 sport
                      65:72 # queets and quilly
                    )
                    ) |>
      dplyr::mutate(difference = abs(.data$total_quota_bk - .data$total_quota_fwd)) |>
      dplyr::arrange(-.data$difference) |>
      dplyr::inner_join(fisheries, by = 'fishery_id')

    if (nrow(mismatch_quotas) == 0) {
      cli::cli_alert_success(
        'Fishery quotas match between the backward run ({bk_run_name}) and forward run ({fwd_run_name})'
      )
    } else {
      cli::cli_alert_warning('Mismatch of fishery quotas below is a list, coastal iterations can cause this')
      print(mismatch_quotas)
    }



    # quilly / queets, ETRS vs target -----------------------------------------
    cli::cli_h2('Checking ETRS Stocks are similar to target escapements')

    #define etrs stocks
    etrs_stocks <- c(
      # quilly
      'U-quilsw',
      'M-quilsw',
      'U-quilsh',
      'M-quilsh',
      'U-quilfw',
      'M-quilfw',
      'U-quilfh',
      'M-quilfh',
      # queets
      'U-quetfw',
      'M-quetfw',
      'U-quetfh',
      'M-quetfh',
      'U-quetph',
      'M-quetph'
    )

    # pull out targets
    etrs_targets <- bk_target_escapement |>
      dplyr::inner_join(stocks, by = 'stock_id') |>
      dplyr::filter(.data$stock_name %in% etrs_stocks) |>
      dplyr::select(.data$stock_id,
                    .data$stock_name,
                    .data$target_esc_age3,
                    .data$target_flag)

    # calculate extreme terminal runsizes
    etrs_escapments <- fwd_escapement |>
      dplyr::inner_join(stocks, by = 'stock_id') |>
      dplyr::filter(.data$stock_name  %in% etrs_stocks) |>
      dplyr::select(.data$stock_id, .data$stock_name, .data$escapement)

    # this chunk of code can be made more flexible,
    # if stocks are ever looked at with ETRS update the
    # fisheries
    etrs_mortality <- fwd_mortality |>
      dplyr::inner_join(stocks, by = 'stock_id') |>
      dplyr::filter(
        .data$stock_name %in% etrs_stocks,
        .data$time_step %in% c(4:5),
        .data$fishery_id %in% c(65:72)
      ) |> # all in river
      dplyr::mutate(
        all_mortality = .data$landed_catch + .data$shaker + .data$non_retention + .data$drop_off +
          .data$msf_landed_catch + .data$msf_shaker + .data$msf_non_retention + .data$msf_drop_off
      ) |>
      dplyr::group_by(.data$stock_id, .data$stock_name) |>
      dplyr::summarize(etrs_mort = sum(.data$all_mortality),
                       .groups = 'drop')

    cli::cli_alert_warning('When reading this table remember to use escapement flags correctly')

    etrs <- etrs_escapments |>
      dplyr::inner_join(etrs_mortality, by = c('stock_id', 'stock_name')) |>
      dplyr::mutate(etrs = .data$escapement + .data$etrs_mort) |>
      dplyr::left_join(etrs_targets, by = c('stock_id', 'stock_name'))

    print(etrs)


    # check coastal iterations ------------------------------------------------
    # coastal iterations should be turned off

    cli::cli_h2('Checking that coastal iterations are off')
    bk_run_coastal <- run |>
      dplyr::filter(.data$run_id == .env$backward_run_id) |>
      dplyr::pull(.data$coastal_iterations)

    fwd_run_coastal <- run |>
      dplyr::filter(.data$run_id == .env$forward_run_id) |>
      dplyr::pull(.data$coastal_iterations)

    if(bk_run_coastal == 'Yes'){
      cli::cli_alert_danger('Coastal Iterations for the backward run ({bk_run_name}) is on')
    }
    if(fwd_run_coastal == 'Yes'){
      cli::cli_alert_danger('Coastal Iterations for the forward run ({fwd_run_name}) is on')
    }

    if (bk_run_coastal == 'No' & fwd_run_coastal == 'No') {
      cli::cli_alert_success('Coastal Iterations is turned off for both backward and forward runs')
    }


    # differences between target and bkfram escapement ------------------------
    cli::cli_h2('Checking backwards FRAM escapements vs targets')

    bk_esc_ck <- bk_esc <- bk_escapement |>
      dplyr::select(.data$stock_id, bk_escapement = .data$escapement)

    bk_esc_target_ck <- bk_target_escapement |>
      dplyr::select(.data$stock_id,
                    target_escapement = .data$target_esc_age3,
                    .data$target_flag)


    cli::cli_alert_info(
      'Top 10 stocks with largests differences between target escapement and bkFRAM escapement'
    )
    cli::cli_alert_warning('Note: these are filtered to flag 1, for the full dataset see returned tibble')
    bk_esc_ck |>
      dplyr::inner_join(bk_esc_target_ck, by = 'stock_id') |>
      dplyr::mutate(difference = abs(.data$bk_escapement - .data$target_escapement)) |>
      dplyr::filter(.data$target_flag == 1) |>
      dplyr::arrange(-.data$difference) |>
      dplyr::inner_join(stocks, by = 'stock_id') |>
      print(n = 10)

    bk_esc_export <- bk_esc_ck |>
      dplyr::inner_join(bk_esc_target_ck, by = 'stock_id') |>
      dplyr::mutate(difference = abs(.data$bk_escapement - .data$target_escapement)) |>
      dplyr::arrange(-.data$difference) |>
      dplyr::inner_join(stocks, by = 'stock_id')


    # return a nested dataframe -----------------------------------------------
    tibble::tibble(
      check = c(
        'scaler flags backward',
        'scaler flags forward',
        'buoy 10 backward',
        'buoy 10 forward',
        'qully and queets ETRS escapement target',
        'target vs bkfram escapement',
        'flagging differences',
        'quota differences',
        'queets/quilly inputs forward',
        'queets/quilly inputs backward',
        'coastal iterations backward',
        'coastal iterations forward'
      ),
      type = c(
        'flag',
        'flag',
        'quota',
        'quota',
        'escapement',
        'escapement',
        'flag',
        'quota',
        'quota',
        'quota',
        'coastal iterations',
        'coastal iterations'
      ),
      data = c(
        list(bk_bad_flags),
        list(fwd_bad_flags),
        list(bk_buoy),
        list(fwd_buoy),
        list(etrs),
        list(bk_esc_export),
        list(mismatch_flags),
        list(mismatch_quotas),
        list(bk_qq),
        list(fwd_qq),
        bk_run_coastal,
        fwd_run_coastal
      )
    )
  }
