#' Generates post-season January age 3 abundances by stock from post-season databases.
#' Used for forecasting.
#' @param fram_db FRAM database object
#' @param units Default January Age 3 'ja3', optional ocean age 3 'oa3'
#' @export
#' @examples
#' \dontrun{framdb |> post_season_abundance()}
#'

post_season_abundance <- function(fram_db, units = c('ja3', 'oa3')){

  validate_fram_db(fram_db)

  unit <- rlang::arg_match(units)

  if(fram_db$fram_db_species != "COHO"){
    cli::cli_abort('This function currently only works with coho')
  }

  # get data
  stock_recruit <- fram_db |> fetch_table('StockRecruit')
  base_cohort <- fram_db |> fetch_table('BaseCohort')
  stock <- fram_db |> fetch_table('Stock')
  run_id <- fram_db |> fetch_table('RunID') |>
    dplyr::filter(.data$run_type == 'Post') # only want post-season runs

  cohort_table <- run_id |>
    dplyr::inner_join(base_cohort,
                      by = 'base_period_id',
                      relationship = 'many-to-many') |> # making sure baseperiod is correct for run
    dplyr::inner_join(stock, by = 'stock_id') |>
    dplyr::filter(
      .data$run_year >= 2010, # don't care about earlier apparently
    ) |> # don't care about earlier apparently
    dplyr::inner_join(stock_recruit, by = c('run_id', 'stock_id'),
                      relationship = 'many-to-many') |>
    dplyr::mutate(
      recruit_cohort_size = .data$recruit_scale_factor * .data$base_cohort_size,
      origin = dplyr::case_when(
        stringr::str_detect(stock_long_name, 'Wild') |
          stringr::str_detect(stock_long_name, 'Nat') |
          stringr::str_detect(stock_long_name, '/Wild')  ~ 'Wild',
        stringr::str_detect(stock_long_name, 'Hatchery') |
          stringr::str_detect(stock_long_name, 'Net Pen')  ~ 'Hatchery',
        .default = 'Misc'

      )
    ) |> #count(origin)
    dplyr::select(.data$run_year, .data$run_id, .data$stock_id,  .data$stock_name,
                  .data$recruit_scale_factor, .data$base_cohort_size,
                  .data$recruit_cohort_size, .data$origin)


  if(unit == 'ja3'){
    cli::cli_alert_info('Abundances given in terms of January age 3')
    # summary sheet
    cohort_table |>
      dplyr::select(
        .data$stock_id,
        .data$stock_name,
        .data$run_year,
        .data$recruit_cohort_size,
        .data$origin
      ) |>
      tidyr::pivot_wider(names_from = .data$run_year,
                         values_from = .data$recruit_cohort_size)
  } else {
    cli::cli_alert_info('Abundances given in terms of ocean age 3')
    cohort_table |>
      dplyr::select(
        .data$stock_id,
        .data$stock_name,
        .data$run_year,
        .data$recruit_cohort_size,
        .data$origin
      ) |>
      # take out natural mortality if oa3
      tidyr::pivot_wider(names_from = .data$run_year,
                         values_from = .data$recruit_cohort_size) |>
      dplyr::mutate(
        dplyr::across(
          -c(.data$stock_id, .data$stock_name, .data$origin)
          , \(x) x / 1.2317)
      )
  }

}

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



#' Create table comparing mortalities
#'
#' Tool for investigating causes of ER overages identified in PSC reports (e.g., the Annual Report). For a given stock in a given year (typically one that experienced an ER overage), compares the predicted mortalities in each fishery in the preseason with the estimated mortalities in those fisheries in the postseason. Also provides the context of the preseason and postseason estimated mortalities of other stocks; for mixed stock fisheries, mismatches between preseason and postseason abundances of other stock can lead to ER overages of a focal stock in FRAM even if modeling of the focal stock itself was perfect.
#'
#' Intended as helper function for `present_mort_comparison()`, which provides a formatted `gt` table based on the output of this function. However, `create_mort_comparison()` will be useful for providing results in dataframes for further processing or plotting.
#'
#' @param con.pre Connection to a Coho preseason database, presumably the complete one managed by the PSC. Must contain a run for the `year` year.
#' @param con.post Connection to a Coho postseason database, presumably the complete one managed by the PSC. Must contain a run for the `year` year.
#' @param year.focal Year to compare
#' @param stock.focal PSC stock to compare (`unique(framrosetta::stock_coho_psc$psc_stock_name)` to see all PSC stock)
#'
#' @return Dataframe of results:
#'    * `$fishery_id`:  Fram fishery id
#'    * `$preseason_mort_focal`:  Preseason estimated mortality of the focal stock (as defined with the `stock` parameter) in numbers of fish.
#'    * `postseason_mort_focal`:  Postseason mortality of the focal stock (as defined with the `stock` parameter) in numbers of fish.
#'    * `preseason_mort_other`:  Preseason estimated mortality of all stocks except the focal stock (as defined with the `stock` parameter) in numbers of fish.
#'    * `postseason_mort_other`:  Postseason mortality of all stocks except the focal stock (as defined with the `stock` parameter) in numbers of fish.
#'    * `overage`:  `postseason_mort_focal` - `preseason_mort_focal`. Positive values mean we believe we had higher mortality than we had estimated in the preseason.
#'    * `fishery_title`:  Name of the FRAM fishery
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_mort_comparison(
#' con.pre = framrsquared::connect_fram_db("PSC_CoTC_Preseason_CohoFRAMDB_thru2024_06182024.mdb"),
#' con.post = framrsquared::connect_fram_db("PSC_CoTC_PostSeason_CohoFRAMDB_thru2022_03042024.mdb"),
#' year = 2019,
#' stock = "Grays Harbor"
#' )
#' }
#'
create_mort_comparison <- function(con.pre,
                                   con.post,
                                   year.focal,
                                   stock.focal) {

  ## identify run
  id.pre <- framrsquared::fetch_table(con.pre, "RunID") |>
    dplyr::filter(.data$run_year == year.focal) |>
    dplyr::pull(.data$run_id)

  id.post <- framrsquared::fetch_table(con.post, "RunID") |>
    dplyr::filter(.data$run_year == year.focal) |>
    dplyr::pull(.data$run_id)

  stock_id_list <- framrosetta::stock_coho_psc |>
    dplyr::filter(.data$psc_stock_name == stock.focal) |>
    dplyr::pull(.data$fram_stock_id)

  morts.pre <- framrsquared::fetch_table(con.pre, table_name = "Mortality") |>
    dplyr::filter(.data$run_id == id.pre) |>
    dplyr::mutate(focal_stock = as.character(.data$stock_id %in% stock_id_list)) |>
    dplyr::mutate(
      total_mort = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off +
        .data$msf_landed_catch + .data$msf_non_retention + .data$msf_shaker + .data$msf_drop_off
    ) |>
    dplyr::group_by(.data$fishery_id, .data$focal_stock) |>
    dplyr::summarize(total_mort_pre = sum(.data$total_mort)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(values_from = "total_mort_pre", names_from = "focal_stock") |>
    dplyr::rename(
      preseason_mort_focal = "TRUE",
      preseason_mort_other = "FALSE"
    ) |>
    dplyr::mutate(preseason_mort_focal = dplyr::if_else(is.na(.data$preseason_mort_focal), 0, .data$preseason_mort_focal)) |>
    dplyr::mutate(preseason_mort_other = dplyr::if_else(is.na(.data$preseason_mort_other), 0, .data$preseason_mort_other))

  morts.post <- framrsquared::fetch_table(con.post, table_name = "Mortality") |>
    dplyr::filter(.data$run_id == id.post) |>
    dplyr::mutate(focal_stock = as.character(.data$stock_id %in% stock_id_list)) |>
    dplyr::mutate(
      total_mort = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off +
        .data$msf_landed_catch + .data$msf_non_retention + .data$msf_shaker + .data$msf_drop_off
    ) |>
    dplyr::group_by(.data$fishery_id, .data$focal_stock) |>
    dplyr::summarize(total_mort_post = sum(.data$total_mort)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(values_from = "total_mort_post", names_from = "focal_stock") |>
    dplyr::rename(
      postseason_mort_focal = "TRUE",
      postseason_mort_other = "FALSE"
    ) |>
    dplyr::mutate(postseason_mort_focal = dplyr::if_else(is.na(.data$postseason_mort_focal), 0, .data$postseason_mort_focal)) |>
    dplyr::mutate(postseason_mort_other = dplyr::if_else(is.na(.data$postseason_mort_other), 0, .data$postseason_mort_other))


  mort_compare <- morts.pre |>
    dplyr::left_join(morts.post, by = c("fishery_id")) |>
    dplyr::left_join(
      framrosetta::fishery_coho_fram |>
        dplyr::select("fishery_id", "fishery_title"),
      by = "fishery_id"
    ) |>
    dplyr::mutate(
      overage = .data$postseason_mort_focal - .data$preseason_mort_focal,
      .before = "fishery_title"
    ) |>
    dplyr::arrange(dplyr::desc(.data$overage)) |>
    dplyr::relocate("preseason_mort_other", .before = "postseason_mort_other") |>
    dplyr::relocate("postseason_mort_focal", .after = "preseason_mort_focal")
  return(mort_compare)
}

#' Create gt table comparing mortalities
#'
#' Wrapper function for `create_mort_comparison` to provide pretty output. Filters
#' out fisheries with less than 1 total fish, and fisheries that did not have at an overage of 1,
#' then formats into a `gt` table with clear labels and pretty format choices.
#'
#' @inheritParams create_mort_comparison
#'
#' @return gt table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' present_mort_comparison(
#' con.pre = framrsquared::connect_fram_db("PSC_CoTC_Preseason_CohoFRAMDB_thru2024_06182024.mdb"),
#' con.post = framrsquared::connect_fram_db("PSC_CoTC_PostSeason_CohoFRAMDB_thru2022_03042024.mdb"),
#' year = 2019,
#' stock = "Grays Harbor"
#' )
#' }
#'
present_mort_comparison <-  function(con.pre,
                                     con.post,
                                     year.focal,
                                     stock.focal) {

  mort_compare <- create_mort_comparison(con.pre,
                                         con.post,
                                         year.focal,
                                         stock.focal)
  mort_compare |>
    dplyr::filter(.data$postseason_mort_focal > 1) |>
    dplyr::filter(.data$postseason_mort_focal > (.data$preseason_mort_focal + 1)) |>
    dplyr::select(-"fishery_id") |>
    gt::gt() |>
    gt::fmt_number(decimals = 0) |>
    gt::cols_label(
      .data$preseason_mort_focal ~ "Pre-season mortalities\n(focal stock)",
      .data$postseason_mort_focal ~ "Post-season mortalities\n(focal stock)",
      .data$preseason_mort_other ~ "Pre-season mortalities\n(other stock)",
      .data$postseason_mort_other ~ "Post-season mortalities\n(other stock)",
      .data$fishery_title ~ "FRAM fishery"
    ) |>
    gt::tab_header(glue::glue("{stock.focal} {year.focal}"))
}
