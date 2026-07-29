# Extracted from test-post_season.R:521

# prequel ----------------------------------------------------------------------
make_psa_mock_db <- function(
    run_year     = 2022L,
    run_type     = "Post",
    scale_factor = 2.0,
    base_cohort  = 5000.0
) {
  run_id_tbl <- data.frame(
    run_id         = 1L,
    base_period_id = 10L,
    run_year       = as.integer(run_year),
    run_type       = run_type,
    run_name       = "TestRun"
  )

  stock_tbl <- data.frame(
    stock_id        = c(1L, 2L, 3L),
    stock_version   = 1L,
    species         = "COHO",
    stock_long_name = c("River Wild Stock", "River Hatchery Stock", "Other Stock"),
    stock_name      = c("RWS", "RHS", "OTH")
  )

  stock_recruit <- data.frame(
    run_id               = 1L,
    stock_id             = c(1L, 2L, 3L),
    age                  = 3L,
    recruit_scale_factor = scale_factor
  )

  base_cohort_tbl <- data.frame(
    base_period_id   = 10L,
    stock_id         = c(1L, 2L, 3L),
    age              = 3L,
    base_cohort_size = base_cohort
  )

  make_queryable_mock_db_list(
    table_list = list(
      RunID        = run_id_tbl,
      Stock        = stock_tbl,
      StockRecruit = stock_recruit,
      BaseCohort   = base_cohort_tbl
    ),
    species = "COHO"
  )
}
make_bkfram_mock_db <- function(
    bk_fishery_flag  = 2L,
    fwd_fishery_flag = 2L,
    bk_scale_factor  = 0.0,
    fwd_buoy_quota   = 50.0,
    bk_coastal       = "No",
    fwd_coastal      = "No"
) {
  run_id_tbl <- data.frame(
    run_id             = c(1L, 2L),
    base_period_id     = c(10L, 10L),
    run_name           = c("BK Run", "FWD Run"),
    coastal_iterations = c(bk_coastal, fwd_coastal)
  )

  # Fishery 1 = generic; fishery 23 = Buoy 10 Sport.
  # Buoy 10 quota is 0 in bk run (correct) and fwd_buoy_quota in fwd run.
  fishery_scalers <- data.frame(
    run_id                   = c(1L, 1L, 2L, 2L),
    fishery_id               = c(1L, 23L, 1L, 23L),
    time_step                = 1L,
    fishery_flag             = c(bk_fishery_flag, 2L, fwd_fishery_flag, 2L),
    fishery_scale_factor     = c(bk_scale_factor, 0.0, 0.0, 0.0),
    msf_fishery_scale_factor = 0.0,
    quota                    = c(100.0, 0.0, 100.0, fwd_buoy_quota),
    msf_quota                = 0.0
  )

  stock_tbl <- data.frame(
    stock_id        = 1L,
    stock_version   = 1L,
    species         = "COHO",
    stock_name      = "TST",
    stock_long_name = "Test Stock"
  )

  fishery_tbl <- data.frame(
    fishery_id     = c(1L, 23L),
    version_number = 1L,
    species        = "COHO",
    fishery_name   = c("Generic Fishery", "Buoy 10 Sport"),
    fishery_title  = c("Generic Fishery", "Buoy 10 Sport")
  )

  escapement_tbl <- data.frame(
    primary_key = c(1L, 2L),
    run_id      = c(1L, 2L),
    stock_id    = 1L,
    age         = 3L,
    time_step   = 1L,
    escapement  = 500.0
  )

  backwards_fram <- data.frame(
    run_id          = c(1L, 2L),
    stock_id        = 1L,
    target_esc_age3 = 500.0,
    target_flag     = 1L
  )

  # Mortality needs all columns required by add_total_mortality()
  mortality_tbl <- data.frame(
    primary_key       = c(1L, 2L),
    run_id            = c(1L, 2L),
    fishery_id        = 1L,
    stock_id          = 1L,
    age               = 3L,
    time_step         = 1L,
    landed_catch      = 50.0,
    non_retention     = 0.0,
    shaker            = 0.0,
    drop_off          = 0.0,
    msf_landed_catch  = 0.0,
    msf_non_retention = 0.0,
    msf_shaker        = 0.0,
    msf_drop_off      = 0.0,
    encounter         = 0.0,
    msf_encounter     = 0.0
  )

  make_queryable_mock_db_list(
    table_list = list(
      RunID          = run_id_tbl,
      FisheryScalers = fishery_scalers,
      Stock          = stock_tbl,
      Fishery        = fishery_tbl,
      Escapement     = escapement_tbl,
      BackwardsFRAM  = backwards_fram,
      Mortality      = mortality_tbl
    ),
    species = "COHO",
    type    = "full"
  )
}

# test -------------------------------------------------------------------------
skip_if_no_test_db()
fram_db <- connection_coho_post()
withr::defer(disconnect_mock_fram_db(fram_db))
result <- post_season_abundance(fram_db)
