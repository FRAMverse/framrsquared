# ── Helpers: post_season_abundance() -----------------------------------------

# Three stocks covering all three origin categories (Wild, Hatchery, Misc).
make_psa_mock_db <- function(
    return_list = FALSE,
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

  table_list = list(
    RunID        = run_id_tbl,
    Stock        = stock_tbl,
    StockRecruit = stock_recruit,
    BaseCohort   = base_cohort_tbl
  )


  if(return_list){
    table_list
  } else {
    make_queryable_mock_db_list(
      table_list = table_list,
      species = "COHO"
    )
  }
}

# ── Helpers: bkfram_checks_coho() --------------------------------------------

make_bkfram_mock_db <- function(
    bk_fishery_flag  = 2L,
    fwd_fishery_flag = 2L,
    bk_scale_factor  = 0.0,
    fwd_buoy_quota   = 50.0,
    bk_coastal       = "No",
    fwd_coastal      = "No",
    return_list = FALSE
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

  table_list = list(
    RunID          = run_id_tbl,
    FisheryScalers = fishery_scalers,
    Stock          = stock_tbl,
    Fishery        = fishery_tbl,
    Escapement     = escapement_tbl,
    BackwardsFRAM  = backwards_fram,
    Mortality      = mortality_tbl
  )

  if(return_list){
    return(table_list)
  } else {
    make_queryable_mock_db_list(
      table_list = table_list,
      species = "COHO",
      type    = "full"
    )
  }
}


# UNIT TESTS -------------------------------------------------------------------

## --- post_season_abundance() -------------------------------------------------

test_that("post_season_abundance() errors on a Chinook database", {
  fram_db <- make_mock_fram_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(post_season_abundance(fram_db), class = "framrsquared_error")
})

test_that("post_season_abundance() errors on an invalid run_id", {
  fram_db <- make_psa_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(post_season_abundance(fram_db, run_ids = 999L), class = "framrsquared_error")
})

test_that("post_season_abundance() returns a data frame", {
  fram_db <- make_psa_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db),
    regexp = "Abundances given in terms of January age 3"
  )
  expect_s3_class(result, "data.frame")
})

test_that("post_season_abundance() contains stock_id, stock_name, and origin columns", {
  fram_db <- make_psa_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db),
    regexp = "Abundances given in terms of January age 3"
  )

  expect_true(all(c("stock_id", "stock_name", "origin") %in% names(result)))
})

test_that("post_season_abundance() ja3: recruit_cohort_size = scale_factor * base_cohort_size", {
  fram_db <- make_psa_mock_db(scale_factor = 3.0, base_cohort = 4000.0)
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db),
    regexp = "Abundances given in terms of January age 3"
  )

  expect_true("2022" %in% names(result))
  expect_equal(unique(result$`2022`), 12000.0)
})

test_that("post_season_abundance() oa3: values equal ja3 / 1.2317", {
  fram_db <- make_psa_mock_db(scale_factor = 1.0, base_cohort = 12317.0)
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    ja3_result <- post_season_abundance(fram_db, units = "ja3"),
    regexp = "Abundances given in terms of January age 3"
  )

  expect_message(
    oa3_result <- post_season_abundance(fram_db, units = "oa3"),
    regexp = "Abundances given in terms of ocean age 3"
  )

  expect_equal(oa3_result$`2022`, ja3_result$`2022` / 1.2317, tolerance = 1e-4)
})

test_that("post_season_abundance() assigns 'Wild' origin to stocks with 'Wild' in name", {
  fram_db <- make_psa_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db),
    regexp = "Abundances given in terms of January age 3"
  )
  expect_equal(result$origin[result$stock_name == "RWS"], "Wild")
})

test_that("post_season_abundance() assigns 'Hatchery' origin to stocks with 'Hatchery' in name", {
  fram_db <- make_psa_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db),
    regexp = "Abundances given in terms of January age 3"
  )
  expect_equal(result$origin[result$stock_name == "RHS"], "Hatchery")
})

test_that("post_season_abundance() assigns 'Misc' to stocks with no neither Hatchery nor Wild in name", {
  fram_db <- make_psa_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db),
    regexp = "Abundances given in terms of January age 3"
  )
  expect_equal(result$origin[result$stock_name == "OTH"], "Misc")
})

test_that("post_season_abundance() excludes non-Post run_type runs", {
  run_id_tbl <- data.frame(
    run_id         = c(1L, 2L),
    base_period_id = 10L,
    run_year       = c(2022L, 2021L),
    run_type       = c("Post", "Pre"),
    run_name       = c("PostRun", "PreRun")
  )
  stock_tbl <- data.frame(
    stock_id = 1L, stock_version = 1L, species = "COHO",
    stock_long_name = "Wild Stock", stock_name = "WS"
  )
  sr <- data.frame(run_id = c(1L, 2L), stock_id = 1L, age = 3L,
                   recruit_scale_factor = 1.0)
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 1000.0)

  fram_db <- make_queryable_mock_db_list(
    list(RunID = run_id_tbl, Stock = stock_tbl, StockRecruit = sr, BaseCohort = bc),
    species = "COHO"
  )
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db),
    regexp = "Abundances given in terms of January age 3"
  )

  expect_true("2022" %in% names(result))
  expect_false("2021" %in% names(result))
})

test_that("post_season_abundance() excludes run_year < 2010", {
  run_id_tbl <- data.frame(
    run_id         = c(1L, 2L),
    base_period_id = 10L,
    run_year       = c(2022L, 2005L),
    run_type       = "Post",
    run_name       = c("Recent", "Old")
  )
  stock_tbl <- data.frame(
    stock_id = 1L, stock_version = 1L, species = "COHO",
    stock_long_name = "Wild Stock", stock_name = "WS"
  )
  sr <- data.frame(run_id = c(1L, 2L), stock_id = 1L, age = 3L,
                   recruit_scale_factor = 1.0)
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 1000.0)

  fram_db <- make_queryable_mock_db_list(
    list(RunID = run_id_tbl, Stock = stock_tbl, StockRecruit = sr, BaseCohort = bc),
    species = "COHO"
  )
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db),
    regexp = "Abundances given in terms of January age 3"
  )

  expect_true("2022" %in% names(result))
  expect_false("2005" %in% names(result))
})

test_that("post_season_abundance() filters to specified run_ids", {
  run_id_tbl <- data.frame(
    run_id         = c(1L, 2L),
    base_period_id = 10L,
    run_year       = c(2022L, 2021L),
    run_type       = "Post",
    run_name       = c("Run1", "Run2")
  )
  stock_tbl <- data.frame(
    stock_id = 1L, stock_version = 1L, species = "COHO",
    stock_long_name = "Wild Stock", stock_name = "WS"
  )
  sr <- data.frame(run_id = c(1L, 2L), stock_id = 1L, age = 3L,
                   recruit_scale_factor = 1.0)
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 1000.0)

  fram_db <- make_queryable_mock_db_list(
    list(RunID = run_id_tbl, Stock = stock_tbl, StockRecruit = sr, BaseCohort = bc),
    species = "COHO"
  )
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db, run_ids = 1L),
    regexp = "Abundances given in terms of January age 3"
  )

  expect_true("2022" %in% names(result))
  expect_false("2021" %in% names(result))
})


test_that("post_season_abundance() errors when run_ids = NULL and multiple runs share a run_year", {
  run_id_tbl <- data.frame(
    run_id         = c(1L, 2L),
    base_period_id = c(10L, 10L),
    run_year       = c(2022L, 2022L),
    run_type       = "Post",
    run_name       = c("Run1", "Run2")
  )
  stock_tbl <- data.frame(
    stock_id = 1L, stock_version = 1L, species = "COHO",
    stock_long_name = "Wild Stock", stock_name = "WS"
  )
  sr <- data.frame(run_id = c(1L, 2L), stock_id = 1L, age = 3L,
                   recruit_scale_factor = 1.0)
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 1000.0)

  fram_db <- make_queryable_mock_db_list(
    list(RunID = run_id_tbl, Stock = stock_tbl, StockRecruit = sr, BaseCohort = bc),
    species = "COHO"
  )
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(post_season_abundance(fram_db), class = "framrsquared_error")
})

test_that("post_season_abundance() labels columns by run_id when run_ids provided with duplicate years", {
  run_id_tbl <- data.frame(
    run_id         = c(1L, 2L),
    base_period_id = c(10L, 10L),
    run_year       = c(2022L, 2022L),
    run_type       = "Post",
    run_name       = c("Run1", "Run2")
  )
  stock_tbl <- data.frame(
    stock_id = 1L, stock_version = 1L, species = "COHO",
    stock_long_name = "Wild Stock", stock_name = "WS"
  )
  sr <- data.frame(run_id = c(1L, 2L), stock_id = 1L, age = 3L,
                   recruit_scale_factor = 1.0)
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 1000.0)

  fram_db <- make_queryable_mock_db_list(
    list(RunID = run_id_tbl, Stock = stock_tbl, StockRecruit = sr, BaseCohort = bc),
    species = "COHO"
  )
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    expect_message(
      result <- post_season_abundance(fram_db, run_ids = c(1L, 2L)),
      regexp = "Abundances given in terms of January age 3"
    ),
    regexp = "have multiple runs associated"
  )


  expect_true("run_1" %in% names(result))
  expect_true("run_2" %in% names(result))
})


## --- bkfram_checks_coho() ----------------------------------------------------



test_that("bkfram_checks_coho() errors when backward_run_id or forwards run is not a valid run", {
  fram_db <- make_bkfram_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = NULL, forward_run_id = 1L)
  )

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = "ten", forward_run_id = 1L)
  )

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = 1:5, forward_run_id = 1L)
  )

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = list(1), forward_run_id = 1L)
  )

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = 10, forward_run_id = 1L)
  )

  ############### forwards

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = 1, forward_run_id = NULL)
  )

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = 1, forward_run_id = "ten")
  )

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = 1, forward_run_id = 1:5)
  )

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = 1, forward_run_id = list(1))
  )

  expect_error_m_f(
    bkfram_checks_coho(fram_db, backward_run_id = 1, forward_run_id = 10)
  )

})

test_that("bkfram_checks_coho() errors on a Chinook database", {
  fram_db <- make_mock_fram_db(type = "full", species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(bkfram_checks_coho(fram_db, 1L, 2L),
               regexp = "specifically for COHO",
               class = "framrsquared_error")
})

test_that("bkfram_checks_coho() errors on a transfer database", {
  fram_db <- make_mock_fram_db(type = "transfer", species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(bkfram_checks_coho(fram_db, 1L, 2L),
               regexp = "requires a full database",
               class = "framrsquared_error")
})

test_that("bkfram_checks_coho() returns a tibble with 12 rows and check/type/data columns", {
  fram_db <- make_bkfram_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    print = function(...) {},
    .package = "base"
  )

  suppressMessages(
    result <- bkfram_checks_coho(fram_db, backward_run_id = 1L, forward_run_id = 2L)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 12L)
  expect_true(all(c("check", "type", "data") %in% names(result)))
})

test_that("bkfram_checks_coho() backward bad-flags data is empty when all flags are valid quota flags", {

  local_mocked_bindings(
    print = function(...) {},
    .package = "base"
  )


  # fishery_flag = 2 (valid), scale_factor = 0 -> no bad flags
  fram_db <- make_bkfram_mock_db(bk_fishery_flag = 2L, bk_scale_factor = 0.0)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages(
    result <- bkfram_checks_coho(fram_db, backward_run_id = 1L, forward_run_id = 2L)
  )
  bk_flags_data <- result$data[result$check == "scaler flags backward"][[1]]
  expect_equal(nrow(bk_flags_data), 0L)
})

test_that("bkfram_checks_coho() backward bad-flags data has rows when non-quota flag paired with non-zero scaler", {

  local_mocked_bindings(
    print = function(...) {},
    .package = "base"
  )

  # fishery_flag = 1 (not in 2/8/28) AND scale_factor != 0 -> bad
  fram_db <- make_bkfram_mock_db(bk_fishery_flag = 1L, bk_scale_factor = 1.0)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages(
    result <- bkfram_checks_coho(fram_db, backward_run_id = 1L, forward_run_id = 2L)
  )
  bk_flags_data <- result$data[result$check == "scaler flags backward"][[1]]
  expect_gt(nrow(bk_flags_data), 0L)
})

test_that("bkfram_checks_coho() detects flag mismatches between backward and forward runs", {

  local_mocked_bindings(
    print = function(...) {},
    .package = "base"
  )


  # bk fishery 1 has flag 2; fwd fishery 1 has flag 8 -> mismatch
  fram_db <- make_bkfram_mock_db(bk_fishery_flag = 2L, fwd_fishery_flag = 8L)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages(
    result <- bkfram_checks_coho(fram_db, backward_run_id = 1L, forward_run_id = 2L)
  )

  mismatch_data <- result$data[result$check == "flagging differences"][[1]]
  expect_gt(nrow(mismatch_data), 0L)
})

test_that("bkfram_checks_coho() reports no flag mismatch when backward and forward flags match", {

  local_mocked_bindings(
    print = function(...) {},
    .package = "base"
  )


  fram_db <- make_bkfram_mock_db(bk_fishery_flag = 2L, fwd_fishery_flag = 2L)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages(
    result <- bkfram_checks_coho(fram_db, backward_run_id = 1L, forward_run_id = 2L)
  )

  mismatch_data <- result$data[result$check == "flagging differences"][[1]]
  expect_equal(nrow(mismatch_data), 0L)
})

test_that("bkfram_checks_coho() stores the coastal_iterations value for each run", {

  local_mocked_bindings(
    print = function(...) {},
    .package = "base"
  )

  fram_db <- make_bkfram_mock_db(bk_coastal = "No", fwd_coastal = "Yes")
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages(
    result <- bkfram_checks_coho(fram_db, backward_run_id = 1L, forward_run_id = 2L)
  )

  bk_coastal  <- result$data[result$check == "coastal iterations backward"][[1]]
  fwd_coastal <- result$data[result$check == "coastal iterations forward"][[1]]
  expect_equal(bk_coastal,  "No")
  expect_equal(fwd_coastal, "Yes")
})

test_that("bkfram_checks_coho() buoy 10 backward data has rows when backward run has non-zero buoy quota", {

  local_mocked_bindings(
    print = function(...) {},
    .package = "base"
  )


  # bk_buoy_quota != 0 means backward run incorrectly has buoy 10 quota
  fram_db <- make_bkfram_mock_db(bk_fishery_flag = 2L)

  # Override to give bk run a buoy 10 quota by building a custom FisheryScalers
  run_id_tbl <- data.frame(
    run_id = c(1L, 2L), base_period_id = c(10L, 10L),
    run_name = c("BK", "FWD"), coastal_iterations = "No"
  )
  fishery_scalers <- data.frame(
    run_id                   = c(1L, 1L, 2L, 2L),
    fishery_id               = c(1L, 23L, 1L, 23L),
    time_step                = 1L,
    fishery_flag             = 2L,
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(100.0, 75.0, 100.0, 50.0),  # bk buoy 10 = 75 (bad)
    msf_quota                = 0.0
  )
  stock_tbl     <- data.frame(stock_id = 1L, stock_version = 1L, species = "COHO",
                              stock_name = "TST", stock_long_name = "Test")
  fishery_tbl   <- data.frame(fishery_id = c(1L, 23L), version_number = 1L,
                              species = "COHO",
                              fishery_name = c("Generic", "Buoy 10 Sport"),
                              fishery_title = c("Generic", "Buoy 10 Sport"))
  escapement    <- data.frame(primary_key = c(1L, 2L), run_id = c(1L, 2L),
                              stock_id = 1L, age = 3L, time_step = 1L, escapement = 500.0)
  bk_fram       <- data.frame(run_id = c(1L, 2L), stock_id = 1L,
                              target_esc_age3 = 500.0, target_flag = 1L)
  mortality     <- data.frame(
    primary_key = c(1L, 2L), run_id = c(1L, 2L), fishery_id = 1L, stock_id = 1L,
    age = 3L, time_step = 1L, landed_catch = 50.0, non_retention = 0.0,
    shaker = 0.0, drop_off = 0.0, msf_landed_catch = 0.0,
    msf_non_retention = 0.0, msf_shaker = 0.0, msf_drop_off = 0.0,
    encounter = 0.0, msf_encounter = 0.0
  )

  fram_db2 <- make_queryable_mock_db_list(
    list(RunID = run_id_tbl, FisheryScalers = fishery_scalers, Stock = stock_tbl,
         Fishery = fishery_tbl, Escapement = escapement, BackwardsFRAM = bk_fram,
         Mortality = mortality),
    species = "COHO", type = "full"
  )
  withr::defer(disconnect_mock_fram_db(fram_db2))

  suppressMessages(
    result <- bkfram_checks_coho(fram_db2, backward_run_id = 1L, forward_run_id = 2L)
  )

  bk_buoy_data <- result$data[result$check == "buoy 10 backward"][[1]]
  expect_gt(nrow(bk_buoy_data), 0L)
})


# INTEGRATION TESTS ------------------------------------------------------------

test_that("post_season_abundance() works on a real Coho post-season database", {

  local_mocked_bindings(
    print = function(...) {},
    .package = "base"
  )

  skip_if_no_test_db()
  fram_db <- connection_coho_post(quiet = TRUE)
  withr::defer(disconnect_mock_fram_db(fram_db))


  expect_message(
    result <- post_season_abundance(fram_db,
                                    run_ids = c(34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L,
                                                49L, 52L)),
    regexp = "Abundances given in terms of January age 3"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("stock_id", "stock_name", "origin") %in% names(result)))
  expect_gt(nrow(result), 0L)
})

test_that("post_season_abundance() ja3 values are 1.2317 times oa3 values in  a real database", {
  skip_if_no_test_db()
  fram_db <- connection_coho_post(quiet = TRUE)
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    ja3 <- post_season_abundance(fram_db, units = "ja3", run_id = 34:40),
    regexp = "Abundances given in terms of January age 3"
  )
  expect_message(
    oa3 <- post_season_abundance(fram_db, units = "oa3", run_id = 34:40),
    regexp = "Abundances given in terms of ocean age 3"
  )

  year_cols <- setdiff(names(ja3), c("stock_id", "stock_name", "origin"))
  col <- year_cols[[1]]
  oa3_vals = oa3[[col]]
  ja3_vals = ja3[[col]]


  ratios = (ja3_vals/oa3_vals)
  ratios = ratios[!is.na(ratios)]

  ## handle numerical imprecision
  expect_true(all(abs(ratios - 1.2317) < 1e-10))
})

test_that("post_season_abundance() origin column contains only Wild, Hatchery, or Misc on a real database", {
  skip_if_no_test_db()
  fram_db <- connection_coho_post(quiet = TRUE)
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    result <- post_season_abundance(fram_db, run_ids = 34:40),
    regexp = "Abundances given in terms of January age 3"
  )
  expect_true(all(result$origin %in% c("Wild", "Hatchery", "Misc")))
})
