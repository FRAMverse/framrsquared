# ── Helpers -------------------------------------------------------------------

# base_period_id 10 covers: fishery 1 ts 1, fishery 1 ts 2, fishery 2 ts 1
make_bp_mock_db <- function(species = "CHINOOK",
                            fishery_scalers  = NULL,
                            non_retention    = NULL,
                            double_checking = FALSE) {

  run_id <- data.frame(run_id = 1L, base_period_id = 10L)

  base_er <- data.frame(
    base_period_id    = c(10L, 10L, 10L, 10L),
    fishery_id        = c(1L,  1L,  2L,  3L),
    time_step         = c(1L,  2L,  1L,  1L),
    exploitation_rate = c(0.5, 0.3, 0.4, 0.0)  # fishery 3 has 0 ER - not covered
  )

  if (is.null(fishery_scalers)) {
    fishery_scalers <- data.frame(
      run_id                   = 1L,
      fishery_id               = 1L,
      time_step                = 1L,
      fishery_flag             = 1L,
      fishery_scale_factor     = 1.2,
      msf_fishery_scale_factor = 0.0,
      quota                    = 0L,
      msf_quota                = 0L
    )
  }

  if (is.null(non_retention)) {
    non_retention <- data.frame(
      run_id             = 1L,
      fishery_id         = 1L,
      time_step          = 1L,
      non_retention_flag = 1L,
      cnr_input1         = 0L,
      cnr_input2         = 0L,
      cnr_input3         = 100L,
      cnr_input4         = 50L
    )
  }

  table_list = list(
    RunID                = run_id,
    BaseExploitationRate = base_er,
    FisheryScalers       = fishery_scalers,
    NonRetention         = non_retention
  )

  if(!double_checking){ ## to help with test dev
    res <- make_queryable_mock_db_list(
      table_list = table_list,
      species = species
    )
  } else {
    res <- table_list
  }
  return(res)
}

## Example mock FRAM connection:
## make_bp_mock_db()
## Can examine list of tables instead of return fram connection with
## make_bp_mock_db(double_checking = TRUE)

# UNIT TESTS -------------------------------------------------------------------

## --- return structure ---------------------------------------------------------

test_that("check_bp_coverage() returns an invisible list", {
  fram_db <- make_bp_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_type(result, "list")
  expect_named(result, c("scalers_problem", "cnr_problems"))
})

test_that("check_bp_coverage() list elements are data frames", {
  fram_db <- make_bp_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_s3_class(result$scalers_problem, "data.frame")
  expect_s3_class(result$cnr_problems, "data.frame")
})

## --- scalers: no problems -----------------------------------------------------

test_that("check_bp_coverage() scalers_problem is empty when all scalers are in base period", {
  fram_db <- make_bp_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "All modeled fisheries are represented")
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "All modeled CNR is represented in bp!")
  })
  expect_equal(nrow(result$scalers_problem), 0L)
})

test_that("check_bp_coverage() zero-value scaler rows are not flagged as problems", {
  scalers <- data.frame(
    run_id                   = 1L,
    fishery_id               = 99L,  # not in base period, but zero values
    time_step                = 1L,
    fishery_flag             = 1L,
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = 0L,
    msf_quota                = 0L
  )
  fram_db <- make_bp_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_equal(nrow(result$scalers_problem), 0L)
})

test_that("check_bp_coverage() fisheries with only zero ER in base period are not treated as covered", {
  # fishery 3 has exploitation_rate = 0 in make_bp_mock_db(), so it is NOT covered
  scalers <- data.frame(
    run_id                   = 1L,
    fishery_id               = 3L,
    time_step                = 1L,
    fishery_flag             = 1L,
    fishery_scale_factor     = 1.5,
    msf_fishery_scale_factor = 0.0,
    quota                    = 0L,
    msf_quota                = 0L
  )
  fram_db <- make_bp_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "The following are represented in 'FisheryScalers' but not in the base period")
  })

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_equal(nrow(result$scalers_problem), 1L)
  expect_equal(result$scalers_problem$fishery_id, 3L)
})

## --- scalers: problems detected -----------------------------------------------

test_that("check_bp_coverage() scalers_problem has rows when a fishery is outside base period", {
  scalers <- data.frame(
    run_id                   = c(1L, 1L),
    fishery_id               = c(1L, 99L),  # fishery 99 not in base period
    time_step                = c(1L,  1L),
    fishery_flag             = c(1L,  1L),
    fishery_scale_factor     = c(1.2, 0.9),
    msf_fishery_scale_factor = c(0.0, 0.0),
    quota                    = c(0L,  0L),
    msf_quota                = c(0L,  0L)
  )
  fram_db <- make_bp_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "The following are represented in 'FisheryScalers' but not in the base period")
  })

  suppressMessage({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_equal(nrow(result$scalers_problem), 1L)
  expect_equal(result$scalers_problem$fishery_id, 99L)
  expect_equal(result$scalers_problem$time_step, 1L)
})

test_that("check_bp_coverage() scalers_problem captures multiple out-of-base-period rows", {
  scalers <- data.frame(
    run_id                   = c(1L, 1L, 1L),
    fishery_id               = c(1L, 98L, 99L),
    time_step                = c(1L,  1L,  2L),
    fishery_flag             = c(1L,  1L,  1L),
    fishery_scale_factor     = c(1.0, 0.5, 0.8),
    msf_fishery_scale_factor = c(0.0, 0.0, 0.0),
    quota                    = c(0L,  0L,  0L),
    msf_quota                = c(0L,  0L,  0L)
  )
  fram_db <- make_bp_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "fishery_id 98")
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "fishery_id 99")

  })

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_equal(nrow(result$scalers_problem), 2L)
  expect_true(all(c(98L, 99L) %in% result$scalers_problem$fishery_id))
})

## --- cnr: no problems ---------------------------------------------------------

test_that("check_bp_coverage() cnr_problems is empty when all CNR is in base period", {
  fram_db <- make_bp_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "All modeled CNR is represented")
  })

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_equal(nrow(result$cnr_problems), 0L)
})

test_that("check_bp_coverage() flag-0 non-retention rows are not flagged as problems", {
  nr <- data.frame(
    run_id             = 1L,
    fishery_id         = 99L,  # not in base period
    time_step          = 1L,
    non_retention_flag = 0L,   # flag 0 - ignored
    cnr_input1         = 100L,
    cnr_input2         = 100L,
    cnr_input3         = 100L,
    cnr_input4         = 100L
  )
  fram_db <- make_bp_mock_db(non_retention = nr)
  withr::defer(disconnect_mock_fram_db(fram_db))


  suppressMessages({
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "All modeled CNR is represented")
  })

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_equal(nrow(result$cnr_problems), 0L)
})

test_that("check_bp_coverage() zero-input non-retention rows are not flagged as problems", {
  nr <- data.frame(
    run_id             = 1L,
    fishery_id         = 99L,  # not in base period
    time_step          = 1L,
    non_retention_flag = 1L,
    cnr_input1         = 0L,
    cnr_input2         = 0L,
    cnr_input3         = 0L,   # flag 1 keeps cnr_input3 and cnr_input4; both zero
    cnr_input4         = 0L
  )
  fram_db <- make_bp_mock_db(non_retention = nr)
  withr::defer(disconnect_mock_fram_db(fram_db))


  suppressMessages({
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "All modeled CNR is represented")
  })

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_equal(nrow(result$cnr_problems), 0L)
})

## --- cnr: problems detected ---------------------------------------------------

test_that("check_bp_coverage() cnr_problems has rows when a CNR fishery is outside base period", {
  nr <- data.frame(
    run_id             = c(1L, 1L),
    fishery_id         = c(1L, 99L),  # fishery 99 not in base period
    time_step          = c(1L,  1L),
    non_retention_flag = c(1L,  1L),
    cnr_input1         = c(0L,  0L),
    cnr_input2         = c(0L,  0L),
    cnr_input3         = c(100L, 80L),
    cnr_input4         = c(50L,  40L)
  )
  fram_db <- make_bp_mock_db(non_retention = nr)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "The following are represented in 'NonRetention' but not in the base period")
  })

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_equal(nrow(result$cnr_problems), 1L)
  expect_equal(result$cnr_problems$fishery_id, 99L)
  expect_equal(result$cnr_problems$time_step, 1L)
})

## --- scalers and cnr problems together ----------------------------------------

test_that("check_bp_coverage() detects both scalers and CNR problems simultaneously", {
  scalers <- data.frame(
    run_id                   = 1L,
    fishery_id               = 97L,
    time_step                = 1L,
    fishery_flag             = 1L,
    fishery_scale_factor     = 1.1,
    msf_fishery_scale_factor = 0.0,
    quota                    = 0L,
    msf_quota                = 0L
  )
  nr <- data.frame(
    run_id             = 1L,
    fishery_id         = 98L,
    time_step          = 1L,
    non_retention_flag = 1L,
    cnr_input1         = 0L,
    cnr_input2         = 0L,
    cnr_input3         = 100L,
    cnr_input4         = 50L
  )
  fram_db <- make_bp_mock_db(fishery_scalers = scalers, non_retention = nr)
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "The following are represented in 'NonRetention' but not in the base period")
    expect_message(check_bp_coverage(fram_db, run_id = 1L),
                   regexp = "The following are represented in 'FisheryScalers' but not in the base period")
  })

  suppressMessages({
    result <- check_bp_coverage(fram_db, run_id = 1L)
  })

  expect_equal(nrow(result$scalers_problem), 1L)
  expect_equal(nrow(result$cnr_problems), 1L)
})
