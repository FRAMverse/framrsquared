# ── Helpers -------------------------------------------------------------------

make_cmp_run_id_tbl <- function(ids = c(1L, 2L), bp_id = 10L) {
  data.frame(
    run_id         = ids,
    run_name       = paste0("Run ", LETTERS[seq_along(ids)]),
    base_period_id = bp_id,
    run_time_date  = as.POSIXct(
      paste0("2024-01-0", seq_along(ids), " 10:00:00"), tz = "UTC"
    )
  )
}

make_cmp_base_id_tbl <- function(bp_id = 10L, species = "CHINOOK") {
  data.frame(
    base_period_id    = bp_id,
    fishery_version   = 1L,
    stock_version     = 1L,
    time_step_version = 1L,
    species_name      = species,
    base_period_name  = "Test BP"
  )
}

make_cmp_fishery_tbl <- function(species = "CHINOOK") {
  data.frame(
    fishery_id     = 1:3L,
    version_number = 1L,
    species        = species,
    fishery_title  = c("Troll", "Net", "Sport"),
    fishery_name   = c("T", "N", "S")
  )
}

make_cmp_stock_tbl <- function(species = "CHINOOK") {
  data.frame(
    stock_id        = 1:2L,
    stock_version   = 1L,
    species         = species,
    stock_long_name = c("Stock Alpha", "Stock Beta"),
    stock_name      = c("A", "B")
  )
}

# Minimal default FisheryScalers: identical inputs across both runs
default_fishery_scalers <- function() {
  data.frame(
    run_id                   = c(1L, 1L, 2L, 2L),
    fishery_id               = c(1L, 2L, 1L, 2L),
    time_step                = c(1L, 1L, 1L, 1L),
    fishery_flag             = c(2L, 2L, 2L, 2L),
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(1000L, 500L, 1000L, 500L),
    msf_quota                = 0L
  )
}

default_non_retention <- function() {
  data.frame(
    run_id             = c(1L, 2L),
    fishery_id         = c(1L, 1L),
    time_step          = c(1L, 1L),
    non_retention_flag = c(1L, 1L),
    cnr_input1         = 0L,
    cnr_input2         = 0L,
    cnr_input3         = c(100L, 100L),
    cnr_input4         = c(50L,  50L)
  )
}

# Core helper: builds a queryable mock db for compare_* functions.
# Pass custom tables to override the defaults.
make_compare_mock_db <- function(species         = "CHINOOK",
                                 fishery_scalers = NULL,
                                 non_retention   = NULL,
                                 sl_ratio        = NULL,
                                 stock_recruit   = NULL,
                                 base_cohort     = NULL,
                                 sfrs            = NULL,
                                 double_checking = FALSE) {
  tables <- list(
    RunID          = make_cmp_run_id_tbl(),
    BaseID         = make_cmp_base_id_tbl(species = species),
    Fishery        = make_cmp_fishery_tbl(species),
    Stock          = make_cmp_stock_tbl(species),
    FisheryScalers = if (is.null(fishery_scalers)) default_fishery_scalers() else fishery_scalers,
    NonRetention   = if (is.null(non_retention))   default_non_retention()   else non_retention
  )
  if (!is.null(sl_ratio))      tables[["SLRatio"]]                <- sl_ratio
  if (!is.null(stock_recruit)) tables[["StockRecruit"]]           <- stock_recruit
  if (!is.null(base_cohort))   tables[["BaseCohort"]]             <- base_cohort
  if (!is.null(sfrs))          tables[["StockFisheryRateScaler"]] <- sfrs

  if(double_checking){
    return(tables)
  } else {
    return(
      make_queryable_mock_db_list(table_list = tables, species = species)
    )
  }
}


# UNIT TESTS -------------------------------------------------------------------

## --- input_summary_() --------------------------------------------------------

test_that("input_summary_() errors on non-dataframe input", {
  expect_error(input_summary_(list(run_id = 1), run_id = 1),
               class = "framrsquared_error")
})

test_that("input_summary_() filters to the specified run_id", {
  df <- data.frame(run_id = c(1L, 2L), fishery_id = c(1L, 1L),
                   fishery_flag = c(2L, 2L), time_step = c(1L, 1L),
                   quota = c(100L, 200L), msf_quota = c(0L, 0L))
  result <- input_summary_(df, run_id = 1L)
  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), 1L)
})

test_that("input_summary_() computes total_quota correctly for each flag group", {
  df <- data.frame(
    run_id       = rep(1L, 8),
    fishery_id   = 1:8L,
    time_step    = 1L,
    fishery_flag = c(1L, 2L, 7L, 8L, 17L, 18L, 27L, 28L),
    quota        = 1000L,
    msf_quota    = 200L
  )
  result <- input_summary_(df, run_id = 1L)

  # flags 1 & 2: use quota only
  expect_equal(result$total_quota[result$fishery_flag %in% c(1, 2)], c(1000, 1000))
  # flags 7 & 8: use msf_quota only
  expect_equal(result$total_quota[result$fishery_flag %in% c(7, 8)], c(200, 200))
  # flags 17, 18, 27, 28: quota + msf_quota
  expect_true(all(result$total_quota[result$fishery_flag %in% c(17, 18, 27, 28)] == 1200))
})

test_that("input_summary_() assigns regulation labels correctly", {
  df <- data.frame(
    run_id       = rep(1L, 5),
    fishery_id   = 1:5L,
    time_step    = 1L,
    fishery_flag = c(0L, 1L, 7L, 17L, 28L),
    quota        = 0L,
    msf_quota    = 0L
  )
  result <- input_summary_(df, run_id = 1L)

  expect_equal(result$regulation[result$fishery_flag == 0],  "none")
  expect_equal(result$regulation[result$fishery_flag == 1],  "NS")
  expect_equal(result$regulation[result$fishery_flag == 7],  "MSF")
  expect_equal(result$regulation[result$fishery_flag == 17], "NS+MSF")
  expect_equal(result$regulation[result$fishery_flag == 28], "NS+MSF")
})

test_that("input_summary_() returns the correct columns", {
  df <- data.frame(run_id = 1L, fishery_id = 1L, fishery_flag = 2L,
                   time_step = 1L, quota = 100L, msf_quota = 0L)
  result <- input_summary_(df, run_id = 1L)
  expect_named(result,
               c("run_id", "fishery_id", "fishery_flag", "time_step", "total_quota", "regulation"))
})


## --- compare_inputs() --------------------------------------------------------

test_that("compare_inputs() errors on invalid fram_db", {
  expect_error(compare_inputs(list(), run_ids = c(1L, 2L)),
               class = "framrsquared_error")
})

test_that("compare_inputs() errors when run_ids has length != 2", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_inputs(fram_db, run_ids = 1L),          class = "framrsquared_error")
  expect_error(compare_inputs(fram_db, run_ids = c(1L, 2L, 3L)), class = "framrsquared_error")
})

test_that("compare_inputs() returns a data frame with expected columns", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_inputs(fram_db, run_ids = c(1L, 2L))

  expect_s3_class(result, "data.frame")
  expect_true(all(c("fishery_id", "fishery_label", "time_step",
                    "total_quota_original", "total_quota_comparison",
                    "percent_diff", "reg_change") %in% names(result)))
})

test_that("compare_inputs() computes percent_diff correctly", {
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(2L, 2L),
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(1000L, 1200L),
    msf_quota                = 0L
  )
  fram_db <- make_compare_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_inputs(fram_db, run_ids = c(1L, 2L))

  expect_equal(result$percent_diff[result$fishery_id == 1L], 0.2)
})

test_that("compare_inputs() populates reg_change when regulation type changes", {
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(2L, 8L),  # NS -> MSF
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(500L, 0L),
    msf_quota                = c(0L, 300L)
  )
  fram_db <- make_compare_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_inputs(fram_db, run_ids = c(1L, 2L))

  expect_equal(result$reg_change[result$fishery_id == 1L], "NS->MSF")
})

test_that("compare_inputs() sets comparison attributes", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_inputs(fram_db, run_ids = c(1L, 2L))

  expect_equal(attr(result, "original_run_id"),    1L)
  expect_equal(attr(result, "comparison_run_id"),  2L)
  expect_equal(attr(result, "original_run_name"),  "Run A")
  expect_equal(attr(result, "comparison_run_name"),"Run B")
})

## --- compare_fishery_input_flags() -------------------------------------------

test_that("compare_fishery_input_flags() returns empty result when flags are identical", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_input_flags(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 0L)
})

test_that("compare_fishery_input_flags() detects a changed flag", {
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(1L, 7L),   # flag changes 1 -> 7
    fishery_scale_factor     = 1.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = 0L,
    msf_quota                = 0L
  )
  fram_db <- make_compare_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_input_flags(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 1L)
  expect_equal(result$flag_original,   1L)
  expect_equal(result$flag_comparison, 7L)
})

test_that("compare_fishery_input_flags() output has expected columns", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_input_flags(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_true(
    all(c("fishery_id", "fishery_label", "time_step", "flag_original", "flag_comparison") %in% names(result))
  )
})

test_that("compare_fishery_input_flags() verbose works correctly", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  ## no message when verbose = FALSE
  expect_no_message(compare_fishery_input_flags(fram_db, run_ids = c(1L, 2L), verbose = FALSE))
  ## message when verbose = TRUE and no differences
  expect_message(compare_fishery_input_flags(fram_db, run_ids = c(1L, 2L), verbose = TRUE),
                 regexp = "No differences in fishery flags")
  ## no message when verbose = TRUE but there are differences
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(1L, 7L),   # flag changes 1 -> 7
    fishery_scale_factor     = 1.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = 0L,
    msf_quota                = 0L
  )
  fram_db2 <- make_compare_mock_db(fishery_scalers = scalers)
  expect_no_message(compare_fishery_input_flags(fram_db2, run_ids = c(1L, 2L), verbose = TRUE))

})


## --- compare_fishery_inputs() ------------------------------------------------

test_that("compare_fishery_inputs() errors on bad inputs", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_fishery_inputs(10, run_ids = c(1L, 2L), verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_fishery_inputs(fram_db, run_ids = 1, verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_fishery_inputs(fram_db, run_ids = c("a", "b"), verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_fishery_inputs(fram_db, run_ids = c(1, 2), verbose = "ten"),
               class = "framrsquared_error")
  expect_error(compare_fishery_inputs(fram_db, run_ids = c(1, 2), verbose = FALSE, tolerance = 1:4),
               class = "framrsquared_error")

})

test_that("compare_fishery_inputs() returns empty result when inputs are identical", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_inputs(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 0L)
})

test_that("compare_fishery_inputs() detects change above tolerance", {
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(2L, 2L),
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(1000L, 1200L),  # 20% change
    msf_quota                = 0L
  )
  fram_db <- make_compare_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_inputs(fram_db, run_ids = c(1L, 2L),
                                   tolerance = 0.1, verbose = FALSE)

  expect_gt(nrow(result), 0L)
  expect_true("quota" %in% result$parameter)
})

test_that("compare_fishery_inputs() suppresses changes below tolerance", {
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(2L, 2L),
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(1000L, 1005L),  # 0.5% change
    msf_quota                = 0L
  )
  fram_db <- make_compare_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_inputs(fram_db, run_ids = c(1L, 2L),
                                   tolerance = 0.01, verbose = FALSE)

  expect_equal(nrow(result), 0L)
})

test_that("compare_fishery_inputs() with tolerance = 0 flags any change", {
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(2L, 2L),
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(1000L, 1001L),  # tiny change
    msf_quota                = 0L
  )
  fram_db <- make_compare_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_inputs(fram_db, run_ids = c(1L, 2L),
                                   tolerance = 0, verbose = FALSE)

  expect_gt(nrow(result), 0L)
})

test_that("compare_fishery_inputs() errors when tolerance is out of range", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_fishery_inputs(fram_db, c(1L, 2L), tolerance = -0.1),
               class = "framrsquared_error")
  expect_error(compare_fishery_inputs(fram_db, c(1L, 2L), tolerance = 1.1),
               class = "framrsquared_error")
})

test_that("compare_fishery_inputs() computes prop_diff correctly", {
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(2L, 2L),
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(1000L, 1500L),  # 50% change
    msf_quota                = 0L
  )
  fram_db <- make_compare_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_inputs(fram_db, run_ids = c(1L, 2L),
                                   tolerance = 0, verbose = FALSE)

  quota_row <- result[result$parameter == "quota", ]
  expect_equal(quota_row$prop_diff, 0.5)
})

test_that("compare_fishery_inputs() captures changes in non-flagged category", {
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(2L, 2L),
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(1000L, 1000L),  # 50% change
    msf_quota                = c(200, 400)
  )
  fram_db <- make_compare_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_inputs(fram_db, run_ids = c(1L, 2L),
                                   tolerance = 0, verbose = FALSE)

  expect_equal(nrow(result), 1)
})

test_that("compare_fishery_inputs() ignores changes when only flag changes", {
  scalers <- data.frame(
    run_id                   = c(1L, 2L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    fishery_flag             = c(2L, 8L),
    fishery_scale_factor     = 0.0,
    msf_fishery_scale_factor = 0.0,
    quota                    = c(1000L, 1000L),  # 50% change
    msf_quota                = c(400, 400)
  )
  fram_db <- make_compare_mock_db(fishery_scalers = scalers)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_fishery_inputs(fram_db, run_ids = c(1L, 2L),
                                   tolerance = 0, verbose = FALSE)

  expect_equal(nrow(result), 0)
})





## --- compare_non_retention_inputs() ------------------------------------------

test_that("compare_non_retention_inputs() errors on bad inputs", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_error(compare_non_retention_inputs(10, run_ids = c(1L, 2L), verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_non_retention_inputs(fram_db, run_ids = 1, verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_non_retention_inputs(fram_db, run_ids = c("a", "b"), verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_non_retention_inputs(fram_db, run_ids = c(1, 2), verbose = "ten"),
               class = "framrsquared_error")
})


test_that("compare_non_retention_inputs() returns empty result when inputs are identical", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_non_retention_inputs(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 0L)
})

test_that("compare_non_retention_inputs() detects a changed CNR input", {
  nr <- data.frame(
    run_id             = c(1L, 2L),
    fishery_id         = c(1L, 1L),
    time_step          = c(1L, 1L),
    non_retention_flag = c(1L, 1L),
    cnr_input1         = 0L,
    cnr_input2         = 0L,
    cnr_input3         = c(100L, 150L),  # changed
    cnr_input4         = c(50L,  50L)
  )
  fram_db <- make_compare_mock_db(non_retention = nr)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_non_retention_inputs(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 1)
  expect_equal(result$original, 100)
  expect_equal(result$comparison, 150)
  expect_true("cnr_input3" %in% result$parameter)
})

test_that("compare_non_retention_inputs() doesn't respond to changes in flag", {
  nr <- data.frame(
    run_id             = c(1L, 2L),
    fishery_id         = c(1L, 1L),
    time_step          = c(1L, 1L),
    non_retention_flag = c(1L, 2L),
    cnr_input1         = 0L,
    cnr_input2         = 0L,
    cnr_input3         = c(100L, 100L),  # changed
    cnr_input4         = c(50L,  50L)
  )
  fram_db <- make_compare_mock_db(non_retention = nr)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_non_retention_inputs(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 0)
})

test_that("compare_non_retention_inputs() responds to changes ignored by flag", {
  nr <- data.frame(
    run_id             = c(1L, 2L),
    fishery_id         = c(1L, 1L),
    time_step          = c(1L, 1L),
    non_retention_flag = c(1L, 1L),
    cnr_input1         = c(10, 20),
    cnr_input2         = c(22, 11),
    cnr_input3         = c(100L, 140L),  # changed
    cnr_input4         = c(50L,  51L)
  )
  fram_db <- make_compare_mock_db(non_retention = nr)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_non_retention_inputs(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 4)
})

test_that("compare_non_retention_inputs() output has expected columns", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_non_retention_inputs(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_named(result,
               c("fishery_id", "fishery_label", "time_step", "parameter", "original", "comparison"),
               ignore.order = TRUE)
})


## --- compare_non_retention_input_flags() -------------------------------------


test_that("compare_non_retention_input_flags() errors on bad inputs", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_error(compare_non_retention_input_flags(10, run_ids = c(1L, 2L), verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_non_retention_input_flags(fram_db, run_ids = 1, verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_non_retention_input_flags(fram_db, run_ids = c("a", "b"), verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_non_retention_input_flags(fram_db, run_ids = c(1, 2), verbose = "ten"))
})

test_that("compare_non_retention_input_flags() returns empty result when flags are identical", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_non_retention_input_flags(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 0L)
})

test_that("compare_non_retention_input_flags() detects a changed flag", {
  nr <- data.frame(
    run_id             = c(1L, 2L),
    fishery_id         = c(1L, 1L),
    time_step          = c(1L, 1L),
    non_retention_flag = c(1L, 2L),   # flag changes 1 -> 2
    cnr_input1         = 0L,
    cnr_input2         = 0L,
    cnr_input3         = 100L,
    cnr_input4         = 50L
  )
  fram_db <- make_compare_mock_db(non_retention = nr)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_non_retention_input_flags(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 1L)
  expect_equal(result$flag_original,   1L)
  expect_equal(result$flag_comparison, 2L)
})


## --- compare_sl_ratio() ------------------------------------------------------

test_that("compare_sl_ratio() errors for bad inputs", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_sl_ratio(list(5), run_ids = c(1L, 2L)),
               class = "framrsquared_error")
  expect_error(compare_sl_ratio(fram_db, run_ids = c("A", "B")),
               class = "framrsquared_error")
})

test_that("compare_sl_ratio() errors for a non-Chinook database", {
  fram_db <- make_compare_mock_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_sl_ratio(fram_db, run_ids = c(1L, 2L)),
               class = "framrsquared_error")
})

test_that("compare_sl_ratio() returns empty result when SL ratios are identical", {
  sl <- data.frame(
    run_id                        = c(1L, 2L),
    fishery_id                    = c(1L, 1L),
    age                           = c(3L, 3L),
    time_step                     = c(1L, 1L),
    target_ratio                  = c(0.5, 0.5),
    run_encounter_rate_adjustment = c(1.0, 1.0)
  )
  fram_db <- make_compare_mock_db(sl_ratio = sl)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_sl_ratio(fram_db, run_ids = c(1L, 2L))

  expect_equal(nrow(result), 0L)
})

test_that("compare_sl_ratio() detects a changed target_ratio", {
  sl <- data.frame(
    run_id                        = c(1L, 2L),
    fishery_id                    = c(1L, 1L),
    age                           = c(3L, 3L),
    time_step                     = c(1L, 1L),
    target_ratio                  = c(0.5, 0.6),  # changed
    run_encounter_rate_adjustment = c(1.0, 1.0)
  )
  fram_db <- make_compare_mock_db(sl_ratio = sl)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_sl_ratio(fram_db, run_ids = c(1L, 2L))

  expect_equal(nrow(result), 1L)
  expect_equal(result$target_ratio_diff, 0.1)
})

test_that("compare_sl_ratio() flags rows present in only one run (na_mismatch)", {
  # fishery 2 present in run 1 only
  sl <- data.frame(
    run_id                        = c(1L, 1L, 2L),
    fishery_id                    = c(1L, 2L, 1L),
    age                           = c(3L, 3L, 3L),
    time_step                     = c(1L, 1L, 1L),
    target_ratio                  = c(0.5, 0.4, 0.5),
    run_encounter_rate_adjustment = c(1.0, 1.0, 1.0)
  )
  fram_db <- make_compare_mock_db(sl_ratio = sl)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_sl_ratio(fram_db, run_ids = c(1L, 2L))

  # fishery 2 should appear with NA in the comparison run
  expect_true(any(result$fishery_id == 2L))
  expect_true(is.na(result$target_ratio_comparison[result$fishery_id == 2L]))
})


## --- compare_recruits() -------------------------------------------------------

test_that("compare_recruits() errors on bad inputs", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_recruits(10, run_ids = c(1L, 2L), verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_recruits(fram_db, run_ids = 1, verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_recruits(fram_db, run_ids = c("a", "b"), verbose = FALSE),
               class = "framrsquared_error")
  expect_error(compare_recruits(fram_db, run_ids = c(1, 2), verbose = "ten"),
               class = "framrsquared_error")
  expect_error(compare_recruits(fram_db, run_ids = c(1, 2), verbose = FALSE, tolerance = 1:4),
               class = "framrsquared_error")

})

test_that("compare_recruits() errors when tolerance is out of range", {
  sr <- data.frame(run_id = c(1L, 2L), stock_id = c(1L, 1L),
                   age = c(3L, 3L), recruit_scale_factor = c(1.0, 1.0))
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 10000.0)
  fram_db <- make_compare_mock_db(stock_recruit = sr, base_cohort = bc)
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_recruits(fram_db, c(1L, 2L), tolerance = -0.1),
               class = "framrsquared_error")
  expect_error(compare_recruits(fram_db, c(1L, 2L), tolerance = 1.5),
               class = "framrsquared_error")
})

test_that("compare_recruits() returns empty result when recruit cohorts are identical", {
  sr <- data.frame(run_id = c(1L, 2L),
                   stock_id = c(1L, 1L),
                   age = c(3L, 3L),
                   recruit_scale_factor = c(1.0, 1.0))
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 10000.0)
  fram_db <- make_compare_mock_db(stock_recruit = sr, base_cohort = bc)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_recruits(fram_db, run_ids = c(1L, 2L), verbose = FALSE)

  expect_equal(nrow(result), 0L)
})

test_that("compare_recruits() detects a change above tolerance", {
  sr <- data.frame(run_id = c(1L, 2L), stock_id = c(1L, 1L),
                   age = c(3L, 3L), recruit_scale_factor = c(1.0, 1.2))  # 20% change
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 10000.0)
  fram_db <- make_compare_mock_db(stock_recruit = sr, base_cohort = bc)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_recruits(fram_db, run_ids = c(1L, 2L),
                             tolerance = 0.1, verbose = FALSE)

  expect_equal(nrow(result), 1L)
})

test_that("compare_recruits() computes recruit cohort from scale factor and base cohort", {
  sr <- data.frame(run_id = c(1L, 2L), stock_id = c(1L, 1L),
                   age = c(3L, 3L), recruit_scale_factor = c(1.0, 1.5))
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 10000.0)
  fram_db <- make_compare_mock_db(stock_recruit = sr, base_cohort = bc)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_recruits(fram_db, run_ids = c(1L, 2L),
                             tolerance = 0, verbose = FALSE)

  expect_equal(result$recruit_cohort_original,   10000)
  expect_equal(result$recruit_cohort_comparison, 15000)
  expect_equal(result$prop_diff, 0.5)
})

test_that("compare_recruits() suppresses changes below tolerance", {
  sr <- data.frame(run_id = c(1L, 2L), stock_id = c(1L, 1L),
                   age = c(3L, 3L), recruit_scale_factor = c(1.0, 1.005)) # 0.5% change
  bc <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                   base_cohort_size = 10000.0)
  fram_db <- make_compare_mock_db(stock_recruit = sr, base_cohort = bc)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_recruits(fram_db, run_ids = c(1L, 2L),
                             tolerance = 0.01, verbose = FALSE)

  expect_equal(nrow(result), 0L)
})


## --- compare_stock_fishery_rate_scalers() ------------------------------------

test_that("compare_stock_fishery_rate_scalers() errors on bad inputs", {
  fram_db <- make_compare_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_stock_fishery_rate_scalers(10, run_ids = c(1L, 2L)),
               class = "framrsquared_error")
  expect_error(compare_stock_fishery_rate_scalers(fram_db, run_ids = 1:3),
               class = "framrsquared_error")
  expect_error(compare_stock_fishery_rate_scalers(fram_db, run_ids = c("a", "bee")),
               class = "framrsquared_error")
})

test_that("compare_stock_fishery_rate_scalers() errors for a Chinook database", {
  fram_db <- make_compare_mock_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_stock_fishery_rate_scalers(fram_db, run_ids = c(1L, 2L)),
               class = "framrsquared_error")
})

test_that("compare_stock_fishery_rate_scalers() returns empty result when scalers are identical", {
  sfrs <- data.frame(
    run_id                   = c(1L, 2L),
    stock_id                 = c(1L, 1L),
    fishery_id               = c(1L, 1L),
    time_step                = c(1L, 1L),
    stock_fishery_rate_scaler = c(1.0, 1.0)
  )
  fram_db <- make_compare_mock_db(species = "COHO", sfrs = sfrs)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_stock_fishery_rate_scalers(fram_db, run_ids = c(1L, 2L))

  expect_equal(nrow(result), 0L)
})

test_that("compare_stock_fishery_rate_scalers() detects a changed scaler", {
  sfrs <- data.frame(
    run_id                    = c(1L, 2L),
    stock_id                  = c(1L, 1L),
    fishery_id                = c(1L, 1L),
    time_step                 = c(1L, 1L),
    stock_fishery_rate_scaler = c(1.0, 1.5)  # changed
  )
  fram_db <- make_compare_mock_db(species = "COHO", sfrs = sfrs)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- compare_stock_fishery_rate_scalers(fram_db, run_ids = c(1L, 2L))

  expect_equal(nrow(result), 1L)
  expect_equal(result$sfrs_original,   1.0)
  expect_equal(result$sfrs_comparison, 1.5)
})

test_that("compare_stock_fishery_rate_scalers() errors when a run_id is absent from the SFRS table", {
  sfrs <- data.frame(
    run_id                    = c(1L),  # run 2 is missing
    stock_id                  = 1L,
    fishery_id                = 1L,
    time_step                 = 1L,
    stock_fishery_rate_scaler = 1.0
  )
  fram_db <- make_compare_mock_db(species = "COHO", sfrs = sfrs)
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(compare_stock_fishery_rate_scalers(fram_db, run_ids = c(1L, 2L)),
               class = "framrsquared_error")
})


## --- compare_inputs_chart() --------------------------------------------------

test_that("compare_inputs_chart() errors on non-dataframe input", {
  expect_error(compare_inputs_chart(list(fishery_id = 1)),
               class = "framrsquared_error")
})

test_that("compare_inputs_chart() returns a ggplot object", {
  df <- data.frame(fishery_id   = c(1L, 2L),
                   time_step    = c(1L, 1L),
                   percent_diff = c(0.2, -0.1),
                   reg_change   = c(NA_character_, "NS->MSF"))
  result <- compare_inputs_chart(df)
  expect_s3_class(result, "ggplot")
})

test_that("compare_inputs_chart() converts Inf percent_diff to 1", {
  df <- data.frame(fishery_id   = 1L,
                   time_step    = 1L,
                   percent_diff = Inf,
                   reg_change   = NA_character_)
  # Should not error (Inf would break the plot otherwise)
  expect_no_error(compare_inputs_chart(df))
  result <- compare_inputs_chart(df)
  expect_true(result$data$percent_diff == 1)
})

test_that("compare_inputs_chart() converts NA percent_diff to 0", {
  df <- data.frame(fishery_id   = 1L,
                   time_step    = 1L,
                   percent_diff = NA_real_,
                   reg_change   = NA_character_)
  expect_no_error(compare_inputs_chart(df))
  result <- compare_inputs_chart(df)
  expect_true(result$data$percent_diff == 0)
})


# Integration test to make sure there are no errors for compare_runs() -----------------------------

## helper function to hide the output of compare_runs
quiet_run <- function(expr) {
  suppressMessages(capture_output(expr))
}

test_that("compare_runs() doesn't error on basic runs", {
  skip_if_no_test_db()

  fram_db <- connection_chin_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))


  expect_no_error(quiet_run(
    compare_runs(fram_db, c(139, 140))
  ))
  expect_error(quiet_run(
    compare_runs(fram_db, "ten")
  ),
  class = "framrsquared_error")

  fram_db_coho <- connection_coho_post(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db_coho))

  expect_no_error(quiet_run(
    compare_runs(fram_db_coho, c(34, 35))
  ))
})

test_that("compare_runs() doesn't accept identical run ids", {
  skip_if_no_test_db()

  fram_db <- connection_chin_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  expect_error(quiet_run(
    compare_runs(fram_db, c(139, 139))
  ))

})

test_that("compare_runs() produces appropriate list",{

  skip_if_no_test_db()

  fram_db <- connection_chin_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  junk <- quiet_run(result <- compare_runs(fram_db, c(139, 140)) )

  expect_equal(names(result), c("retention_flags", "retention_inputs", "sl_ratio", "recruits",
                                "fishery_flags", "fishery_inputs", "sfrs"))
  expect_true(is.null(result$sfrs))
  expect_true(!is.null(result$sl_ratio))

  fram_db_coho <- connection_coho_post(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db_coho))

  junk <- quiet_run(result <- compare_runs(fram_db_coho, c(34, 35))  )

  expect_equal(names(result), c("retention_flags", "retention_inputs", "sl_ratio", "recruits",
                                "fishery_flags", "fishery_inputs", "sfrs"))
  expect_true(!is.null(result$sfrs))
  expect_true(is.null(result$sl_ratio))

})


## copy a database, copy a run within it, and then check that comparisons
## of the identical runs produce empty comparison dataframes
test_that("compare_runs() gives correct results for identical runs",{
  skip_if_no_test_db()

  test_file <- paste0(db_test_path(), "/chin_pre_copytest.mdb")
  file.copy(from = paste0(db_test_path(), "/original_databases/chin_pre.mdb"),
            to = test_file)



  fram_db <- connect_fram_db(test_file, quiet = TRUE)
  withr::defer({
    disconnect_fram_db(fram_db)
    file.remove(test_file)
  })

  init_run = get_run_ids(fram_db)[1]
  new_run <- fram_db |>
    copy_run(target_run = init_run)


  junk <- quiet_run (result <-  compare_runs(fram_db, c(init_run, new_run)) )

  expect_equal(nrow(result$retention_flags), 0)
  expect_equal(nrow(result$retention_inputs), 0)
  expect_equal(nrow(result$sl_ratio), 0)
  expect_equal(nrow(result$recruits), 0)
  expect_equal(nrow(result$fishery_flags), 0)
  expect_equal(nrow(result$fishery_inputs), 0)

})


## compare_runs handles file saving correctly ---------------------------------

test_that("compare_runs saves output to file when save_file is provided", {
  tmp <- tempfile()
  withr::defer(unlink(tmp)) ## remove on exit

  fram_db <- connection_chin_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))


  compare_runs(fram_db, c(139, 140), save_file = tmp)


  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
})

test_that("compare_runs overwrites existing file", {
  tmp <- tempfile()
  withr::defer(unlink(tmp))

  fram_db <- connection_chin_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  compare_runs(fram_db, c(139, 140), save_file = tmp)
  size_first <- file.size(tmp)

  compare_runs(fram_db, c(139, 140), save_file = tmp)
  size_second <- file.size(tmp)

  expect_equal(size_first, size_second)
})

