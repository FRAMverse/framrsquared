## stock_comp.R tests

## Helpers ------------------------------------------------------------------

# Minimal mock Coho DB with two stocks (IDs 1 and 2) in a single fishery.
# Both stock IDs exist in the internal coho_stock_comp_lut (both map to
# "North Puget Sound").
#
# Mortality breakdown (run_id = 1, fishery_id = 10, time_step = 1):
#   stock 1 (odd  → Unmarked): landed_catch = 100, all others = 0 → total_mort = 100
#   stock 2 (even → Marked):   landed_catch = 1,   all others = 0 → total_mort = 1
#   total mortality = 101
#   ts stock 1 = 100/101 ≈ 0.990 (> default 1% threshold → kept)
#   ts stock 2 =   1/101 ≈ 0.0099 (< default 1% threshold → grouped)
make_stock_comp_db <- function(return_list = FALSE) {
  mortality <- tibble::tibble(
    primary_key       = 1:2,
    run_id            = c(1L, 1L),
    fishery_id        = c(10L, 10L),
    time_step         = c(1L, 1L),
    stock_id          = c(1L, 2L),
    age               = c(3L, 3L),
    landed_catch      = c(100, 1),
    non_retention     = c(0, 0),
    shaker            = c(0, 0),
    drop_off          = c(0, 0),
    encounter         = c(100, 1),
    msf_landed_catch  = c(0, 0),
    msf_non_retention = c(0, 0),
    msf_shaker        = c(0, 0),
    msf_drop_off      = c(0, 0),
    msf_encounter     = c(0, 0)
  )
  run_id_tbl <- tibble::tibble(
    primary_key  = 1L,
    run_id       = 1L,
    base_period_id = 1,
    run_name     = "TestRun",
    run_comments = ""
  )
  fishery_tbl <- tibble::tibble(
    fishery_id   = 10L,
    version_number = 1,
    fishery_name = "Tst Fish",
    fishery_title = "Test Fishery"
  )
  base_id <- tibble::tibble(
    base_period_id = 1,
    fishery_version = 1
  )
  stock_tbl <- tibble::tibble(
    stock_id        = c(1L, 2L),
    stock_long_name = c("Test Stock 1", "Test Stock 2")
  )

  table_list <- list(
    Mortality = mortality,
    RunID     = run_id_tbl,
    Fishery   = fishery_tbl,
    Stock     = stock_tbl,
    BaseID = base_id
  )
  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "COHO")
}


## calculate_stock_comp() ---------------------------------------------------

### input validation ---------------------------------------------------------

test_that("calculate_stock_comp() errors for invalid fram_db", {
  expect_error(
    calculate_stock_comp(list(),
                         run_id          = 1L,
                         fishery_id      = 10L,
                         time_step       = 1L,
                         group_threshold = 0),
    class = "framrsquared_error"
  )
})

test_that("calculate_stock_comp() errors for non-existent run_id", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    calculate_stock_comp(fram_db,
                         run_id          = 999L,
                         fishery_id      = 10L,
                         time_step       = 1L,
                         group_threshold = 0),
    class = "framrsquared_error"
  )
})

test_that("calculate_stock_comp() errors for non-existent fishery_id", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    calculate_stock_comp(fram_db,
                         run_id          = 1L,
                         fishery_id      = 999L,
                         time_step       = 1L,
                         group_threshold = 0),
    class = "framrsquared_error"
  )
})

test_that("calculate_stock_comp() errors for non-numeric time_step", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    calculate_stock_comp(fram_db,
                         run_id          = 1L,
                         fishery_id      = 10L,
                         time_step       = "one",
                         group_threshold = 0),
    class = "framrsquared_error"
  )
})

test_that("calculate_stock_comp() errors for time_step out of range (0 and 6)", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    calculate_stock_comp(fram_db, run_id = 1L, fishery_id = 10L, time_step = 0L),
    class = "framrsquared_error"
  )
  expect_error(
    calculate_stock_comp(fram_db, run_id = 1L, fishery_id = 10L, time_step = 6L),
    class = "framrsquared_error"
  )
})

test_that("calculate_stock_comp() errors for non-numeric group_threshold", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    calculate_stock_comp(fram_db,
                         run_id          = 1L,
                         fishery_id      = 10L,
                         time_step       = 1L,
                         group_threshold = "high"),
    class = "framrsquared_error"
  )
})


### behavior -----------------------------------------------------------------

test_that("calculate_stock_comp() returns a tibble with expected columns", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- calculate_stock_comp(fram_db,
                                  run_id     = 1L,
                                  fishery_id = 10L,
                                  time_step  = 1L,
                                  group_threshold = 0)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("run_id", "age", "fishery_id", "time_step",
                     "stock_long_name", "mark", "total_mort", "ts", "total")
                   %in% names(result)))
})

test_that("calculate_stock_comp() ts values sum to 1", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- calculate_stock_comp(fram_db,
                                  run_id          = 1L,
                                  fishery_id      = 10L,
                                  time_step       = 1L,
                                  group_threshold = 0)

  expect_equal(sum(result$ts), 1, tolerance = 1e-9)
})

test_that("calculate_stock_comp() assigns 'Unmarked' to odd stock IDs and 'Marked' to even", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- calculate_stock_comp(fram_db,
                                  run_id          = 1L,
                                  fishery_id      = 10L,
                                  time_step       = 1L,
                                  group_threshold = 0)

  # stock_id 1 (odd) → Unmarked; stock_id 2 (even) → Marked
  stock1_mark <- result |>
    dplyr::filter(.data$stock_long_name == "Test Stock 1") |>
    dplyr::pull(.data$mark)
  stock2_mark <- result |>
    dplyr::filter(.data$stock_long_name == "Test Stock 2") |>
    dplyr::pull(.data$mark)

  expect_equal(stock1_mark, "Unmarked")
  expect_equal(stock2_mark, "Marked")
})

test_that("calculate_stock_comp() preserves individual stock names when group_threshold = 0", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- calculate_stock_comp(fram_db,
                                  run_id          = 1L,
                                  fishery_id      = 10L,
                                  time_step       = 1L,
                                  group_threshold = 0)

  expect_true("Test Stock 1" %in% result$stock_long_name)
  expect_true("Test Stock 2" %in% result$stock_long_name)
})

test_that("calculate_stock_comp() groups low-frequency stocks with default threshold", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # ts for stock 2 ≈ 1/101 ≈ 0.0099 < default group_threshold of 0.01
  result <- calculate_stock_comp(fram_db,
                                  run_id     = 1L,
                                  fishery_id = 10L,
                                  time_step  = 1L)

  # "Test Stock 2" should be replaced by its coho_stock_comp_lut stock_group
  expect_false("Test Stock 2" %in% result$stock_long_name)
  # "Test Stock 1" should be unaffected (ts ≈ 0.99 > 0.01)
  expect_true("Test Stock 1" %in% result$stock_long_name)
})

test_that("calculate_stock_comp() groups all stocks when group_threshold = 1", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- calculate_stock_comp(fram_db,
                                  run_id          = 1L,
                                  fishery_id      = 10L,
                                  time_step       = 1L,
                                  group_threshold = 1)

  # Both stocks have ts < 1; all individual names should be replaced
  expect_false("Test Stock 1" %in% result$stock_long_name)
  expect_false("Test Stock 2" %in% result$stock_long_name)
})


## plot_stock_comp() --------------------------------------------------------

test_that("plot_stock_comp() returns a ggplot object", {
  fram_db <- make_stock_comp_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- plot_stock_comp(fram_db,
                             run_id          = 1L,
                             fishery_id      = 10L,
                             time_step       = 1L,
                             group_threshold = 0)

  expect_s3_class(result, "ggplot")
})
