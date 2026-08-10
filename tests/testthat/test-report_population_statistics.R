# ── Helpers -------------------------------------------------------------------

# Builds a mock DB for population_statistics() with two run IDs, two stocks,
# and two time steps.
#
# Cohort / Escapement known values (run_id = 1):
#
#   stock_id = 10, time_step = 1:
#     starting_cohort = 1000, post_nat_mort = 950, post_pre_terminal = 900,
#     maturation = 100, escapement = 800
#
#   stock_id = 10, time_step = 2:
#     starting_cohort = 500, post_nat_mort = 480, post_pre_terminal = 450,
#     maturation = 50, escapement = 400
#
#   stock_id = 20, time_step = 1:
#     starting_cohort = 2000, post_nat_mort = 1900, post_pre_terminal = 1800,
#     maturation = 200, escapement = NA -> replaced with 0
#     (no matching row in Escapement table — tests NA replacement)
#
# run_id = 2:
#   stock_id = 10, time_step = 1:
#     starting_cohort = 1200, post_nat_mort = 1100, post_pre_terminal = 1000,
#     maturation = 120, escapement = 900

make_pop_stat_mock_db <- function(return_list = FALSE) {

  cohort <- tibble::tibble(
    run_id         = c(1L,    1L,    1L,    2L),
    stock_id       = c(10L,   10L,   20L,   10L),
    age            = c(3L,    3L,    3L,    3L),
    time_step      = c(1L,    2L,    1L,    1L),
    start_cohort   = c(1000,  500,   2000,  1200),
    working_cohort = c(950,   480,   1900,  1100),
    cohort         = c(900,   450,   1800,  1000),
    mature_cohort  = c(100,   50,    200,   120)
  )

  # stock_id=20, time_step=1 is intentionally absent to trigger NA -> 0
  escapement <- tibble::tibble(
    primary_key = c(1L,  2L,  3L),
    run_id      = c(1L,  1L,  2L),
    stock_id    = c(10L, 10L, 10L),
    age         = c(3L,  3L,  3L),
    time_step   = c(1L,  2L,  1L),
    escapement  = c(800, 400, 900)
  )

  run_id_tbl <- tibble::tibble(
    run_id         = c(1L, 2L),
    base_period_id = c(2L, 2L)
  )

  table_list <- list(
    RunID      = run_id_tbl,
    Cohort     = cohort,
    Escapement = escapement
  )

  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "CHINOOK")
}


# ── Validate inputs ------------------------------------------------------------

test_that("population_statistics() errors on an invalid `fram_db`", {
  expect_error(population_statistics("string"),  class = "framrsquared_error")
  expect_error(population_statistics(1),          class = "framrsquared_error")
  expect_error(population_statistics(list(1:5)),  class = "framrsquared_error")
})

test_that("population_statistics() errors when `run_id` is not in the database", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(population_statistics(db, run_id = 99L), class = "framrsquared_error")
})

test_that("population_statistics() does not error with a valid db and no run_id", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(population_statistics(db))
})

test_that("population_statistics() does not error with a valid run_id", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(population_statistics(db, run_id = 1L))
})


# ── Output structure -----------------------------------------------------------

test_that("population_statistics() returns a tibble", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_s3_class(population_statistics(db), "tbl_df")
})

test_that("population_statistics() output contains all expected columns", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- population_statistics(db)
  expected_cols <- c("run_id", "stock_id", "age", "time_step",
                     "starting_cohort", "post_nat_mort", "post_pre_terminal",
                     "maturation", "escapement")
  expect_setequal(expected_cols,
                  names(result))
})


# ── run_id filtering -----------------------------------------------------------

test_that("population_statistics() with run_id = NULL returns all runs", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- population_statistics(db)
  expect_setequal(unique(result$run_id), c(1L, 2L))
  # cohort has 4 rows across both runs
  expect_equal(nrow(result), 4L)
})

test_that("population_statistics() filters to the requested run_id", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- population_statistics(db, run_id = 1L)
  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), 3L)
})

test_that("population_statistics() accepts a vector of run_ids", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- population_statistics(db, run_id = c(1L, 2L))
  expect_setequal(unique(result$run_id), c(1L, 2L))
})


# ── NA escapement replaced with 0 ---------------------------------------------

test_that("population_statistics() replaces NA escapement (no Escapement row) with 0", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- population_statistics(db, run_id = 1L)
  # stock_id=20 has no Escapement row -> should be 0, not NA
  row <- result[result$stock_id == 20L & result$time_step == 1L, ]
  expect_equal(row$escapement, 0)
  expect_false(is.na(row$escapement))
})

test_that("population_statistics() has no NA escapement values", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))
  result <- population_statistics(db)
  expect_false(any(is.na(result$escapement)))
})


# ── Numeric values -------------------------------------------------------------

test_that("population_statistics() returns correct cohort column values", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result  <- population_statistics(db, run_id = 1L)
  row     <- result[result$stock_id == 10L & result$time_step == 1L, ]

  expect_equal(row$starting_cohort,    1000)
  expect_equal(row$post_nat_mort,       950)
  expect_equal(row$post_pre_terminal,   900)
  expect_equal(row$maturation,          100)
  expect_equal(row$escapement,          800)
})


# ── Row ordering --------------------------------------------------------------

test_that("population_statistics() orders output by stock_id then time_step", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- population_statistics(db, run_id = 1L)

  # stock_id should be non-decreasing
  expect_true(all(diff(result$stock_id) >= 0))

  # within each stock, time_step should be non-decreasing
  for (sid in unique(result$stock_id)) {
    ts <- result$time_step[result$stock_id == sid]
    expect_true(all(diff(ts) >= 0))
  }
})

test_that("population_statistics() orders output by stock_id then time_step, real db", {
  skip_if_no_test_db()
  db <- connection_chin_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  result <- population_statistics(db, run_id = 139)

  # stock_id should be non-decreasing
  expect_true(all(diff(result$stock_id) >= 0))

  # within each stock, time_step should be non-decreasing
  for (sid in unique(result$stock_id)) {
    ts <- result$time_step[result$stock_id == sid]
    expect_true(all(diff(ts) >= 0))
  }
})

# -- Snapshots of behavior -------------------------------------------------------

test_that("population_statistics() output snapshot hasn't changed", {
  skip_if_no_test_db()
  db <- connection_chin_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  expect_snapshot_value(population_statistics(db),
                        style = "json2")

  suppressMessages(
    withr::deferred_run()
  )

  db <- connection_coho_post(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  expect_snapshot_value(population_statistics(db),
                        style = "json2")

})

# ── Species attribute ----------------------------------------------------------

test_that("population_statistics() attaches the correct species attribute", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- population_statistics(db)
  expect_equal(attr(result, "species"), "CHINOOK")
})

test_that("population_statistics() attaches species attribute when run_id is filtered", {
  db <- make_pop_stat_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- population_statistics(db, run_id = 1L)
  expect_equal(attr(result, "species"), "CHINOOK")
})
