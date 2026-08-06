# ── Helpers -------------------------------------------------------------------

# Builds a mock DB for stock_mortality() with two runs, two fisheries, and
# two stocks.
#
# Known aggregated values (NS + MSF combined) for run_id = 1, fishery_id = 10:
#
#   stock_id = 1, age = 3, time_step = 2:
#     landed_catch  = 10 + 4   = 14
#     non_retention =  2 + 0.5 =  2.5
#     shaker        =  1 + 0.3 =  1.3
#     drop_off      =  0.5 + 0.1 = 0.6
#
#   stock_id = 2, age = 3, time_step = 2 (all MSF = 0):
#     landed_catch  =  5
#     non_retention =  1
#     shaker        =  0.5
#     drop_off      =  0.3
#
# run_id = 1, fishery_id = 20, stock_id = 1 (NS only, all MSF = 0):
#     landed_catch  =  3
#
# run_id = 2, fishery_id = 10, stock_id = 1, time_step = 1 (NS only):
#     landed_catch  =  8

make_stock_mortality_mock_db <- function(species = "CHINOOK", return_list = FALSE) {

  mortality <- tibble::tibble(
    primary_key       = 1:4L,
    run_id            = c(1L,  1L,  1L,  2L),
    fishery_id        = c(10L, 10L, 20L, 10L),
    stock_id          = c(1L,  2L,  1L,  1L),
    age               = c(3L,  3L,  3L,  3L),
    time_step         = c(2L,  2L,  2L,  1L),
    landed_catch      = c(10.0, 5.0, 3.0, 8.0),
    non_retention     = c(2.0,  1.0, 0.0, 0.0),
    shaker            = c(1.0,  0.5, 0.0, 0.0),
    drop_off          = c(0.5,  0.3, 0.0, 0.0),
    encounter         = c(12.0, 6.0, 3.0, 9.0),
    msf_landed_catch  = c(4.0,  0.0, 0.0, 0.0),
    msf_non_retention = c(0.5,  0.0, 0.0, 0.0),
    msf_shaker        = c(0.3,  0.0, 0.0, 0.0),
    msf_drop_off      = c(0.1,  0.0, 0.0, 0.0),
    msf_encounter     = c(5.0,  0.0, 0.0, 0.0)
  )

  run_id_tbl <- tibble::tibble(
    run_id   = c(1L, 2L),
    run_name = c("Run A", "Run B")
  )

  stock_tbl <- tibble::tibble(
    stock_id   = c(1L, 2L),
    stock_name = c("Stock 1", "Stock 2"),
    species    = species
  )

  table_list <- list(
    Mortality = mortality,
    RunID     = run_id_tbl,
    Stock     = stock_tbl
  )

  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = species)
}


# ── Validate inputs ------------------------------------------------------------

test_that("stock_mortality() errors on an invalid `fram_db`", {
  expect_error(stock_mortality("string"), class = "framrsquared_error")
  expect_error(stock_mortality(1),        class = "framrsquared_error")
  expect_error(stock_mortality(list(1:5)), class = "framrsquared_error")
})

test_that("stock_mortality() does not error with a valid db and no filters", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(stock_mortality(db))
})

test_that("stock_mortality() errors when run_id is not in the database", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(stock_mortality(db, run_id = 999L), class = "framrsquared_error")
})

test_that("stock_mortality() errors when run_id is not numeric", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(stock_mortality(db, run_id = "one"), class = "framrsquared_error")
  expect_error(stock_mortality(db, run_id = TRUE),  class = "framrsquared_error")
})

test_that("stock_mortality() does not error with a valid run_id", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(stock_mortality(db, run_id = 1L))
})

test_that("stock_mortality() errors when stock_id is not in the database", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(stock_mortality(db, stock_id = 999L), class = "framrsquared_error")
})

test_that("stock_mortality() errors when stock_id is not numeric", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(stock_mortality(db, stock_id = "one"), class = "framrsquared_error")
  expect_error(stock_mortality(db, stock_id = TRUE),  class = "framrsquared_error")
})

test_that("stock_mortality() does not error with a valid stock_id", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(stock_mortality(db, stock_id = 1L))
})


# ── Output structure -----------------------------------------------------------

test_that("stock_mortality() returns a tibble", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_s3_class(stock_mortality(db), "tbl_df")
})

test_that("stock_mortality() returns exactly the expected columns in order", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- stock_mortality(db)
  expect_named(result, c("run_id", "fishery_id", "stock_id", "age", "time_step",
                         "landed_catch", "non_retention", "shaker", "drop_off"))
})


# ── NS + MSF aggregation -------------------------------------------------------

test_that("stock_mortality() sums NS and MSF for all mortality columns", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- stock_mortality(db)
  row <- result[result$run_id == 1L & result$fishery_id == 10L & result$stock_id == 1L, ]

  expect_equal(row$landed_catch,  14.0)
  expect_equal(row$non_retention,  2.5)
  expect_equal(row$shaker,         1.3)
  expect_equal(row$drop_off,       0.6)
})

test_that("stock_mortality() returns NS values unchanged when all MSF are zero", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- stock_mortality(db)
  row <- result[result$run_id == 1L & result$fishery_id == 10L & result$stock_id == 2L, ]

  expect_equal(row$landed_catch,   5.0)
  expect_equal(row$non_retention,  1.0)
  expect_equal(row$shaker,         0.5)
  expect_equal(row$drop_off,       0.3)
})


# ── Stock_id preserved in output -----------------------------------------------

test_that("stock_mortality() preserves stock_id (does not aggregate across stocks)", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- stock_mortality(db)
  expect_true("stock_id" %in% names(result))

  # Two stocks in run_id=1, fishery_id=10 → two rows
  rows <- result[result$run_id == 1L & result$fishery_id == 10L, ]
  expect_equal(nrow(rows), 2L)
})


# ── Row count -----------------------------------------------------------------

test_that("stock_mortality() returns all rows when no filters applied", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- stock_mortality(db)
  # 4 distinct (run_id, fishery_id, stock_id, age, time_step) combinations
  expect_equal(nrow(result), 4L)
})


# ── Filtering: run_id ----------------------------------------------------------

test_that("stock_mortality() filters to a single run_id", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- stock_mortality(db, run_id = 1L)
  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), 3L)
})

test_that("stock_mortality() filters to a vector of run_ids", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result_all  <- stock_mortality(db)
  result_both <- stock_mortality(db, run_id = c(1L, 2L))
  expect_equal(nrow(result_all), nrow(result_both))
})


# ── Filtering: stock_id --------------------------------------------------------

test_that("stock_mortality() filters to a single stock_id", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- stock_mortality(db, stock_id = 2L)
  expect_true(all(result$stock_id == 2L))
  expect_equal(nrow(result), 1L)
})

test_that("stock_mortality() run_id and stock_id filters can be combined", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- stock_mortality(db, run_id = 1L, stock_id = 1L)
  expect_true(all(result$run_id == 1L))
  expect_true(all(result$stock_id == 1L))
  expect_equal(nrow(result), 2L)
})


# ── Row ordering --------------------------------------------------------------

test_that("stock_mortality() output is ordered by run_id, fishery_id, age, time_step", {
  db <- make_stock_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- stock_mortality(db)
  expected_order <- order(result$run_id, result$fishery_id, result$age, result$time_step)
  expect_equal(seq_len(nrow(result)), expected_order)
})

test_that("stock_mortality() output is ordered by run_id, fishery_id, age, time_step, real db", {
  skip_if_no_test_db()

  db <- connection_coho_post(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  result <- stock_mortality(db, run_id = 34:36)
  expected_order <- order(result$run_id, result$fishery_id, result$age, result$time_step)
  expect_equal(seq_len(nrow(result)), expected_order)
})

# ── Species attribute ----------------------------------------------------------

test_that("stock_mortality() attaches species attribute matching the DB species", {
  db_chin <- make_stock_mortality_mock_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(db_chin))

  result <- stock_mortality(db_chin)
  expect_equal(attr(result, "species"), "CHINOOK")
})

test_that("stock_mortality() species attribute reflects COHO DB species", {
  db_coho <- make_stock_mortality_mock_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(db_coho))

  result <- stock_mortality(db_coho)
  expect_equal(attr(result, "species"), "COHO")
})


# ── Snapshots remain the same -------------------------------------------------

test_that("stock_mortality() snapshot hasn't changed (CHINOOK)", {
  skip_if_no_test_db()

  db <- connection_chin_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  expect_snapshot(stock_mortality(db))

  suppressMessages(
    withr::deferred_run()
  )

  db <- connection_chin_post(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  expect_snapshot(stock_mortality(db, run_id = 34:36))
})

test_that("stock_mortality() snapshot hasn't changed (COHO)", {
  skip_if_no_test_db()

  db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  expect_snapshot(stock_mortality(db))

  suppressMessages(
    withr::deferred_run()
  )

  db <- connection_coho_post(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  expect_snapshot(stock_mortality(db))
})
