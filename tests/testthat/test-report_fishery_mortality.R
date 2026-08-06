# ── Helpers -------------------------------------------------------------------

# Builds a mock DB with two runs, two fisheries, two stocks,
# providing known NS and MSF values for arithmetic checking.
#
# Expected aggregations for run_id=1, fishery_id=10, age=3, time_step=2
#   (two stock rows summed, then NS + MSF combined):
#   landed_catch  : (10 + 5)  + (4 + 2)   = 21
#   non_retention : (2  + 1)  + (0.5+0.2) = 3.7
#   shaker        : (1  + 0.5)+ (0.3+0.1) = 1.9
#   drop_off      : (0.5+0.3) + (0.1+0.05)= 0.95
make_fishery_mortality_mock_db <- function(species = "COHO", return_list = FALSE) {
  mortality <- tibble::tibble(
    primary_key       = 1:4L,
    run_id            = c(1L, 1L, 2L, 1L),
    fishery_id        = c(10L, 10L, 10L, 20L),
    stock_id          = c(1L,  2L,  1L,  1L),
    age               = c(3L,  3L,  3L,  3L),
    time_step         = c(2L,  2L,  2L,  2L),
    landed_catch      = c(10.0, 5.0, 8.0, 3.0),
    non_retention     = c(2.0,  1.0, 0.0, 0.5),
    shaker            = c(1.0,  0.5, 0.0, 0.2),
    drop_off          = c(0.5,  0.3, 0.0, 0.1),
    msf_landed_catch  = c(4.0,  2.0, 1.0, 0.0),
    msf_non_retention = c(0.5,  0.2, 0.0, 0.0),
    msf_shaker        = c(0.3,  0.1, 0.0, 0.0),
    msf_drop_off      = c(0.1,  0.05,0.0, 0.0)
  )

  run_id_tbl <- tibble::tibble(
    run_id   = c(1L, 2L),
    run_name = c("Run A", "Run B")
  )

  fishery_tbl <- tibble::tibble(
    fishery_id   = c(10L, 20L),
    fishery_name = c("Fishery 10", "Fishery 20"),
    species      = species
  )

  table_list <- list(
    Mortality = mortality,
    RunID     = run_id_tbl,
    Fishery   = fishery_tbl
  )
  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = species)
}

# -- Validate inputs ------------------------------------------------------------

test_that("fishery_mortality() errors on an invalid `fram_db`", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(fishery_mortality("ten"))
  expect_error(fishery_mortality(1))
  expect_error(fishery_mortality(list(1:5)))

  expect_no_error(db)

})

test_that("fishery_mortality() errors on an invalid run_id", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(fishery_mortality(db, run_id = "ten"))
  expect_error(fishery_mortality(db, run_id = TRUE))
  expect_error(fishery_mortality(db, run_id = list(1:5)))
  expect_error(fishery_mortality(db, run_id = 999))

  expect_no_error(fishery_mortality(db, run_id = 1))

})

test_that("fishery_mortality() errors on an invalid fishery_id", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(fishery_mortality(db, fishery_id = "ten"))
  expect_error(fishery_mortality(db, fishery_id = TRUE))
  expect_error(fishery_mortality(db, fishery_id = list(1:5)))
  expect_error(fishery_mortality(db, fishery_id = 999))

  expect_no_error(fishery_mortality(db, fishery_id = 10))

})

test_that("fishery_mortality() errors on invalid msp", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(fishery_mortality(db, msp = "ten"))
  expect_error(fishery_mortality(db, msp = 1))
  expect_error(fishery_mortality(db, msp = list(1:5)))
  expect_error(fishery_mortality(db, msp = c(TRUE, FALSE)))

  expect_no_error(fishery_mortality(db, msp = TRUE))

})



# ── Output structure -----------------------------------------------------------

test_that("fishery_mortality() returns a tibble", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db)
  expect_s3_class(result, "tbl_df")
})

test_that("fishery_mortality() returns exactly the expected columns in order", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db)
  expect_named(result, c("run_id", "fishery_id", "age", "time_step",
                         "landed_catch", "non_retention", "shaker", "drop_off"))
})

# ── Aggregation logic ---------------------------------------------------------

test_that("fishery_mortality() aggregates across stocks (no stock_id in grouping)", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db)

  # run_id=1, fishery_id=10 has two stock rows — they should be collapsed to one
  rows <- result[result$run_id == 1L & result$fishery_id == 10L, ]
  expect_equal(nrow(rows), 1L)
})

test_that("fishery_mortality() sums NS and MSF for all mortality columns", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db)
  row <- result[result$run_id == 1L & result$fishery_id == 10L, ]

  expect_equal(row$landed_catch, 21.0)
  expect_equal(row$non_retention, 3.7)
  expect_equal(row$shaker,        1.9)
  expect_equal(row$drop_off,      0.95)
})

test_that("fishery_mortality() with zero MSF values returns NS values unchanged", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db)

  # run_id=1, fishery_id=20: single stock, all MSF = 0
  row <- result[result$run_id == 1L & result$fishery_id == 20L, ]
  expect_equal(row$landed_catch,  3.0)
  expect_equal(row$non_retention, 0.5)
  expect_equal(row$shaker,        0.2)
  expect_equal(row$drop_off,      0.1)
})

# ── Filtering ------------------------------------------------------------------




test_that("fishery_mortality() returns all rows when run_id and fishery_id are NULL", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db)
  # 3 distinct (run_id, fishery_id) combinations in the mock
  expect_equal(nrow(result), 3L)
})

test_that("fishery_mortality() filters to a single run_id", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db, run_id = 1L)
  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), 2L)  # fishery 10 and fishery 20
})

test_that("fishery_mortality() filters to a vector of run_ids", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result_all  <- fishery_mortality(db)
  result_both <- fishery_mortality(db, run_id = c(1L, 2L))
  expect_equal(nrow(result_all), nrow(result_both))
})

test_that("fishery_mortality() filters to a single fishery_id", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db, fishery_id = 20L)
  expect_true(all(result$fishery_id == 20L))
  expect_equal(nrow(result), 1L)
})

test_that("fishery_mortality() run_id and fishery_id filters can be combined", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db, run_id = 1L, fishery_id = 10L)
  expect_equal(nrow(result), 1L)
  expect_equal(result$run_id,     1L)
  expect_equal(result$fishery_id, 10L)
})

test_that("fishery_mortality() correctly handles vector forms of run_id and fishery_id", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(fishery_mortality(db, run_id = 1:2, fishery_id = c(10, 20)))
  res <- fishery_mortality(db, run_id = 1:2, fishery_id = c(10, 20))

  ## has all runs
  expect_true(all(1:2 == sort(unique(res$run_id))))

  ## has all fishery ids
  expect_true(all(c(10, 20) == sort(unique(res$fishery_id))))

})


# ── Species attribute ----------------------------------------------------------

test_that("fishery_mortality() attaches species attribute matching the DB species", {
  db_coho <- make_fishery_mortality_mock_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(db_coho))

  result <- fishery_mortality(db_coho)
  expect_equal(attr(result, "species"), "COHO")
})

test_that("fishery_mortality() species attribute reflects CHINOOK DB species", {
  db_chin <- make_fishery_mortality_mock_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(db_chin))

  result <- fishery_mortality(db_chin)
  expect_equal(attr(result, "species"), "CHINOOK")
})

# ── Row ordering --------------------------------------------------------------

test_that("fishery_mortality() output is ordered by run_id, fishery_id, age, time_step", {
  db <- make_fishery_mortality_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- fishery_mortality(db)
  expected_order <- order(result$run_id, result$fishery_id, result$age, result$time_step)
  expect_equal(seq_len(nrow(result)), expected_order)
})

test_that("fishery_mortality() output is ordered by run_id, fishery_id, age, time_step, real db", {
  skip_if_no_test_db()

  db <- connection_chin_post(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  result <- fishery_mortality(db)
  expected_order <- order(result$run_id, result$fishery_id, result$age, result$time_step)
  expect_equal(seq_len(nrow(result)), expected_order)
})

# -- Snapshots remain constant-------------------


test_that("fishery_mortality() output is ordered by run_id, fishery_id, age, time_step, real db", {
  skip_if_no_test_db()

  db <- connection_chin_post(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  expect_snapshot(fishery_mortality(db))

  suppressMessages(withr::deferred_run())

  db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(db))

  expect_snapshot(fishery_mortality(db))

})

