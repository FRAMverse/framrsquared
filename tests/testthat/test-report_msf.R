# ── Helpers -------------------------------------------------------------------

# COHO mock: Mortality + RunID (needed for validate_run_id() in the wrappers).
# Two runs, two fisheries, two stocks (odd=unmarked, even=marked).
# stock_id sits between run_id and time_step to satisfy the column-range
# selector used in msf_mortalities_coho_().

make_msf_coho_db <- function(return_list = FALSE) {

  mortality <- tibble::tibble(
    primary_key       = 1:4L,
    run_id            = c(1L, 1L, 1L, 2L),
    stock_id          = c(1L,  2L,  1L,  1L),
    fishery_id        = c(10L, 10L, 20L, 10L),
    time_step         = c(2L,  2L,  2L,  2L),
    age               = c(3L,  3L,  3L,  3L),
    landed_catch      = c(100, 100, 100, 100),
    non_retention     = c(0,   0,   0,   0),
    shaker            = c(0,   0,   0,   0),
    drop_off          = c(0,   0,   0,   0),
    encounter         = c(12,  5,   5,   8),
    msf_landed_catch  = c(10,  4,   5,   8),
    msf_non_retention = c(2,   1,   0,   0),
    msf_shaker        = c(1,   0.5, 0,   0),
    msf_drop_off      = c(0.5, 0.2, 0,   0),
    msf_encounter     = c(12,  5,   5,   8)
  )

  run_id_tbl <- tibble::tibble(run_id = c(1L, 2L))

  table_list <- list(Mortality = mortality, RunID = run_id_tbl)

  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "COHO")
}

# CHINOOK mock: requires Mortality, RunID (PascalCase for raw SQL in
# msf_encounters_chinook_()), FisheryModelStockProportion, and ShakerMortRate.

make_msf_chinook_db <- function(return_list = FALSE) {

  mortality <- tibble::tibble(
    primary_key       = 1:4L,
    run_id            = c(1L, 1L, 1L, 2L),
    stock_id          = c(1L,  2L,  1L,  1L),
    fishery_id        = c(10L, 10L, 20L, 10L),
    time_step         = c(2L,  2L,  2L,  2L),
    age               = c(3L,  3L,  3L,  3L),
    landed_catch      = c(100, 100, 100, 100),
    non_retention     = c(0,   0,   0,   0),
    shaker            = c(0,   0,   0,   0),
    drop_off          = c(0,   0,   0,   0),
    encounter         = c(12,  5,   5,   8),
    msf_landed_catch  = c(10,  4,   5,   8),
    msf_non_retention = c(2,   1,   0,   0),
    msf_shaker        = c(1,   0.5, 0,   0),
    msf_drop_off      = c(0.5, 0.2, 0,   0),
    msf_encounter     = c(12,  5,   5,   8)
  )

  # PascalCase required for raw SQL in msf_encounters_chinook_()
  run_id_tbl <- tibble::tibble(
    RunID        = c(1L, 2L),
    BasePeriodID = c(100L, 100L)
  )

  fmsp_tbl <- tibble::tibble(
    fishery_id             = c(10L, 20L),
    base_period_id         = c(100L, 100L),
    model_stock_proportion = c(1.0, 1.0)
  )

  shaker_rate_tbl <- tibble::tibble(
    BasePeriodID   = c(100L, 100L),
    FisheryID      = c(10L,  20L),
    TimeStep       = c(2L,   2L),
    ShakerMortRate = c(0.5,  0.5)
  )

  table_list <- list(
    Mortality                   = mortality,
    RunID                       = run_id_tbl,
    FisheryModelStockProportion = fmsp_tbl,
    ShakerMortRate              = shaker_rate_tbl
  )

  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "CHINOOK")
}


# ── Validate inputs: msf_mortalities() ----------------------------------------

test_that("msf_mortalities() errors on an invalid `fram_db`", {
  expect_error(msf_mortalities("string"),  class = "framrsquared_error")
  expect_error(msf_mortalities(1),          class = "framrsquared_error")
  expect_error(msf_mortalities(list(1:5)),  class = "framrsquared_error")
})

test_that("msf_mortalities() errors on an invalid run_id", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(msf_mortalities(db, run_id = "ten"),    class = "framrsquared_error")
  expect_error(msf_mortalities(db, run_id = TRUE),     class = "framrsquared_error")
  expect_error(msf_mortalities(db, run_id = list(1)),  class = "framrsquared_error")
  expect_error(msf_mortalities(db, run_id = 999),      class = "framrsquared_error")
})

test_that("msf_mortalities() does not error with run_id = NULL (default)", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_mortalities(db))
})

test_that("msf_mortalities() does not error with a valid run_id", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_mortalities(db, run_id = 1L))
})


# ── Validate inputs: msf_encounters() -----------------------------------------

test_that("msf_encounters() errors on an invalid `fram_db`", {
  expect_error(msf_encounters("string"),  class = "framrsquared_error")
  expect_error(msf_encounters(1),          class = "framrsquared_error")
  expect_error(msf_encounters(list(1:5)),  class = "framrsquared_error")
})

test_that("msf_encounters() errors on an invalid run_id", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(msf_encounters(db, run_id = "ten"),    class = "framrsquared_error")
  expect_error(msf_encounters(db, run_id = TRUE),     class = "framrsquared_error")
  expect_error(msf_encounters(db, run_id = list(1)),  class = "framrsquared_error")
  expect_error(msf_encounters(db, run_id = 999),      class = "framrsquared_error")
})

test_that("msf_encounters() does not error with run_id = NULL (default)", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_encounters(db))
})

test_that("msf_encounters() does not error with a valid run_id", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_encounters(db, run_id = 1L))
})


# ── Validate inputs: msf_landed_catch() ---------------------------------------

test_that("msf_landed_catch() errors on an invalid `fram_db`", {
  expect_error(msf_landed_catch("string"),  class = "framrsquared_error")
  expect_error(msf_landed_catch(1),          class = "framrsquared_error")
  expect_error(msf_landed_catch(list(1:5)),  class = "framrsquared_error")
})

test_that("msf_landed_catch() errors on an invalid run_id", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(msf_landed_catch(db, run_id = "ten"),    class = "framrsquared_error")
  expect_error(msf_landed_catch(db, run_id = TRUE),     class = "framrsquared_error")
  expect_error(msf_landed_catch(db, run_id = list(1)),  class = "framrsquared_error")
  expect_error(msf_landed_catch(db, run_id = 999),      class = "framrsquared_error")
})

test_that("msf_landed_catch() does not error with run_id = NULL (default)", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_landed_catch(db))
})

test_that("msf_landed_catch() does not error with a valid run_id", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_landed_catch(db, run_id = 1L))
})


# ── Species dispatch -----------------------------------------------------------

test_that("msf_mortalities() dispatches to COHO function for a COHO db", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities(db)
  # COHO output has marked/unmarked columns, not legal_marked/legal_unmarked
  expect_true(all(c("marked", "unmarked") %in% names(result)))
  expect_false("legal_marked" %in% names(result))
})

test_that("msf_mortalities() dispatches to CHINOOK function for a CHINOOK db", {
  db <- make_msf_chinook_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities(db)
  # CHINOOK output has legal_marked/legal_unmarked/sublegal_* columns
  expect_true(all(c("legal_marked", "legal_unmarked") %in% names(result)))
  expect_false("marked" %in% names(result))
})

test_that("msf_encounters() dispatches to COHO function for a COHO db", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters(db)
  expect_true(all(c("marked", "unmarked") %in% names(result)))
  expect_false("legal_marked" %in% names(result))
})

test_that("msf_encounters() dispatches to CHINOOK function for a CHINOOK db", {
  db <- make_msf_chinook_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters(db)
  expect_true(all(c("legal_marked", "legal_unmarked") %in% names(result)))
  expect_false("marked" %in% names(result))
})

test_that("msf_landed_catch() dispatches to COHO function for a COHO db", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch(db)
  expect_true(all(c("marked", "unmarked") %in% names(result)))
  expect_false("legal_marked" %in% names(result))
})

test_that("msf_landed_catch() dispatches to CHINOOK function for a CHINOOK db", {
  db <- make_msf_chinook_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch(db)
  expect_true(all(c("legal_marked", "legal_unmarked") %in% names(result)))
  expect_false("marked" %in% names(result))
})


# ── run_id filtering ----------------------------------------------------------

test_that("msf_mortalities() returns all rows when run_id is NULL", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities(db)
  expect_equal(nrow(result), 3L)  # 3 distinct (run_id, fishery_id, time_step) combos
})

test_that("msf_mortalities() filters to the specified run_id", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities(db, run_id = 1L)
  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), 2L)  # fishery 10 and fishery 20
})

test_that("msf_mortalities() filters to a vector of run_ids", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result_all  <- msf_mortalities(db)
  result_both <- msf_mortalities(db, run_id = c(1L, 2L))
  expect_equal(nrow(result_all), nrow(result_both))
})

test_that("msf_encounters() returns all rows when run_id is NULL", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters(db)
  expect_equal(nrow(result), 3L)
})

test_that("msf_encounters() filters to the specified run_id", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters(db, run_id = 1L)
  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), 2L)
})

test_that("msf_landed_catch() returns all rows when run_id is NULL", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch(db)
  expect_equal(nrow(result), 3L)
})

test_that("msf_landed_catch() filters to the specified run_id", {
  db <- make_msf_coho_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch(db, run_id = 1L)
  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), 2L)
})

