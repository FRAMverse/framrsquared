# ── Helpers -------------------------------------------------------------------

# Builds a COHO mock DB with two runs, two fisheries, and two stocks
# (odd stock_id = unmarked, even stock_id = marked).
#
# stock_id is placed between run_id and time_step to satisfy the column-range
# selector "run_id":"time_step" used in msf_mortalities_coho_().
#
# Known values for run_id=1, fishery_id=10, time_step=2:
#
#   msf_mortalities_coho_():
#     unmarked = sum of all msf_* columns for stock_id=1
#              = 10 + 2 + 1 + 0.5 = 13.5
#     marked   = sum of all msf_* columns for stock_id=2
#              = 4  + 1 + 0.5 + 0.2 = 5.7
#
#   msf_encounters_coho_():
#     unmarked = msf_encounter (stock_id=1) = 12
#     marked   = msf_encounter (stock_id=2) = 5
#
#   msf_landed_catch_coho_():
#     unmarked = msf_landed_catch (stock_id=1) = 10
#     marked   = msf_landed_catch (stock_id=2) = 4

make_msf_coho_mock_db <- function(return_list = FALSE) {

  mortality <- tibble::tibble(
    primary_key       = 1:4L,
    run_id            = c(1L, 1L, 1L, 2L),
    stock_id          = c(1L,  2L,  1L,  1L),  # 1 = odd = unmarked, 2 = even = marked
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
    msf_shaker        = c(0,   0, 0,   0),
    msf_drop_off      = c(0.5, 0.2, 0,   0),
    msf_encounter     = c(12,  5,   5,   8)
  )

  table_list <- list(Mortality = mortality)

  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "COHO")
}


# ── Validate inputs: msf_mortalities_coho_() ----------------------------------

test_that("msf_mortalities_coho_() errors on an invalid `fram_db`", {
  expect_error(msf_mortalities_coho_("string"),  class = "framrsquared_error")
  expect_error(msf_mortalities_coho_(1),          class = "framrsquared_error")
  expect_error(msf_mortalities_coho_(list(1:5)),  class = "framrsquared_error")
})

test_that("msf_mortalities_coho_() does not error with a valid COHO db", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_mortalities_coho_(db))
})


# ── Validate inputs: msf_encounters_coho_() -----------------------------------

test_that("msf_encounters_coho_() errors on an invalid `fram_db`", {
  expect_error(msf_encounters_coho_("string"),  class = "framrsquared_error")
  expect_error(msf_encounters_coho_(1),          class = "framrsquared_error")
  expect_error(msf_encounters_coho_(list(1:5)),  class = "framrsquared_error")
})

test_that("msf_encounters_coho_() does not error with a valid COHO db", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_encounters_coho_(db))
})


# ── Validate inputs: msf_landed_catch_coho_() ---------------------------------

test_that("msf_landed_catch_coho_() errors on an invalid `fram_db`", {
  expect_error(msf_landed_catch_coho_("string"),  class = "framrsquared_error")
  expect_error(msf_landed_catch_coho_(1),          class = "framrsquared_error")
  expect_error(msf_landed_catch_coho_(list(1:5)),  class = "framrsquared_error")
})

test_that("msf_landed_catch_coho_() does not error with a valid COHO db", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_landed_catch_coho_(db))
})


# ── Output structure -----------------------------------------------------------

test_that("msf_mortalities_coho_() returns a tibble", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_s3_class(msf_mortalities_coho_(db), "tbl_df")
})

test_that("msf_mortalities_coho_() output contains expected key columns", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_coho_(db)
  expect_true(all(c("run_id", "fishery_id", "time_step",
                    "marked", "unmarked") %in% names(result)))
})

test_that("msf_encounters_coho_() returns a tibble", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_s3_class(msf_encounters_coho_(db), "tbl_df")
})

test_that("msf_encounters_coho_() output contains expected key columns", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_coho_(db)
  expect_true(all(c("run_id", "fishery_id", "time_step",
                    "marked", "unmarked") %in% names(result)))
})

test_that("msf_landed_catch_coho_() returns a tibble", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_s3_class(msf_landed_catch_coho_(db), "tbl_df")
})

test_that("msf_landed_catch_coho_() output contains expected key columns", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_coho_(db)
  expect_true(all(c("run_id", "fishery_id", "time_step",
                    "marked", "unmarked") %in% names(result)))
})


# ── Mark status logic ----------------------------------------------------------

test_that("msf_mortalities_coho_() correctly separates marked and unmarked using stock_id parity", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_coho_(db)
  row    <- result[result$run_id == 1L & result$fishery_id == 10L, ]

  # unmarked = stock_id=1 (odd): 10 + 2 + 0 + 0.5 = 13.5
  expect_equal(row$unmarked, 12.5)
  # marked = stock_id=2 (even): 4 + 1 + 0 + 0.2 = 5.7
  expect_equal(row$marked,   5.2)
})

test_that("msf_encounters_coho_() correctly separates marked and unmarked using stock_id parity", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_coho_(db)
  row    <- result[result$run_id == 1L & result$fishery_id == 10L, ]

  expect_equal(row$unmarked, 12)
  expect_equal(row$marked,   5)
})

test_that("msf_landed_catch_coho_() correctly separates marked and unmarked using stock_id parity", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_coho_(db)
  row    <- result[result$run_id == 1L & result$fishery_id == 10L, ]

  expect_equal(row$unmarked, 10)
  expect_equal(row$marked,   4)
})


# ── msf_mortalities_coho_() combines all MSF columns (no legal/sublegal split) -

test_that("msf_mortalities_coho_() does not produce sublegal columns", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_coho_(db)
  expect_false(any(c("sublegal_marked", "sublegal_unmarked",
                     "legal_marked",   "legal_unmarked") %in% names(result)))
})


# ── Aggregation across stocks --------------------------------------------------

test_that("msf_mortalities_coho_() aggregates across stocks (no stock_id in output)", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_coho_(db)
  expect_false("stock_id" %in% names(result))

  # run_id=1, fishery_id=10 has two stock rows — collapsed to one
  rows <- result[result$run_id == 1L & result$fishery_id == 10L, ]
  expect_equal(nrow(rows), 1L)
})

test_that("msf_encounters_coho_() aggregates across stocks (no stock_id in output)", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_coho_(db)
  expect_false("stock_id" %in% names(result))

  rows <- result[result$run_id == 1L & result$fishery_id == 10L, ]
  expect_equal(nrow(rows), 1L)
})

test_that("msf_landed_catch_coho_() aggregates across stocks (no stock_id in output)", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_coho_(db)
  expect_false("stock_id" %in% names(result))

  rows <- result[result$run_id == 1L & result$fishery_id == 10L, ]
  expect_equal(nrow(rows), 1L)
})


# ── Row count -----------------------------------------------------------------

test_that("msf_mortalities_coho_() returns one row per (run_id, fishery_id, time_step)", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_coho_(db)
  # mock has 3 distinct (run_id, fishery_id, time_step) combinations
  expect_equal(nrow(result), 3L)
})

test_that("msf_encounters_coho_() returns one row per (run_id, fishery_id, time_step)", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_coho_(db)
  expect_equal(nrow(result), 3L)
})

test_that("msf_landed_catch_coho_() returns one row per (run_id, fishery_id, time_step)", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_coho_(db)
  expect_equal(nrow(result), 3L)
})


# ── Zero-MSF fishery -----------------------------------------------------------

test_that("msf_mortalities_coho_() handles a fishery with all-zero MSF values", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_coho_(db)
  # fishery 20: only stock_id=1 (unmarked), all MSF values = 0 except msf_landed_catch=5
  row <- result[result$run_id == 1L & result$fishery_id == 20L, ]
  expect_equal(row$unmarked, 5.0)
})


# ── Species attribute ----------------------------------------------------------

test_that("msf_mortalities_coho_() attaches a COHO species attribute", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_coho_(db)
  expect_equal(attr(result, "species"), "COHO")
})

test_that("msf_encounters_coho_() attaches a COHO species attribute", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_coho_(db)
  expect_equal(attr(result, "species"), "COHO")
})

test_that("msf_landed_catch_coho_() attaches a COHO species attribute", {
  db <- make_msf_coho_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_coho_(db)
  expect_equal(attr(result, "species"), "COHO")
})

