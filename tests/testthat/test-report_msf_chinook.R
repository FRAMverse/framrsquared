# ── Helpers -------------------------------------------------------------------

# Builds a CHINOOK mock DB with two runs, two fisheries, and two stocks
# (odd stock_id = unmarked, even stock_id = marked), using MSP = 1 so that
# the raw Mortality values are preserved through msp_mortality().
#
# Known values for run_id=1, fishery_id=10, time_step=2:
#
#   msf_mortalities_chinook_():
#     legal_unmarked   = msf_landed_catch + msf_non_retention + msf_drop_off
#                      = 10 + 2 + 0.5  = 12.5  (stock 1, unmarked)
#     legal_marked     = 4  + 1 + 0.2  = 5.2   (stock 2, marked)
#     sublegal_unmarked = msf_shaker    = 1.0
#     sublegal_marked   = msf_shaker    = 0.5
#
#   msf_encounters_chinook_() [shaker_mort_rate = 0.5]:
#     legal_unmarked    = msf_encounter = 12
#     legal_marked      = msf_encounter = 5
#     sublegal_unmarked = msf_shaker / 0.5 = 1 / 0.5 = 2.0
#     sublegal_marked   = msf_shaker / 0.5 = 0.5 / 0.5 = 1.0
#
#   msf_landed_catch_chinook_():
#     legal_unmarked = msf_landed_catch = 10
#     legal_marked   = msf_landed_catch = 4

make_msf_chinook_mock_db <- function(return_list = FALSE) {

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
    msf_shaker        = c(1,   0.5, 0,   0),
    msf_drop_off      = c(0.5, 0.2, 0,   0),
    msf_encounter     = c(12,  5,   5,   8)
  )

  # PascalCase column names required for the raw SQL in msf_encounters_chinook_()
  run_id_tbl <- tibble::tibble(
    RunID        = c(1L, 2L),
    BasePeriodID = c(100L, 100L)
  )

  fmsp_tbl <- tibble::tibble(
    fishery_id             = c(10L, 20L),
    base_period_id         = c(100L, 100L),
    model_stock_proportion = c(1.0, 1.0)
  )

  # PascalCase column names required for the raw SQL in msf_encounters_chinook_()
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


# ── Validate inputs: msf_mortalities_chinook_() --------------------------------

test_that("msf_mortalities_chinook_() errors on an invalid `fram_db`", {
  expect_error(msf_mortalities_chinook_("string"),  class = "framrsquared_error")
  expect_error(msf_mortalities_chinook_(1),          class = "framrsquared_error")
  expect_error(msf_mortalities_chinook_(list(1:5)),  class = "framrsquared_error")
})

test_that("msf_mortalities_chinook_() errors when `fram_db` is not CHINOOK", {
  coho_db <- make_mock_fram_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(coho_db))

  expect_error(msf_mortalities_chinook_(coho_db), class = "framrsquared_error")
})

test_that("msf_mortalities_chinook_() does not error with a valid CHINOOK db", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_mortalities_chinook_(db))
})


# ── Validate inputs: msf_encounters_chinook_() --------------------------------

test_that("msf_encounters_chinook_() errors on an invalid `fram_db`", {
  expect_error(msf_encounters_chinook_("string"),  class = "framrsquared_error")
  expect_error(msf_encounters_chinook_(1),          class = "framrsquared_error")
  expect_error(msf_encounters_chinook_(list(1:5)),  class = "framrsquared_error")
})

test_that("msf_encounters_chinook_() errors when `fram_db` is not CHINOOK", {
  coho_db <- make_mock_fram_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(coho_db))

  expect_error(msf_encounters_chinook_(coho_db), class = "framrsquared_error")
})

test_that("msf_encounters_chinook_() does not error with a valid CHINOOK db", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_encounters_chinook_(db))
})


# ── Validate inputs: msf_landed_catch_chinook_() ------------------------------

test_that("msf_landed_catch_chinook_() errors on an invalid `fram_db`", {
  expect_error(msf_landed_catch_chinook_("string"),  class = "framrsquared_error")
  expect_error(msf_landed_catch_chinook_(1),          class = "framrsquared_error")
  expect_error(msf_landed_catch_chinook_(list(1:5)),  class = "framrsquared_error")
})

test_that("msf_landed_catch_chinook_() errors when `fram_db` is not CHINOOK", {
  coho_db <- make_mock_fram_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(coho_db))

  expect_error(msf_landed_catch_chinook_(coho_db), class = "framrsquared_error")
})

test_that("msf_landed_catch_chinook_() does not error with a valid CHINOOK db", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_no_error(msf_landed_catch_chinook_(db))
})


# ── Output structure -----------------------------------------------------------

test_that("msf_mortalities_chinook_() returns a tibble", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_s3_class(msf_mortalities_chinook_(db), "tbl_df")
})

test_that("msf_mortalities_chinook_() output contains expected key columns", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_chinook_(db)
  expect_true(all(c("run_id", "fishery_id", "time_step",
                    "legal_marked", "legal_unmarked",
                    "sublegal_marked", "sublegal_unmarked") %in% names(result)))
})

test_that("msf_encounters_chinook_() returns a tibble", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_s3_class(msf_encounters_chinook_(db), "tbl_df")
})

test_that("msf_encounters_chinook_() output contains expected key columns", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_chinook_(db)
  expect_true(all(c("run_id", "fishery_id", "time_step",
                    "legal_marked", "legal_unmarked",
                    "sublegal_marked", "sublegal_unmarked") %in% names(result)))
})

test_that("msf_landed_catch_chinook_() returns a tibble", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_s3_class(msf_landed_catch_chinook_(db), "tbl_df")
})

test_that("msf_landed_catch_chinook_() output contains expected key columns", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_chinook_(db)
  expect_true(all(c("run_id", "fishery_id", "time_step",
                    "legal_marked", "legal_unmarked") %in% names(result)))
})

test_that("msf_landed_catch_chinook_() output does not contain sublegal columns", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_chinook_(db)
  expect_false(any(c("sublegal_marked", "sublegal_unmarked") %in% names(result)))
})


# ── Mark status logic ----------------------------------------------------------

test_that("msf_mortalities_chinook_() correctly separates marked and unmarked using stock_id parity", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_chinook_(db)
  row    <- result[result$run_id == 1L & result$fishery_id == 10L, ]

  ## Looking at "legal" categories, so shakers are dropped.
  # marked = stock_id=2 (even): legal = 4 + 1 + 0.2 = 5.2
  expect_equal(row$legal_marked,    5.2)
  # unmarked = stock_id=1 (odd): legal = 10 + 2 + 0.5 = 12.5
  expect_equal(row$legal_unmarked, 12.5)
})

test_that("msf_encounters_chinook_() correctly separates marked and unmarked using stock_id parity", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_chinook_(db)
  row    <- result[result$run_id == 1L & result$fishery_id == 10L, ]

  expect_equal(row$legal_marked,   5)
  expect_equal(row$legal_unmarked, 12)
})

test_that("msf_landed_catch_chinook_() correctly separates marked and unmarked using stock_id parity", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_chinook_(db)
  row    <- result[result$run_id == 1L & result$fishery_id == 10L, ]

  expect_equal(row$legal_marked,   4)
  expect_equal(row$legal_unmarked, 10)
})


# ── Legality logic (sublegal = msf_shaker) ------------------------------------

test_that("msf_mortalities_chinook_() routes msf_shaker to sublegal and all other MSF columns to legal", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_chinook_(db)
  row    <- result[result$run_id == 1L & result$fishery_id == 10L, ]

  ## already checked legal-sized in tests above.
  expect_equal(row$sublegal_unmarked, 1.0)
  expect_equal(row$sublegal_marked,   0.5)
})

test_that("msf_mortalities_chinook_() produces zero sublegal when msf_shaker is zero", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_chinook_(db)
  row    <- result[result$run_id == 1L & result$fishery_id == 20L, ]

  # fishery 20 has only stock_id=1 (unmarked) with msf_shaker=0
  expect_equal(row$sublegal_unmarked, 0)
})


# ── Shaker-to-encounter conversion in msf_encounters_chinook_() ---------------

test_that("msf_encounters_chinook_() converts sublegal encounters using shaker_mort_rate", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_chinook_(db)
  row    <- result[result$run_id == 1L & result$fishery_id == 10L, ]

  # shaker_encounters = msf_shaker / shaker_mort_rate
  # unmarked: 1.0 / 0.5 = 2.0; marked: 0.5 / 0.5 = 1.0
  expect_equal(row$sublegal_unmarked, 2.0)
  expect_equal(row$sublegal_marked,   1.0)
})


# ── Aggregation across stocks --------------------------------------------------

test_that("msf_mortalities_chinook_() aggregates across stocks (no stock_id in output)", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_chinook_(db)
  expect_false("stock_id" %in% names(result))

  # run_id=1, fishery_id=10 has two stock rows — they should be collapsed to one
  rows <- result[result$run_id == 1L & result$fishery_id == 10L, ]
  expect_equal(nrow(rows), 1L)
})

test_that("msf_encounters_chinook_() aggregates across stocks (no stock_id in output)", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_chinook_(db)
  expect_false("stock_id" %in% names(result))

  rows <- result[result$run_id == 1L & result$fishery_id == 10L, ]
  expect_equal(nrow(rows), 1L)
})

test_that("msf_landed_catch_chinook_() aggregates across stocks (no stock_id in output)", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_chinook_(db)
  expect_false("stock_id" %in% names(result))

  rows <- result[result$run_id == 1L & result$fishery_id == 10L, ]
  expect_equal(nrow(rows), 1L)
})


# ── Row count -----------------------------------------------------------------

test_that("msf_mortalities_chinook_() returns one row per (run_id, fishery_id, time_step)", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_chinook_(db)
  # mock has 3 distinct (run_id, fishery_id, time_step) combos
  expect_equal(nrow(result), 3L)
})

test_that("msf_encounters_chinook_() returns one row per (run_id, fishery_id, time_step)", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_chinook_(db)
  expect_equal(nrow(result), 3L)
})

test_that("msf_landed_catch_chinook_() returns one row per (run_id, fishery_id, time_step)", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_chinook_(db)
  expect_equal(nrow(result), 3L)
})


# ── Species attribute ----------------------------------------------------------

test_that("msf_mortalities_chinook_() attaches a CHINOOK species attribute", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_mortalities_chinook_(db)
  expect_equal(attr(result, "species"), "CHINOOK")
})

test_that("msf_encounters_chinook_() attaches a CHINOOK species attribute", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_encounters_chinook_(db)
  expect_equal(attr(result, "species"), "CHINOOK")
})

test_that("msf_landed_catch_chinook_() attaches a CHINOOK species attribute", {
  db <- make_msf_chinook_mock_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- msf_landed_catch_chinook_(db)
  expect_equal(attr(result, "species"), "CHINOOK")
})
