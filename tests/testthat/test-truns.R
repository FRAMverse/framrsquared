## truns.R tests

## Helpers ------------------------------------------------------------------

# Minimal COHO mock DB with a ReportDriver table containing two PSCTRuns.DRV
# rows (each with comma-separated IDs) and one unrelated driver row.
#
# Row 1 (PSCTRuns.DRV): stock_ids "1,2"  fishery_ids "10,20"  name "TRun A"
# Row 2 (PSCTRuns.DRV): stock_ids "3"    fishery_ids "30"      name "TRun B"
# Row 3 (OTHER.DRV):    stock_ids "99"   fishery_ids "99"      name "Other"
make_truns_db <- function(return_list = FALSE) {
  report_driver <- tibble::tibble(
    driver_name = c("PSCTRuns.DRV", "PSCTRuns.DRV", "OTHER.DRV"),
    option1     = c("1,2",          "3",             "99"),
    option2     = c("10,20",        "30",            "99"),
    option5     = c("TRun A",       "TRun B",        "Other")
  )
  table_list <- list(ReportDriver = report_driver)
  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "COHO")
}

# Variant with no PSCTRuns.DRV rows at all.
make_truns_db_no_psc <- function(return_list = FALSE) {
  report_driver <- tibble::tibble(
    driver_name = "OTHER.DRV",
    option1     = "99",
    option2     = "99",
    option5     = "Other"
  )
  table_list <- list(ReportDriver = report_driver)
  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "COHO")
}


## truns_stocks() -----------------------------------------------------------

### input validation ---------------------------------------------------------

test_that("truns_stocks() errors for invalid fram_db", {
  expect_error(
    truns_stocks(list()),
    class = "framrsquared_error"
  )
})

test_that("truns_stocks() errors for non-COHO database", {
  db <- make_queryable_mock_db_list(
    table_list = make_truns_db(return_list = TRUE),
    species    = "CHINOOK"
  )
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(truns_stocks(db), class = "framrsquared_error")
})

test_that("truns_stocks() errors for non-full (transfer) database", {
  db <- make_queryable_mock_db_list(
    table_list = make_truns_db(return_list = TRUE),
    species    = "COHO",
    type       = "transfer"
  )
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(truns_stocks(db), class = "framrsquared_error")
})


### behavior -----------------------------------------------------------------

test_that("truns_stocks() returns a tibble", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_stocks(db)

  expect_s3_class(result, "tbl_df")
})

test_that("truns_stocks() returns expected columns", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_stocks(db)

  expect_named(result, c("stock_id", "stock_name"))
})

test_that("truns_stocks() stock_id column is numeric", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_stocks(db)

  expect_type(result$stock_id, "double")
})

test_that("truns_stocks() expands comma-separated stock IDs into multiple rows", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_stocks(db)

  # option1 = "1,2" for TRun A should expand to rows for stock IDs 1 and 2.
  expect_true(1 %in% result$stock_id)
  expect_true(2 %in% result$stock_id)
})

test_that("truns_stocks() returns one row per stock ID after expansion", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_stocks(db)

  # TRun A: "1,2" → 2 rows; TRun B: "3" → 1 row → 3 total PSCTRuns rows.
  # OTHER.DRV row must be excluded.
  expect_equal(nrow(result), 3L)
})

test_that("truns_stocks() excludes rows from other driver names", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_stocks(db)

  expect_false(99 %in% result$stock_id)
})

test_that("truns_stocks() stock_name reflects the TRun group name, not individual stock", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_stocks(db)

  # Both stock IDs from TRun A (1 and 2) should share the same stock_name.
  trun_a_names <- result |>
    dplyr::filter(.data$stock_id %in% c(1, 2)) |>
    dplyr::pull(.data$stock_name) |>
    unique()

  expect_equal(trun_a_names, "TRun A")
})

test_that("truns_stocks() returns zero rows when no PSCTRuns.DRV entries exist", {
  db <- make_truns_db_no_psc()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_stocks(db)

  expect_equal(nrow(result), 0L)
})


## truns_fisheries() --------------------------------------------------------

### input validation ---------------------------------------------------------

test_that("truns_fisheries() errors for invalid fram_db", {
  expect_error(
    truns_fisheries(list()),
    class = "framrsquared_error"
  )
})

test_that("truns_fisheries() errors for non-COHO database", {
  db <- make_queryable_mock_db_list(
    table_list = make_truns_db(return_list = TRUE),
    species    = "CHINOOK"
  )
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(truns_fisheries(db), class = "framrsquared_error")
})

test_that("truns_fisheries() errors for non-full (transfer) database", {
  db <- make_queryable_mock_db_list(
    table_list = make_truns_db(return_list = TRUE),
    species    = "COHO",
    type       = "transfer"
  )
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(truns_fisheries(db), class = "framrsquared_error")
})


### behavior -----------------------------------------------------------------

test_that("truns_fisheries() returns a tibble", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_fisheries(db)

  expect_s3_class(result, "tbl_df")
})

test_that("truns_fisheries() returns expected columns", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_fisheries(db)

  expect_named(result, c("fishery_id", "stock_name"))
})

test_that("truns_fisheries() fishery_id column is numeric", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_fisheries(db)

  expect_type(result$fishery_id, "double")
})

test_that("truns_fisheries() expands comma-separated fishery IDs into multiple rows", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_fisheries(db)

  # option2 = "10,20" for TRun A should expand to rows for fishery IDs 10 and 20.
  expect_true(10 %in% result$fishery_id)
  expect_true(20 %in% result$fishery_id)
})

test_that("truns_fisheries() returns one row per fishery ID after expansion", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_fisheries(db)

  # TRun A: "10,20" → 2 rows; TRun B: "30" → 1 row → 3 total.
  expect_equal(nrow(result), 3L)
})

test_that("truns_fisheries() excludes rows from other driver names", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_fisheries(db)

  expect_false(99 %in% result$fishery_id)
})

test_that("truns_fisheries() stock_name reflects the TRun group name", {
  db <- make_truns_db()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_fisheries(db)

  trun_a_names <- result |>
    dplyr::filter(.data$fishery_id %in% c(10, 20)) |>
    dplyr::pull(.data$stock_name) |>
    unique()

  expect_equal(trun_a_names, "TRun A")
})

test_that("truns_fisheries() returns zero rows when no PSCTRuns.DRV entries exist", {
  db <- make_truns_db_no_psc()
  withr::defer(disconnect_mock_fram_db(db))

  result <- truns_fisheries(db)

  expect_equal(nrow(result), 0L)
})
