# UNIT TESTS --------------------------------------------------------------

# --- Helpers ------------------------------------------------------------------

## Mock Coho DB for calculate_report_trs()
##
## Two stock groups defined via PSCTRuns.DRV report driver rows and typed via
## report_number = 2 rows:
##   GroupA: stocks = [1],   fisheries = [10, 20], type = TAA,  trs_id = 1
##   GroupB: stocks = [2],   fisheries = [10, 20], type = ETRS, trs_id = 2
##
## Mortality (only time_steps 4-5 contribute to TRS calculations):
##   ts=4, f=10, s=1: catch = 25  (landed=20, msf=5)
##   ts=4, f=10, s=2: catch = 30  (landed=30, msf=0)  <- not a GroupA stock
##   ts=5, f=20, s=1: catch = 15  (landed=10, msf=5)
##   ts=5, f=20, s=2: catch = 20  (landed=15, msf=5)  <- not a GroupA stock
##   ts=3, f=10, s=1: catch = 200 <- excluded (wrong time step)
##
## Escapement: stock_id 1 = 100, stock_id 2 = 50
##
## Expected results (run_id = 1):
##   GroupA TAA:  esc=100, catch_taa  = (25+30) + (15+20) = 90  -> TAA  = 190
##   GroupB ETRS: esc= 50, catch_etrs =     30  +    20   = 50  -> ETRS = 100

make_mock_coho_report_trs_db <- function(return_list = FALSE) {
  report_driver <- tibble::tibble(
    report_number = c(2, 2, 5, 5),
    species_name = rep("COHO", 4),
    driver_name   = c("PSCTRuns.DRV", "PSCTRuns.DRV", "JUNK", "JUNK"),
    option1       = c("1", "2", "3", "4"),
    option2       = c("10,20","10,20",NA_character_, NA_character_),
    option4       = c("TAA", "ETRS", "TAA",  "ETRS"),
    option5       = c("GroupA", "GroupB", "GroupAA", "GroupBB"),
    option6       = c("91", "92", "93",    "94")
  )

  escapement <- tibble::tibble(
    run_id     = c(1L, 1L),
    stock_id   = c(1L, 2L),
    escapement = c(100, 50)
  )

  mortality <- tibble::tibble(
    run_id            = c(1L,  1L,  1L,  1L,  1L),
    fishery_id        = c(10L, 10L, 20L, 20L, 10L),
    time_step         = c(4L,  4L,  5L,  5L,  3L),
    stock_id          = c(1L,  2L,  1L,  2L,  1L),
    landed_catch      = c(20,  30,  10,  15, 100),
    msf_landed_catch  = c(5,   0,   5,   5,  100),
    non_retention     = c(0,   0,   0,   0,   0),
    shaker            = c(0,   0,   0,   0,   0),
    drop_off          = c(0,   0,   0,   0,   0),
    msf_non_retention = c(0,   0,   0,   0,   0),
    msf_shaker        = c(0,   0,   0,   0,   0),
    msf_drop_off      = c(0,   0,   0,   0,   0)
  )

  table_list <- list(
    ReportDriver = report_driver,
    Escapement   = escapement,
    Mortality    = mortality,
    RunID        = tibble::tibble(run_id = 1L, base_period_id = 1L)
  )

  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "COHO")
}

## Testing error-catching: duplicated option 5 x option6 of report drivers (ERROR) and duplicate option 5
## but not option 6 (WARNING)
## To test for warning, provide trs id of c(91, 94) (skips the error-causing 92)

make_mock_broken_coho_report_trs_db <- function(return_list = FALSE) {
  report_driver <- tibble::tibble(
    report_number = c(2, 2, 2, 2),
    species_name = rep("COHO", 4),
    driver_name   = rep("PSCTRuns.DRV", 4),
    option1       = c("1", "2", "1", "2"),
    option2       = c("10,20", "10,20", "10,20", "10,20"),
    option4       = c("TAA", "ETRS", "TAA",  "ETRS"),
    option5       = c("GroupA", "GroupB", "GroupB", "GroupA"),
    option6       = c("91", "92", "92",    "94")
  )

  escapement <- tibble::tibble(
    run_id     = c(1L, 1L),
    stock_id   = c(1L, 2L),
    escapement = c(100, 50)
  )

  mortality <- tibble::tibble(
    run_id            = c(1L,  1L,  1L,  1L,  1L),
    fishery_id        = c(10L, 10L, 20L, 20L, 10L),
    time_step         = c(4L,  4L,  5L,  5L,  3L),
    stock_id          = c(1L,  2L,  1L,  2L,  1L),
    landed_catch      = c(20,  30,  10,  15, 100),
    msf_landed_catch  = c(5,   0,   5,   5,  100),
    non_retention     = c(0,   0,   0,   0,   0),
    shaker            = c(0,   0,   0,   0,   0),
    drop_off          = c(0,   0,   0,   0,   0),
    msf_non_retention = c(0,   0,   0,   0,   0),
    msf_shaker        = c(0,   0,   0,   0,   0),
    msf_drop_off      = c(0,   0,   0,   0,   0)
  )

  table_list <- list(
    ReportDriver = report_driver,
    Escapement   = escapement,
    Mortality    = mortality,
    RunID        = tibble::tibble(run_id = 1L, base_period_id = 1L)
  )

  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "COHO")
}

## Extended version with two run IDs for testing the many-to-many path.
make_mock_coho_report_trs_db_extended <- function(return_list = FALSE) {
  tables <- make_mock_coho_report_trs_db(return_list = TRUE)

  tables$Escapement <- dplyr::bind_rows(
    tables$Escapement,
    dplyr::mutate(tables$Escapement, run_id = 2L)
  )
  tables$Mortality <- dplyr::bind_rows(
    tables$Mortality,
    dplyr::mutate(tables$Mortality, run_id = 2L)
  )
  tables$RunID <- tibble::tibble(
    run_id         = c(1L, 2L),
    base_period_id = c(1L, 1L)
  )

  if (return_list) return(tables)
  make_queryable_mock_db_list(table_list = tables, species = "COHO")
}


## Mock Coho DB for calculate_tami_trs()
##
## Two groups from TAAETRSList:
##   taa_num=1, GroupA: stocks=[1], fisheries=[10,20], taa_type=1 (TAA)
##   taa_num=2, GroupB: stocks=[2], fisheries=[10,20], taa_type=0 (ETRS)
##
## Same escapement and mortality as report_trs mock (without the ts=3 row).
##
## Expected results (run_id = 1):
##   GroupA TAA:  esc=100, catch_taa  = (25+30) + (15+20) = 90  -> TAA  = 190
##   GroupB ETRS: esc= 50, catch_etrs =     30  +    20   = 50  -> ETRS = 100

make_mock_coho_tami_trs_db <- function(return_list = FALSE) {
  taaetrs_list <- tibble::tibble(
    taa_num        = c(1L,    2L),
    taa_name       = c("GroupA", "GroupB"),
    taa_stk_list   = c("1",   "2"),
    taa_fish_list  = c("10,20", "10,20"),
    taa_time_step1 = c(4L,    4L),
    taa_time_step2 = c(5L,    5L),
    taa_type       = c(1L,    0L)
  )

  escapement <- tibble::tibble(
    run_id     = c(1L, 1L),
    stock_id   = c(1L, 2L),
    escapement = c(100, 50)
  )

  mortality <- tibble::tibble(
    run_id            = c(1L,  1L,  1L,  1L, 1L, 1L),
    fishery_id        = c(10L, 10L, 20L, 20L, 30L, 30L),
    time_step         = c(4L,  4L,  5L,  5L, 4L, 5L),
    stock_id          = c(1L,  2L,  1L,  2L, 1L, 2L),
    landed_catch      = c(20,  30,  10,  15, 5, 10),
    msf_landed_catch  = c(5,   0,   5,   5, 8, 19),
    non_retention     = c(0,   0,   0,   0, 0, 0),
    shaker            = c(0,   0,   0,   0, 0, 0),
    drop_off          = c(0,   0,   0,   0, 0, 0),
    msf_non_retention = c(0,   0,   0,   0, 0, 0),
    msf_shaker        = c(0,   0,   0,   0, 0, 0),
    msf_drop_off      = c(1,   1,   1,   1, 1, 1) ## should not be included in maths
  )

  table_list <- list(
    TAAETRSList = taaetrs_list,
    Escapement  = escapement,
    Mortality   = mortality,
    RunID       = tibble::tibble(run_id = 1L, base_period_id = 1L)
  )

  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = "COHO")
}


# --- calculate_report_trs() ---------------------------------------------------

## Input validation ------------------------------------------------------------

test_that("calculate_report_trs() errors on a CHINOOK database", {
  # [requires Bug 1 fix: db_speces -> db_species in validate_fram_db() call]
  db <- make_mock_fram_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(
    calculate_report_trs(db, run_id = 1L),
    class = "framrsquared_error"
  )
})

test_that("calculate_report_trs() errors when run_id is non-numeric", {
  # [requires Bug 1 fix: db_speces -> db_species in validate_fram_db() call]
  db <- make_mock_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(
    calculate_report_trs(db, run_id = "1"),
    class = "framrsquared_error"
  )
  expect_error(
    calculate_report_trs(db, run_id = TRUE),
    class = "framrsquared_error"
  )
})

test_that("calculate_report_trs() errors when trs_definition_number is non-numeric", {
  # [requires Bug 1 fix: db_speces -> db_species in validate_fram_db() call]
  db <- make_mock_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(
    calculate_report_trs(db, run_id = 1L, trs_definition_number = "1"),
    class = "framrsquared_error"
  )
})


## Calculation logic -----------------------------------------------------------

test_that("calculate_report_trs() errors for duplicated ReportDrivers rows", {
  db <- make_mock_broken_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  ## trs id 92 has duplicated name x id
  expect_error(calculate_report_trs(db, run_id = 1L),
               class = "framrsquared_error",
               regexp = "GroupB")



})

test_that("calculate_report_trs() warns for duplicated ReportDrivers stock group names", {
  db <- make_mock_broken_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  expect_warning(calculate_report_trs(db, run_id = 1L, trs_definition_number = c(91, 94)),
                 regexp = "GroupA")
})


test_that("calculate_report_trs() excludes mortality outside time steps 4 and 5", {
  db <- make_mock_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_report_trs(db, run_id = 1L)

  # The ts=3 row has landed_catch=100 and msf_landed_catch=100 for stock 1 in
  # fishery 10. If it were included, GroupA's TAA would be > 190.
  groupA <- dplyr::filter(result, .data$taa_name == "GroupA")
  expect_equal(groupA$terminal_run_size, 190)
})

test_that("calculate_report_trs() TAA sums catch from all stocks in the TAA fisheries", {
  db <- make_mock_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_report_trs(db, run_id = 1L)

  # GroupA TAA: esc=100, catch from fisheries 10+20 across ALL stocks = 90
  # -> TAA = 190
  groupA <- dplyr::filter(result, .data$taa_name == "GroupA")
  expect_equal(groupA$escapement, 100)
  expect_equal(groupA$terminal_run_size, 190)
})

test_that("calculate_report_trs() ETRS sums only stock-specific catch from the defined fisheries", {
  db <- make_mock_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_report_trs(db, run_id = 1L)

  # GroupB ETRS: esc=50, catch for stock 2 only in fisheries 10+20 = 30+20 = 50
  # -> ETRS = 100 (not 140, which would result if all-stock catch were used)
  groupB <- dplyr::filter(result, .data$taa_name == "GroupB")
  expect_equal(groupB$escapement, 50)
  expect_equal(groupB$terminal_run_size, 100)
})

test_that("calculate_report_trs() terminal_run_type reflects the trun_type from ReportDriver", {
  db <- make_mock_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_report_trs(db, run_id = 1L)

  expect_equal(
    dplyr::filter(result, .data$taa_name == "GroupA")$terminal_run_type,
    "TAA"
  )
  expect_equal(
    dplyr::filter(result, .data$taa_name == "GroupB")$terminal_run_type,
    "ETRS"
  )
})


## Output structure ------------------------------------------------------------



test_that("calculate_report_trs() returns a tibble with expected columns", {
  db <- make_mock_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_report_trs(db, run_id = 1L)

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c("run_id", "taa_num", "taa_name", "escapement",
      "terminal_run_size", "terminal_run_type", "terminal_run_category")
    %in% names(result)
  ))
})

test_that("calculate_report_trs() trs_definition_number filters to the specified group(s)", {
  db <- make_mock_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_report_trs(db, run_id = 1L, trs_definition_number = 91)

  expect_equal(nrow(result), 1L)
  expect_equal(result$taa_num, 91)
})

test_that("calculate_report_trs() returns rows for all run IDs when multiple are supplied", {
  db <- make_mock_coho_report_trs_db_extended()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result_both   <- calculate_report_trs(db, run_id = c(1L, 2L))
  result_single <- calculate_report_trs(db, run_id = 1L)

  expect_equal(nrow(result_both), nrow(result_single) * 2L)
  expect_setequal(result_both$run_id, c(1L, 2L))
})


# --- calculate_tami_trs() -----------------------------------------------------

## Input validation ------------------------------------------------------------

test_that("calculate_tami_trs() errors on a CHINOOK database", {
  # [requires Bug 1 fix: db_speces -> db_species in validate_fram_db() call]
  db <- make_mock_fram_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(
    calculate_tami_trs(db, run_id = 1L),
    class = "framrsquared_error"
  )
})

test_that("calculate_tami_trs() errors when run_id is non-numeric", {
  # [requires Bug 1 fix: db_speces -> db_species in validate_fram_db() call]
  db <- make_mock_coho_tami_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(
    calculate_tami_trs(db, run_id = "1"),
    class = "framrsquared_error"
  )
})

test_that("calculate_tami_trs() errors when trs_definition_number is non-numeric", {
  # [requires Bug 1 fix: db_speces -> db_species in validate_fram_db() call]
  db <- make_mock_coho_report_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  expect_error(
    calculate_tami_trs(db, run_id = 1L, trs_definition_number = "1"),
    class = "framrsquared_error"
  )
})



## Calculation logic -----------------------------------------------------------

test_that("calculate_tami_trs() taa_type=1 yields TAA as the terminal run size", {
  db <- make_mock_coho_tami_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_tami_trs(db, run_id = 1L)

  # GroupA (taa_type=1): esc=100, catch_taa = 90 -> TAA = 190
  groupA <- dplyr::filter(result, .data$taa_name == "GroupA")
  expect_equal(groupA$terminal_run_type, "TAA")
  expect_equal(groupA$terminal_run_size, 190)
})

test_that("calculate_tami_trs() taa_type=0 yields ETRS as the terminal run size", {
  db <- make_mock_coho_tami_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_tami_trs(db, run_id = 1L)

  # GroupB (taa_type=0): esc=50, catch_etrs = 50 -> ETRS = 100
  groupB <- dplyr::filter(result, .data$taa_name == "GroupB")
  expect_equal(groupB$terminal_run_type, "ETRS")
  expect_equal(groupB$terminal_run_size, 100)
})

test_that("calculate_tami_trs() TAA includes catch from all stocks in defined fisheries", {
  db <- make_mock_coho_tami_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_tami_trs(db, run_id = 1L)

  # GroupA fisheries include stock 2's catch too; TAA (190) > stock-1-only result (140)
  groupA <- dplyr::filter(result, .data$taa_name == "GroupA")
  expect_gt(groupA$terminal_run_size, groupA$escapement + 40)  # 40 = stock-1-only catch
})

test_that("calculate_tami_trs() only uses time steps 4 and 5 for mortality", {
  db_tables <- make_mock_coho_tami_trs_db(return_list = TRUE)

  # Add a ts=3 row with large catch that should be excluded
  db_tables$Mortality <- dplyr::bind_rows(
    db_tables$Mortality,
    tibble::tibble(
      run_id = 1L, fishery_id = 10L, time_step = 3L, stock_id = 1L,
      landed_catch = 999, msf_landed_catch = 999,
      non_retention = 0, shaker = 0, drop_off = 0,
      msf_non_retention = 0, msf_shaker = 0, msf_drop_off = 0
    )
  )

  db <- make_queryable_mock_db_list(table_list = db_tables, species = "COHO")
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_tami_trs(db, run_id = 1L)

  # ts=3 catch excluded; GroupA TAA should still be 190
  groupA <- dplyr::filter(result, .data$taa_name == "GroupA")
  expect_equal(groupA$terminal_run_size, 190)
})


## Output structure ------------------------------------------------------------

test_that("calculate_tami_trs() returns a tibble with expected columns", {
  db <- make_mock_coho_tami_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_tami_trs(db, run_id = 1L)

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c("run_id", "taa_num", "taa_name", "escapement",
      "terminal_run_size", "terminal_run_type", "terminal_run_category")
    %in% names(result)
  ))
})


test_that("calculate_tami_trs() trs_definition_number filters to the specified group(s)", {
  # [requires Bug 2 fix: .data$trs_id -> .data$taa_num in the trs_definition_number filter]
  db <- make_mock_coho_tami_trs_db()
  withr::defer(disconnect_mock_fram_db(db))

  local_mocked_bindings(
    validate_fram_db = function(...) invisible(NULL),
    validate_run_id  = function(...) invisible(NULL),
    validate_numeric = function(...) invisible(NULL)
  )

  result <- calculate_tami_trs(db, run_id = 1L, trs_definition_number = 1)

  expect_equal(nrow(result), 1L)
  expect_equal(result$taa_num, 1L)
})


# INTEGRATION TESTS ------------------------------------------------------------

test_that("calculate_report_trs() runs without error on a Coho database", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_warning(
    expect_no_error(calculate_report_trs(fram_db, run_id = get_run_ids(fram_db)[1])),
    regexp = "have duplicate Stock Group Names"
  )
})

test_that("calculate_report_trs() returns a tibble with expected columns (integration)", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_warning(result  <- calculate_report_trs(fram_db, run_id = get_run_ids(fram_db)[1]),
                 regexp = "have duplicate Stock Group Names")

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c("run_id", "taa_num", "taa_name", "escapement",
      "terminal_run_size", "terminal_run_type", "terminal_run_category")
    %in% names(result)
  ))
})

test_that("calculate_report_trs() terminal_run_size is non-negative (integration)", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_warning(
    result  <- calculate_report_trs(fram_db, run_id = get_run_ids(fram_db)[1]),
    regexp = "have duplicate Stock Group Names")

  expect_true(all(result$terminal_run_size >= 0, na.rm = TRUE))
})

test_that("calculate_tami_trs() runs without error on a Coho database", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_no_error(calculate_tami_trs(fram_db, run_id = get_run_ids(fram_db)[1]))
})

test_that("calculate_tami_trs() returns a tibble with expected columns (integration)", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_pre(quiet = TRUE)
  result  <- calculate_tami_trs(fram_db, run_id = get_run_ids(fram_db)[1])

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c("run_id", "taa_num", "taa_name", "escapement",
      "terminal_run_size", "terminal_run_type", "terminal_run_category")
    %in% names(result)
  ))
})

test_that("calculate_tami_trs() terminal_run_size is non-negative (integration)", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_pre(quiet = TRUE)
  result  <- calculate_tami_trs(fram_db, run_id = get_run_ids(fram_db)[1])

  expect_true(all(result$terminal_run_size >= 0, na.rm = TRUE))
})

test_that("calculate_report_trs() and calculate_tami_trs() escapements agree for matching groups (integration)", {

  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_pre(quiet = TRUE)
  run_id  <- get_run_ids(fram_db)[1]

  expect_warning(
    result_report <- calculate_report_trs(fram_db, run_id = run_id),
    regexp = "duplicate Stock Group Names"
  )
  result_tami   <- calculate_tami_trs(fram_db, run_id = run_id)


  ## find overlapping escapement groups
  report_drivers <- fetch_table_(fram_db, "ReportDriver") |>
    dplyr::filter(driver_name == "PSCTRuns.DRV") |>
    dplyr::select(stock_id = "option1",
                  fishery_id = "option2",
                  taa_name = "option5",
                  taa_num = "option6",
                  taa_type = "option4") |>
    dplyr::mutate(taa_num = as.numeric(taa_num))

  taa_etrs <- fetch_table_(fram_db, "TAAETRSList") |>
    dplyr::mutate(taa_type_char = dplyr::recode_values(taa_type,
                                                       0 ~ "ETRS",
                                                       1 ~ "TAA",
                                                       default = NA_character_)) |>
    dplyr::select(stock_id = "taa_stk_list",
                  fishery_id = "taa_fish_list",
                  taa_name,
                  taa_num,
                  taa_type = taa_type_char) |>
    dplyr::mutate(taa_num = as.numeric(taa_num))

  ## escapement mapping --------------
  result_tami_esc <- result_tami |>
    dplyr::left_join(taa_etrs |> dplyr::select(stock_id, taa_name, taa_num),
                     by = c("taa_name", "taa_num"))|>
    dplyr::select(-taa_name, -taa_num) |>
    dplyr::select(tami_escapement = escapement,
                  stock_id)

  result_report_esc <- result_report |>
    dplyr::left_join(report_drivers |> dplyr::select(stock_id, taa_name, taa_num),
                     by = c("taa_name", "taa_num")) |>
    dplyr::select(-taa_name, -taa_num)|>
    dplyr::select(report_escapement = escapement,
                  stock_id)

  escapement_matching <- dplyr::inner_join(result_tami_esc, result_report_esc, by = "stock_id",
                    relationship = "many-to-many")

  expect_equal(escapement_matching$tami_escapement,
               escapement_matching$report_escapement)

  ## TRS mapping ---------------------------
  result_tami_trs <- result_tami |>
    dplyr::left_join(taa_etrs |> dplyr::select(stock_id, fishery_id,
                                               taa_name, taa_num),
                     by = c("taa_name", "taa_num"))|>
    dplyr::select(-taa_name, -taa_num) |>
    dplyr::select(tami_trs = terminal_run_size,
                  stock_id,
                  fishery_id,
                  terminal_run_type)

  result_report_trs <- result_report |>
    dplyr::left_join(report_drivers |>
                       dplyr::select(stock_id, fishery_id,
                                     taa_name, taa_num),
                     by = c("taa_name", "taa_num")) |>
    dplyr::select(-taa_name, -taa_num)|>
    dplyr::select(report_trs = terminal_run_size,
                  stock_id,
                  fishery_id,
                  terminal_run_type)

  trs_matching <- dplyr::inner_join(result_tami_trs, result_report_trs,
                            by = c("stock_id", "fishery_id", "terminal_run_type"),
                            relationship = "many-to-many")

  expect_equal(trs_matching$tami_trs,
               trs_matching$report_trs)

})


