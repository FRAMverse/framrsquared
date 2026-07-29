# UNIT TESTS -------------------------------------------------------------------

# --- Helpers ------------------------------------------------------------------

mock_timestep_table <- tibble::tibble(
  time_step_id    = c(1L, 2L, 3L, 4L),
  time_step_title = c("Oct - Dec", "Jan - Apr", "May - Jun", "Jul - Sep")
)

mock_taa_table <- tibble::tibble(
  taa_num        = c(1L, 2L),
  taa_name       = c("TAA Group A", "TAA Group B"),
  taa_stk_list   = c("1,2", "3"),
  taa_fish_list  = c("10,20", "0"),   # last row has fishery_id = 0
  taa_time_step1 = c(1L, 2L),
  taa_time_step2 = c(2L, 3L),
  taa_type       = c(1L, 1L)
)

make_reasonable_mock_terminal_db <- function(
    return_list = FALSE,
    species = "COHO",
    table_name = "TAAETRSList") {
  table_list <- list(
    TimeStep = mock_timestep_table,
    RunID = data.frame(species_name = species,
                       run_id = 1)
  )
  table_list[[table_name]] <- mock_taa_table

  if(return_list){
    table_list
  } else {
    make_queryable_mock_db_list(table_list, species = species)
  }
}

# ==============================================================================
# terminal_info() — input validation
# ==============================================================================

test_that("terminal_info() errors on an invalid fram_db", {
  expect_error(terminal_info(list()), class = "framrsquared_error")
})

test_that("terminal_info() errors on a transfer database", {
  fram_db <- make_mock_fram_db(type = "transfer", species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(terminal_info(fram_db), class = "framrsquared_error")
})

test_that("terminal_info() errors when species cannot be determined", {
  # A db reporting multiple species and no species argument should error or
  # require an explicit species argument
  fram_db <- make_mock_fram_db(type = "full", species = "goldfish")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(terminal_info(fram_db))
})


# ==============================================================================
# terminal_info() — species routing
# ==============================================================================

test_that("terminal_info() errors when table name is not right for species", {
  ## chinook good
  fram_db <- make_reasonable_mock_terminal_db(species = "CHINOOK",
                                              table_name = "TAAETRSListChinook")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_no_error(terminal_info(fram_db))

  withr::deferred_run()

  ## chinook bad
  fram_db <- make_reasonable_mock_terminal_db(species = "CHINOOK",
                                              table_name = "TAAETRSList")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(terminal_info(fram_db))

  withr::deferred_run()

  ## coho good
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO",
                                              table_name = "TAAETRSList")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_no_error(terminal_info(fram_db))

  withr::deferred_run()

  ## coho bad
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO",
                                              table_name = "TAAETRSListChinook")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(terminal_info(fram_db))

})

test_that("terminal_info() respects explicit species argument over db species", {
  # db is COHO but we override to CHINOOK — should route to CHINOOK code path, which should cause an error given the table name
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO",
                                              table_name = "TAAETRSList")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(terminal_info(fram_db, species = "CHINOOK"))
})


# ==============================================================================
# terminal_info() — parsing logic
# ==============================================================================

test_that("terminal_info() unnests comma-separated taa_stk_list into one row per stock", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_info(fram_db)

  # TAA Group A has 2 stocks × 2 fisheries = 4 rows; Group B (fishery 0) is dropped
  expect_equal(nrow(result), 4L)
})

test_that("terminal_info() unnests comma-separated taa_fish_list into one row per fishery", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_info(fram_db)

  expect_true(all(result$fishery_id %in% c(10L, 20L)))
})

test_that("terminal_info() filters out rows where fishery_id == 0", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_info(fram_db)

  expect_false(any(result$fishery_id == 0))
})

test_that("terminal_info() formats terminal_time_steps as 'step1-step2'", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_info(fram_db)

  # taa_time_step1 = 1, taa_time_step2 = 2 for TAA Group A
  expect_true(all(result$terminal_time_steps == "1-2"))
})

test_that("terminal_info() assembles terminal_months from the TimeStep table", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_info(fram_db)

  # TimeStep 1 title = "Oct - Dec" → start "Oct"; TimeStep 2 = "Jan - Apr" → end "Apr"
  expect_true(all(result$terminal_months == "Oct-Apr"))
})

test_that("terminal_info() coerces stock_id and fishery_id to numeric", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_info(fram_db)

  expect_true(is.numeric(result$stock_id))
  expect_true(is.numeric(result$fishery_id))
})


# ==============================================================================
# terminal_info() — output structure
# ==============================================================================

test_that("terminal_info() returns a tibble", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_true(tibble::is_tibble(terminal_info(fram_db)))
})

test_that("terminal_info() returns exactly the expected column set", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_info(fram_db)

  expect_named(result, c("taa_name", "taa_num", "stock_label", "stock_id",
                         "terminal_time_steps", "terminal_months",
                         "fishery_label", "fishery_id"))
})


# ==============================================================================
# terminal_stocks() and terminal_fisheries()
# ==============================================================================

test_that("terminal_stocks() errors on an invalid fram_db", {
  expect_error(terminal_stocks(list()), class = "framrsquared_error")
})

test_that("terminal_fisheries() errors on an invalid fram_db", {
  expect_error(terminal_fisheries(list()), class = "framrsquared_error")
})

test_that("terminal_stocks() returns only stock-relevant columns", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_stocks(fram_db)

  expect_named(result, c("taa_name", "stock_label", "terminal_months",
                         "stock_id", "terminal_time_steps"))
})

test_that("terminal_stocks() returns distinct rows (no duplicates across fisheries)", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_stocks(fram_db)

  expect_equal(nrow(result), nrow(dplyr::distinct(result)))
})

test_that("terminal_fisheries() returns only fishery-relevant columns", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_fisheries(fram_db)

  expect_named(result, c("taa_name", "fishery_label", "fishery_id"))
})

test_that("terminal_fisheries() returns distinct rows (no duplicates across stocks)", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_fisheries(fram_db)

  expect_equal(nrow(result), nrow(dplyr::distinct(result)))
})

test_that("terminal_fisheries() returns correct results", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_fisheries(fram_db)

  expect_snapshot(result)
})

test_that("terminal_stocks() returns correct results", {
  fram_db <- make_reasonable_mock_terminal_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- terminal_stocks(fram_db)

  expect_snapshot(result)
})


# INTEGRATION TESTS ------------------------------------------------------------

test_that("terminal_info() runs without error on a Coho database", {
  skip_if_no_test_db()

  fram_db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  expect_no_error(terminal_info(fram_db))
})

test_that("terminal_info() provides consistent results on coho database", {
  skip_if_no_test_db()

  fram_db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  result <- terminal_info(fram_db)
  expect_snapshot(result)
})

test_that("terminal_stocks() provides consistent results on coho database", {
  skip_if_no_test_db()

  fram_db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  result <- terminal_stocks(fram_db)
  expect_snapshot(result)
})

test_that("terminal_fisheries() provides consistent results on coho database", {
  skip_if_no_test_db()

  fram_db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  result <- terminal_fisheries(fram_db)
  expect_snapshot(result)
})

test_that("terminal_stocks() rows are a subset of terminal_info() rows", {
  skip_if_no_test_db()

  fram_db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  full   <- terminal_info(fram_db)
  stocks <- terminal_stocks(fram_db)

  expect_true(all(stocks$stock_id %in% full$stock_id))
  expect_lte(nrow(stocks), nrow(full))
})

test_that("terminal_fisheries() rows are a subset of terminal_info() rows", {
  skip_if_no_test_db()

  fram_db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  full      <- terminal_info(fram_db)
  fisheries <- terminal_fisheries(fram_db)

  expect_true(all(fisheries$fishery_id %in% full$fishery_id))
  expect_lte(nrow(fisheries), nrow(full))
})
