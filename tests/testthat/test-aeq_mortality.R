# UNIT TESTS -------------------------------------------------------------------

# --- Helpers ------------------------------------------------------------------
# Minimal two-row mortality table (one terminal fishery, one non-terminal)


make_reasonable_mock_chinook_db <- function(return_list = FALSE, has_terminal_fishery = TRUE){
  ## fisheries 23 and 24
  ## stocks 1 and 2
  ## run_id 1, time_step 1
  ## WITHOUT AEQ:
  ## fishery 23 is 10% stock 1
  ## fishery 24 is 50% stock 1
  ## AEQ will be halve the effective catch of stock 2, so WITH AEQ:
  ## MSP doesn't matter, since it's a fishery-wide multiplier.

  ## dummy term table so the function doesn't error.
  mock_mortality <- tibble::tibble(
    primary_key       = c(1L, 2L),
    run_id            = c(1L, 1L),
    fishery_id        = c(10L, 20L),
    time_step         = c(1L, 1L),
    stock_id          = c(1L, 1L),
    age               = c(3L, 3L),
    landed_catch      = c(10, 10),
    non_retention     = c(2, 2),
    shaker            = c(1, 1),
    drop_off          = c(0.5, 0.5),
    msf_landed_catch  = c(0, 0),
    msf_non_retention = c(0, 0),
    msf_shaker        = c(0, 0),
    msf_drop_off      = c(0, 0)
  )

  if(has_terminal_fishery){
    mock_terminal_fishery_flag = tibble::tibble(
      base_period_id = 2L,
      fishery_id     = 10L,
      time_step      = 1L,
      terminal_flag  = 1L
    )
  } else {
    mock_terminal_fishery_flag = tibble::tibble(
      base_period_id = integer(0),
      fishery_id     = integer(0),
      time_step      = integer(0),
      terminal_flag  = integer(0)
    )
  }

  table_list = list(Mortality = mock_mortality,
                    RunID = tibble::tibble(
                      run_id = 1L, base_period_id = 2L),
                    AEQ = tibble::tibble(
                      base_period_id = 2L, stock_id = 1L, age = 3L, time_step = 1L, aeq = 0.5),
                    FisheryModelStockProportion = data.frame(
                      fishery_id = c(10, 20),
                      base_period_id = c(2, 2),
                      model_stock_proportion = c(1, 1)
                    ),
                    TerminalFisheryFlag = mock_terminal_fishery_flag
                    )
  if(return_list){
    return(table_list)
  } else {
    return(make_queryable_mock_db_list(
      table_list = table_list,
      species = "CHINOOK"
    )
    )
  }
}

## when we need multiple run ids.
make_reasonable_mock_chinook_db_extended <- function(return_list = FALSE, has_terminal_fishery = TRUE){
  ## fisheries 23 and 24
  ## stocks 1 and 2
  ## run_id 1, time_step 1
  ## WITHOUT AEQ:
  ## fishery 23 is 10% stock 1
  ## fishery 24 is 50% stock 1
  ## AEQ will be halve the effective catch of stock 2, so WITH AEQ:
  ## MSP doesn't matter, since it's a fishery-wide multiplier.

  ## dummy term table so the function doesn't error.
  mock_mortality <- tibble::tibble(
    primary_key       = c(1L, 2L, 3, 4),
    run_id            = c(1L, 1L, 2, 2),
    fishery_id        = c(10L, 20L, 10, 20),
    time_step         = c(1L, 1L, 1, 1),
    stock_id          = c(1L, 1L, 1, 1),
    age               = c(3L, 3L, 3, 3),
    landed_catch      = c(10, 10, 10, 10),
    non_retention     = c(2, 2, 2, 2),
    shaker            = c(1, 1, 1, 1),
    drop_off          = c(0.5, 0.5, 0.5, 0.5),
    msf_landed_catch  = c(0, 0, 0, 0),
    msf_non_retention = c(0, 0, 0, 0),
    msf_shaker        = c(0, 0, 0, 0),
    msf_drop_off      = c(0, 0, 0, 0)
  )

  if(has_terminal_fishery){
    mock_terminal_fishery_flag = tibble::tibble(
      base_period_id = 2L,
      fishery_id     = 10L,
      time_step      = 1L,
      terminal_flag  = 1L
    )
  } else {
    mock_terminal_fishery_flag = tibble::tibble(
      base_period_id = integer(0),
      fishery_id     = integer(0),
      time_step      = integer(0),
      terminal_flag  = integer(0)
    )
  }

  table_list = list(Mortality = mock_mortality,
                    RunID = tibble::tibble(
                      run_id = c(1, 2), base_period_id = c(2, 2)),
                    AEQ = tibble::tibble(
                      base_period_id = 2L, stock_id = 1L, age = 3L, time_step = 1L, aeq = 0.5),
                    FisheryModelStockProportion = data.frame(
                      fishery_id = c(10, 20),
                      base_period_id = c(2, 2),
                      model_stock_proportion = c(1, 1)
                    ),
                    TerminalFisheryFlag = mock_terminal_fishery_flag)
  if(return_list){
    return(table_list)
  } else {
    return(make_queryable_mock_db_list(
      table_list = table_list,
      species = "CHINOOK"
    )
    )
  }
}


# --- Input validation ---------------------------------------------------------

test_that("aeq_mortality() errors on a Coho database", {
  fram_db <- make_mock_fram_db(type = "full", species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(aeq_mortality(fram_db), class = "framrsquared_error")
})

test_that("aeq_mortality() errors on a transfer database", {
  fram_db <- make_mock_fram_db(type = "transfer", species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(aeq_mortality(fram_db), class = "framrsquared_error")
})

test_that("aeq_mortality() errors when run_id is non-numeric", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(aeq_mortality(fram_db, run_id = "132"), class = "framrsquared_error")
  expect_error(aeq_mortality(fram_db, run_id = TRUE),  class = "framrsquared_error")
})

test_that("aeq_mortality() errors when msp is not logical", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(aeq_mortality(fram_db, msp = "yes"),    class = "framrsquared_error")
  expect_error(aeq_mortality(fram_db, msp = 1),        class = "framrsquared_error")
})

test_that("aeq_mortality() errors when msp is a logical vector longer than 1", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(aeq_mortality(fram_db, msp = c(TRUE, FALSE)), class = "framrsquared_error")
})


# --- AEQ calculation logic ----------------------------------------------------

test_that("aeq_mortality() multiplies non-terminal mortality columns by AEQ, not terminal mort columns", {
  fram_db <- make_reasonable_mock_chinook_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # local_mocked_bindings(
  #   validate_run_id  = function(...) invisible(TRUE),
  #   msp_mortality    = function(fram_db, ...) mock_mortality_tbl,
  #   fetch_table_     = mock_fetch_table
  # )

  result <- aeq_mortality(fram_db, label = FALSE)

  # first row is non-terminal, second row is terminal
  expect_equal(result$landed_catch,  c(10, 5))
  expect_equal(result$non_retention, c(2, 1))
  expect_equal(result$shaker,        c(1, 0.5))
  expect_equal(result$drop_off,      c(.5, 0.25))
})



test_that("aeq_mortality() uses msp_mortality() when msp = TRUE", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  called_msp <- FALSE

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) { called_msp <<- TRUE; return(NULL) }
  )
  expect_error(aeq_mortality(fram_db, msp = TRUE, label = FALSE))

  expect_true(called_msp)
})


test_that("aeq_mortality() does not use msp_mortality when msp = FALSE", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  called_msp <- FALSE

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) { called_msp <<- TRUE; return(NULL) }
  )
  expect_error(aeq_mortality(fram_db, msp = FALSE, label = FALSE))

  expect_false(called_msp)
})


# --- Output structure ---------------------------------------------------------

test_that("aeq_mortality() renames 'aeq' column to 'aeq_constant', retains base_period_id and terminal_flag_columns", {
  fram_db <- make_reasonable_mock_chinook_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- aeq_mortality(fram_db, label = FALSE)

  expect_true("aeq_constant" %in% names(result))
  expect_false("aeq" %in% names(result))
  expect_true("base_period_id" %in% names(result))
  expect_true("terminal_flag"  %in% names(result))
})

test_that("aeq_mortality() sets species attribute to 'CHINOOK'", {
  fram_db <- make_reasonable_mock_chinook_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- aeq_mortality(fram_db, label = FALSE)

  expect_equal(attr(result, "species"), "CHINOOK")
})

# --- run_id filtering ---------------------------------------------------------

test_that("aeq_mortality() returns all rows when run_id = NULL", {
  fram_db <- make_reasonable_mock_chinook_db_extended()
  withr::defer(disconnect_mock_fram_db(fram_db))

  mort_table <- make_reasonable_mock_chinook_db_extended(return_list = TRUE)$Mortality

  # local_mocked_bindings(
  #   validate_run_id  = function(...) invisible(TRUE),
  #   msp_mortality    = function(fram_db, ...) mock_mortality_tbl,
  #   fetch_table_     = mock_fetch_table
  # )

  result <- aeq_mortality(fram_db, run_id = NULL, label = FALSE)

  expect_equal(nrow(result), nrow(mort_table))
})

test_that("aeq_mortality() filters to specified run_id(s)", {
  fram_db <- make_reasonable_mock_chinook_db_extended()
  withr::defer(disconnect_mock_fram_db(fram_db))

  mort_table <- make_reasonable_mock_chinook_db_extended(return_list = TRUE)$Mortality

  result <- aeq_mortality(fram_db, run_id = 1L, label = FALSE)

  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), sum(mort_table$run_id == 1))
})


# INTEGRATION TESTS ------------------------------------------------------------

test_that("aeq_mortality() runs without error on a Chinook pre-season database", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  expect_no_error(aeq_mortality(fram_db, label = FALSE))
})

test_that("aeq_mortality() returns a tibble with expected columns", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  result  <- aeq_mortality(fram_db, label = FALSE)

  expect_true(tibble::is_tibble(result))
  expect_true(all(
    c("run_id", "fishery_id", "stock_id", "age", "time_step",
      "landed_catch", "non_retention", "shaker", "drop_off",
      "msf_landed_catch", "msf_non_retention", "msf_shaker", "msf_drop_off",
      "base_period_id", "aeq_constant", "terminal_flag")
    %in% names(result)
  ))
})

test_that("aeq_mortality() mortality columns are all non-negative", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  result  <- aeq_mortality(fram_db, label = FALSE)

  mort_cols <- c("landed_catch", "non_retention", "shaker", "drop_off",
                 "msf_landed_catch", "msf_non_retention", "msf_shaker", "msf_drop_off")

  for (col in mort_cols) {
    expect_true(
      all(result[[col]] >= 0, na.rm = TRUE),
      label = paste("Non-negative check for column:", col)
    )
  }
})

test_that("aeq_mortality() msp=TRUE and msp=FALSE produce different totals", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  run_id  <- get_run_ids(fram_db)[1]

  result_msp   <- aeq_mortality(fram_db, run_id = run_id, msp = TRUE,  label = FALSE)
  result_nomsp <- aeq_mortality(fram_db, run_id = run_id, msp = FALSE, label = FALSE)

  expect_false(
    isTRUE(all.equal(
      sum(result_msp$landed_catch,   na.rm = TRUE),
      sum(result_nomsp$landed_catch, na.rm = TRUE)
    ))
  )
})

test_that("aeq_mortality() run_id filter returns only requested run(s)", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db  <- connection_chin_pre(quiet = TRUE)
  run_ids  <- get_run_ids(fram_db)
  focus_id <- run_ids[1]

  result_all      <- aeq_mortality(fram_db, label = FALSE)
  result_filtered <- aeq_mortality(fram_db, run_id = focus_id, label = FALSE)

  expect_true(all(result_filtered$run_id == focus_id))
  expect_lt(nrow(result_filtered), nrow(result_all))
})

test_that("aeq_mortality() errors on a Coho database (integration)", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_error(aeq_mortality(fram_db), class = "framrsquared_error")
})

test_that("aeq_mortality_() alias matches aeq_mortality(..., label = FALSE)", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db  <- connection_chin_pre(quiet = TRUE)
  run_id   <- get_run_ids(fram_db)[1]

  result_alias  <- aeq_mortality_(fram_db, run_id = run_id)
  result_direct <- aeq_mortality(fram_db,  run_id = run_id, label = FALSE)

  expect_equal(result_alias, result_direct)
})

test_that("aeq_mortality() terminal fishery rows are not AEQ-scaled (integration)", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  run_id  <- get_run_ids(fram_db)[1]
  result  <- aeq_mortality(fram_db, run_id = run_id, msp = FALSE, label = FALSE)

  # For terminal rows, the AEQ constant should have no effect, so the
  # raw mortality can be cross-checked against the un-scaled Mortality table
  result_raw <- fetch_table_(fram_db, "Mortality") |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::select(-"primary_key") |>
    dplyr::arrange(.data$run_id, .data$fishery_id,
                   .data$time_step, .data$stock_id
    )

  terminal_ids <- result |>
    dplyr::filter(!is.na(.data$terminal_flag)) |>
    dplyr::select("fishery_id", "time_step") |>
    dplyr::distinct()

  if (nrow(terminal_ids) > 0) {
    aeq_terminal <- result |>
      dplyr::semi_join(terminal_ids, by = c("fishery_id", "time_step"))|>
      dplyr::select("run_id", "stock_id", "age", "fishery_id", "time_step", "landed_catch":"msf_encounter")
    raw_terminal <- result_raw |>
      dplyr::semi_join(terminal_ids, by = c("fishery_id", "time_step"))|>
      dplyr::select("run_id", "stock_id", "age", "fishery_id", "time_step", "landed_catch":"msf_encounter")
    aeq_comp <- aeq_terminal |>
      dplyr::full_join(raw_terminal, by = c("run_id", "stock_id", "age", "fishery_id", "time_step"),
                       suffix = c("_aeq", "_raw")
      )

    expect_equal(aeq_comp$landed_catch_aeq, aeq_comp$landed_catch_raw)
    expect_equal(aeq_comp$drop_off_aeq, aeq_comp$drop_off_raw)
    expect_equal(aeq_comp$msf_shaker_aeq, aeq_comp$msf_shaker_raw)
  } else {
    skip("No terminal fishery rows found in this database for run_id filtering check.")
  }
})
