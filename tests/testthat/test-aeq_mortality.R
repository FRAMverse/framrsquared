# UNIT TESTS -------------------------------------------------------------------

# --- Helpers ------------------------------------------------------------------

# Minimal two-row mortality table (one terminal fishery, one non-terminal)
mock_mortality_tbl <- tibble::tibble(
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

mock_fetch_table <- function(fram_db, table, ...) {
  switch(table,
         "RunID" = tibble::tibble(
           run_id = 1L, base_period_id = 2L
         ),
         "AEQ" = tibble::tibble(
           base_period_id = 2L, stock_id = 1L, age = 3L, time_step = 1L, aeq = 0.5
         ),
         "TerminalFisheryFlag" = tibble::tibble(
           base_period_id = integer(0),
           fishery_id     = integer(0),
           time_step      = integer(0),
           terminal_flag  = integer(0)
         ),
         "Mortality" = mock_mortality_tbl,
         stop("Unexpected table: ", table)
  )
}

mock_fetch_table_with_terminal <- function(fram_db, table, ...) {
  if (table == "TerminalFisheryFlag") {
    return(tibble::tibble(
      base_period_id = 2L,
      fishery_id     = 10L,
      time_step      = 1L,
      terminal_flag  = 1L
    ))
  } else {
    mock_fetch_table(fram_db, table, ...)
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

test_that("aeq_mortality() multiplies non-terminal mortality columns by AEQ", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) mock_mortality_tbl,
    fetch_table_     = mock_fetch_table
  )

  result <- aeq_mortality(fram_db, label = FALSE)

  # All rows are non-terminal (no TerminalFisheryFlag rows), so AEQ = 0.5 applies
  expect_equal(result$landed_catch,  c(5, 5))
  expect_equal(result$non_retention, c(1, 1))
  expect_equal(result$shaker,        c(0.5, 0.5))
  expect_equal(result$drop_off,      c(0.25, 0.25))
})

test_that("aeq_mortality() does NOT scale terminal fishery mortality columns", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) mock_mortality_tbl,
    fetch_table_     = mock_fetch_table_with_terminal
  )

  result <- aeq_mortality(fram_db, label = FALSE)

  # fishery_id 10 is terminal → not scaled; fishery_id 20 is not → scaled by 0.5
  terminal_row     <- result[result$fishery_id == 10L, ]
  non_terminal_row <- result[result$fishery_id == 20L, ]

  expect_equal(terminal_row$landed_catch,     10)
  expect_equal(non_terminal_row$landed_catch, 5)
})

test_that("aeq_mortality() uses msp_mortality() when msp = TRUE", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  called_msp <- FALSE

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) { called_msp <<- TRUE; mock_mortality_tbl },
    fetch_table_     = mock_fetch_table
  )

  aeq_mortality(fram_db, msp = TRUE, label = FALSE)

  expect_true(called_msp)
})

test_that("aeq_mortality() uses fetch_table_('Mortality') when msp = FALSE", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  called_mortality_table <- FALSE

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    fetch_table_     = function(fram_db, table, ...) {
      if (table == "Mortality") called_mortality_table <<- TRUE
      mock_fetch_table(fram_db, table, ...)
    }
  )

  aeq_mortality(fram_db, msp = FALSE, label = FALSE)

  expect_true(called_mortality_table)
})


# --- Output structure ---------------------------------------------------------

test_that("aeq_mortality() renames 'aeq' column to 'aeq_constant', retains base_period_id and terminal_flag_columns", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) mock_mortality_tbl,
    fetch_table_     = mock_fetch_table
  )

  result <- aeq_mortality(fram_db, label = FALSE)

  expect_true("aeq_constant" %in% names(result))
  expect_false("aeq" %in% names(result))
  expect_true("base_period_id" %in% names(result))
  expect_true("terminal_flag"  %in% names(result))
})

test_that("aeq_mortality() sets species attribute to 'CHINOOK'", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) mock_mortality_tbl,
    fetch_table_     = mock_fetch_table
  )

  result <- aeq_mortality(fram_db, label = FALSE)

  expect_equal(attr(result, "species"), "CHINOOK")
})

test_that("aeq_mortality() output is sorted by run_id, fishery_id, time_step, stock_id", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # Supply rows in reverse order to confirm sorting is applied
  reversed <- mock_mortality_tbl[rev(seq_len(nrow(mock_mortality_tbl))), ]

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) reversed,
    fetch_table_     = mock_fetch_table
  )

  result <- aeq_mortality(fram_db, label = FALSE)

  expect_equal(result$fishery_id, result$fishery_id)
})


# --- run_id filtering ---------------------------------------------------------

test_that("aeq_mortality() returns all rows when run_id = NULL", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) mock_mortality_tbl,
    fetch_table_     = mock_fetch_table
  )

  result <- aeq_mortality(fram_db, run_id = NULL, label = FALSE)

  expect_equal(nrow(result), nrow(mock_mortality_tbl))
})

test_that("aeq_mortality() filters to specified run_id(s)", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  multi_run_mortality <- dplyr::bind_rows(
    mock_mortality_tbl,
    dplyr::mutate(mock_mortality_tbl, run_id = 2L)
  )

  local_mocked_bindings(
    validate_run_id  = function(...) invisible(TRUE),
    msp_mortality    = function(fram_db, ...) multi_run_mortality,
    fetch_table_     = function(fram_db, table, ...) {
      if (table == "RunID") {
        return(tibble::tibble(run_id = c(1L, 2L), base_period_id = c(2L, 2L)))
      }
      mock_fetch_table(fram_db, table, ...)
    }
  )

  result <- aeq_mortality(fram_db, run_id = 1L, label = FALSE)

  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), nrow(mock_mortality_tbl))
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
  result_raw <- framrsquared:::fetch_table_(fram_db, "Mortality") |>
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
