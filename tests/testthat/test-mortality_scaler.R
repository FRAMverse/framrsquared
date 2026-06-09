# UNIT TESTS -------------------------------------------------------------------

# --- Helpers ------------------------------------------------------------------

# Two-stock, two-fishery mortality table used by both Coho and Chinook helpers
#
# Fishery 10: stock 1 total = 13.5,  stock 2 total = 5   → fishery total = 18.5
# Fishery 20: stock 1 total = 7,     stock 2 total = 2   → fishery total = 9
mock_mortality_two_stocks <- tibble::tibble(
  primary_key       = 1:4,
  run_id            = c(1L, 1L, 1L, 1L),
  fishery_id        = c(10L, 10L, 20L, 20L),
  time_step         = c(1L, 1L, 1L, 1L),
  stock_id          = c(1L, 2L, 1L, 2L),
  age               = c(3L, 3L, 3L, 3L),
  landed_catch      = c(10, 4, 6, 2),
  non_retention     = c(2, 1, 1, 0),
  shaker            = c(1, 0, 0, 0),
  drop_off          = c(0.5, 0, 0, 0),
  encounter         = rep(10, 4),
  msf_landed_catch  = c(0, 0, 0, 0),
  msf_non_retention = c(0, 0, 0, 0),
  msf_shaker        = c(0, 0, 0, 0),
  msf_drop_off      = c(0, 0, 0, 0),
  msf_encounter     = rep(10, 4)
)

make_reasonable_mock_coho_db <- function(return_list = FALSE) {
  table_list <- list(
    Mortality = mock_mortality_two_stocks,
    RunID     = tibble::tibble(run_id = 1L),
    Stock     = tibble::tibble(stock_id = c(1L, 2L))
  )
  if(return_list){
    return(table_list)
  } else {
    return(
      make_queryable_mock_db_list(table_list = table_list, species = "COHO")
    )
  }
}

make_reasonable_mock_chinook_db_scalers <- function(return_list = FALSE,
                                                    msp_val = 1) {
  # AEQ = 0.5 for both stocks; no terminal fisheries
  table_list <- list(
    Mortality = mock_mortality_two_stocks,
    RunID     = tibble::tibble(run_id = 1L, base_period_id = 2L),
    Stock     = tibble::tibble(stock_id = c(1L, 2L)),
    AEQ       = tibble::tibble(
      base_period_id = c(2L, 2L, 1L, 1L),
      stock_id       = c(1L, 2L, 1L, 2L),
      age            = c(3L, 3L, 3L, 3L),
      time_step      = c(1L, 1L, 1L, 1L),
      aeq            = c(0.5, 0.5, 0.5, 0.5)),
    FisheryModelStockProportion = tibble::tibble(
      fishery_id             = c(10L, 20L),
      base_period_id         = c(2L,  2L),
      model_stock_proportion = c(msp_val,   msp_val)
    ),
    TerminalFisheryFlag = tibble::tibble(
      base_period_id = integer(0),
      fishery_id     = integer(0),
      time_step      = integer(0),
      terminal_flag  = integer(0)
    )
  )
  if(return_list){
    return(table_list)
  } else {
    return(make_queryable_mock_db_list(table_list, species = "CHINOOK"))
  }
}


# ==============================================================================
# add_total_mortality()
# ==============================================================================

# --- Input validation ---------------------------------------------------------

test_that("add_total_mortality() errors when .data is not a data frame", {
  expect_error(add_total_mortality(list(a = 1)), class = "framrsquared_error")
  expect_error(add_total_mortality("not a df"),  class = "framrsquared_error")
})

test_that("add_total_mortality() errors when required mortality columns are missing", {
  incomplete <- tibble::tibble(landed_catch = 1, non_retention = 1, shaker = 1)
  expect_error(add_total_mortality(incomplete), class = "framrsquared_error")
})

test_that("add_total_mortality() errors when only the msf_* columns are missing", {
  missing_msf <- mock_mortality_two_stocks |>
    dplyr::select(-msf_landed_catch, -msf_non_retention, -msf_shaker, -msf_drop_off)
  expect_error(add_total_mortality(missing_msf), class = "framrsquared_error")
})

# --- Calculation logic --------------------------------------------------------

test_that("add_total_mortality() correctly sums all eight mortality columns", {
  result <- add_total_mortality(mock_mortality_two_stocks)

  expected <- with(mock_mortality_two_stocks,
                   landed_catch + non_retention + shaker + drop_off +
                     msf_landed_catch + msf_non_retention + msf_shaker + msf_drop_off
  )

  expect_equal(result$total_mortality, expected)
})

test_that("add_total_mortality() places total_mortality immediately before landed_catch", {
  result    <- add_total_mortality(mock_mortality_two_stocks)
  col_names <- names(result)
  expect_equal(
    which(col_names == "total_mortality"),
    which(col_names == "landed_catch") - 1
  )
})

test_that("add_total_mortality() returns total_mortality = 0 when all mortality columns are zero", {
  all_zero <- mock_mortality_two_stocks |>
    dplyr::mutate(dplyr::across(
      c(landed_catch, non_retention, shaker, drop_off,
        msf_landed_catch, msf_non_retention, msf_shaker, msf_drop_off),
      \(x) 0
    ))
  result <- add_total_mortality(all_zero)
  expect_true(all(result$total_mortality == 0))
})

test_that("add_total_mortality() includes MSF columns in the sum", {
  with_msf <- mock_mortality_two_stocks |>
    dplyr::mutate(msf_landed_catch = 5, msf_drop_off = 2)

  result         <- add_total_mortality(with_msf)
  result_no_msf  <- add_total_mortality(mock_mortality_two_stocks)

  expect_true(all(result$total_mortality > result_no_msf$total_mortality))
})

# --- Output structure ---------------------------------------------------------

test_that("add_total_mortality() preserves all original columns", {
  result <- add_total_mortality(mock_mortality_two_stocks)
  expect_true(all(names(mock_mortality_two_stocks) %in% names(result)))
})

test_that("add_total_mortality() does not change the number of rows", {
  result <- add_total_mortality(mock_mortality_two_stocks)
  expect_equal(nrow(result), nrow(mock_mortality_two_stocks))
})


# ==============================================================================
# mortality_scalers() — input validation
# ==============================================================================

test_that("mortality_scalers() errors on an invalid fram_db", {
  expect_error(mortality_scalers(list()), class = "framrsquared_error")
})

test_that("mortality_scalers() errors when run_id is non-numeric", {
  fram_db <- make_mock_fram_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    mortality_scalers(fram_db, run_id = "abc", stock_id = 1L),
    class = "framrsquared_error"
  )
})

test_that("mortality_scalers() errors when msp is not logical", {
  fram_db <- make_reasonable_mock_chinook_db_scalers()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    mortality_scalers(fram_db, run_id = 1L, stock_id = 1L, msp = "yes"),
    class = "framrsquared_error"
  )
})

test_that("mortality_scalers() errors when msp is a logical vector longer than 1", {
  fram_db <- make_reasonable_mock_chinook_db_scalers()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    mortality_scalers(fram_db, run_id = 1L, stock_id = 1L, msp = c(TRUE, FALSE)),
    class = "framrsquared_error"
  )
})


# ==============================================================================
# mortality_scalers() — Coho path
# ==============================================================================

test_that("mortality_scalers() returns expected output columns for coho", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L)

  expect_true(all(
    c("run_id", "fishery_id", "time_step",
      "fishery_mortality", "stock_mortality", "stock_mortality_ratio",
      "fishery_mortality_no_cnr", "stock_mortality_ratio_no_cnr")
    %in% names(result)
  ))
})

test_that("mortality_scalers() returns one row per fishery x timestep combination for coho", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L)

  # Two fisheries × one timestep = 2 rows
  expect_equal(nrow(result), 2L)
})

test_that("mortality_scalers() computes fishery_mortality as sum across all stocks for coho", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L) |>
    dplyr::arrange(fishery_id)

  # Fishery 10: stock1 = 13.5, stock2 = 5 → 18.5
  # Fishery 20: stock1 = 7,    stock2 = 2 → 9
  expect_equal(result$fishery_mortality, c(18.5, 9))
})

test_that("mortality_scalers() computes fishery_mortality_no_cnr as sum across all stocks for coho, skips non_retention", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L) |>
    dplyr::arrange(fishery_id)

  # Fishery 10: stock1 = 11.5, stock2 = 4. So total 15.5
  # Fishery 20: stock1 = 6,    stock2 = 2. So total 8
  expect_equal(result$fishery_mortality_no_cnr, c(15.5, 8))
})

test_that("mortality_scalers() computes stock_mortality for only the focal stock(s) for coho", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L) |>
    dplyr::arrange(fishery_id)

  # Stock 1 only: fishery 10 = 13.5, fishery 20 = 7
  expect_equal(result$stock_mortality, c(13.5, 7))
})

test_that("mortality_scalers() includes multiple focal stocks when stock_id is a vector for coho", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(.package = "framrosetta",
                        label_fisheries = function(x, ...) x
  )

  result_both   <- mortality_scalers(fram_db, run_id = 1L, stock_id = c(1L, 2L)) |>
    dplyr::arrange(fishery_id)
  result_single <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L) |>
    dplyr::arrange(fishery_id)

  # With both stocks as focal, stock_mortality should equal fishery_mortality
  expect_equal(result_both$stock_mortality, result_both$fishery_mortality)
  expect_true(all(result_both$stock_mortality > result_single$stock_mortality))
})

test_that("mortality_scalers() stock_mortality_ratio = stock_mortality / fishery_mortality for coho", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(.package = "framrosetta",
                        label_fisheries = function(x, ...) x
  )

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L)

  expect_equal(
    result$stock_mortality_ratio,
    result$stock_mortality / result$fishery_mortality
  )
})

test_that("mortality_scalers() stock_mortality_ratio_no_cnr = stock_mortality / fishery_mortality_no_cnr for coho", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(.package = "framrosetta",
                        label_fisheries = function(x, ...) x
  )

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L)

  expect_equal(
    result$stock_mortality_ratio_no_cnr,
    result$stock_mortality / result$fishery_mortality_no_cnr
  )
})

test_that("mortality_scalers() sets species attribute to 'COHO' when appropriate", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))


  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L)

  expect_equal(attr(result, "species"), "COHO")
})

test_that("mortality_scalers() filters to only the supplied run_id", {
  # Add a second run with identical data to confirm filtering
  two_run_mortality <- dplyr::bind_rows(
    mock_mortality_two_stocks,
    dplyr::mutate(mock_mortality_two_stocks, run_id = 2L)
  )
  table_list <- list(
    Mortality = two_run_mortality,
    RunID     = tibble::tibble(run_id = c(1L, 2L)),
    Stock     = tibble::tibble(stock_id = c(1L, 2L))
  )
  fram_db <- make_queryable_mock_db_list(table_list = table_list, species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L)

  expect_true(all(result$run_id == 1L))
})


# ==============================================================================
# mortality_scalers() — Chinook path
# ==============================================================================

test_that("mortality_scalers() returns expected output columns for Chinook", {
  fram_db <- make_reasonable_mock_chinook_db_scalers()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L)

  expect_true(all(
    c("run_id", "fishery_id", "time_step",
      "fishery_mortality", "stock_mortality", "stock_mortality_ratio",
      "fishery_mortality_no_cnr", "stock_mortality_ratio_no_cnr")
    %in% names(result)
  ))
})


# reference: total mort per fishery for stock 1 is
# 13.5, 7 for fisheires 10 and 20. So with AEQ factor of 0.5, should be 6.75, 3.5.
test_that("mortality_scalers_chinook_() AEQ scaling affects fishery_mortality values", {
  fram_db <- make_reasonable_mock_chinook_db_scalers()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L) |>
    dplyr::arrange(fishery_id)


  # AEQ = 0.5 for all stocks, so fishery_mortality should be half the raw totals
  # Fishery 10 raw = 13.5 → AEQ = ; fishery 20 raw = 9 → AEQ = 4.5
  expect_equal(result$fishery_mortality, c(9.25, 4.5))
})


test_that("mortality_scalers_chinook_() handles msp correctly", {
  msp_val = 0.2
  fram_db <- make_reasonable_mock_chinook_db_scalers(msp = msp_val)
  withr::defer(disconnect_mock_fram_db(fram_db))
  #
  local_mocked_bindings(.package = "framrosetta",
                        label_fisheries = function(x, ...) x
  )


  with_msp <- (mortality_scalers(fram_db, run_id = 1L, stock_id = 1L, msp = TRUE))


  without_msp <- (mortality_scalers(fram_db, run_id = 1L, stock_id = 1L, msp = FALSE))


  expect_true(all(with_msp$fishery_mortality * msp_val ==
                    without_msp$fishery_mortality))

})

test_that("mortality_scalers_chinook_() sets species attribute to 'CHINOOK'", {
  fram_db <- make_reasonable_mock_chinook_db_scalers()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(.package = "framrosetta",
                        label_fisheries = function(x, ...) x
  )

  result <- mortality_scalers(fram_db, run_id = 1L, stock_id = 1L)

  expect_equal(attr(result, "species"), "CHINOOK")
})


# INTEGRATION TESTS ------------------------------------------------------------

test_that("mortality_scalers() runs without error on a Coho database", {
  skip_if_no_test_db()

  fram_db  <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))
  run_id   <- get_run_ids(fram_db)[1]

  expect_no_error(mortality_scalers(fram_db, run_id = run_id, stock_id = 1L))
})

test_that("mortality_scalers() returns a tibble with expected columns on Coho", {
  skip_if_no_test_db()

  fram_db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  run_id  <- get_run_ids(fram_db)[1]
  result  <- mortality_scalers(fram_db, run_id = run_id, stock_id = 1L)

  expect_true(tibble::is_tibble(result))
  expect_true(all(
    c("run_id", "fishery_id", "time_step",
      "fishery_mortality", "stock_mortality", "stock_mortality_ratio")
    %in% names(result)
  ))
})

test_that("mortality_scalers() stock_mortality_ratio is between 0 and 1 on Coho", {
  skip_if_no_test_db()

  fram_db <- connection_coho_pre(quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  run_id  <- get_run_ids(fram_db)[1]
  result  <- mortality_scalers(fram_db, run_id = run_id, stock_id = 1L)

  finite_ratios <- result$stock_mortality_ratio[is.finite(result$stock_mortality_ratio)]
  expect_true(all(finite_ratios >= 0 & finite_ratios <= 1))
})

test_that("mortality_scalers() runs without error on a Chinook database", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  run_id  <- get_run_ids(fram_db)[1]

  expect_no_error(mortality_scalers(fram_db, run_id = run_id, stock_id = 1L))
})

test_that("mortality_scalers() msp = TRUE and msp = FALSE produce different results for Chinook", {
  skip_if_no_test_db()

  fram_db  <- connection_chin_pre(quiet = TRUE)

  withr::defer(disconnect_fram_db(fram_db))

  run_id   <- get_run_ids(fram_db)[1]

  result_msp   <- mortality_scalers(fram_db, run_id = run_id, stock_id = 1L, msp = TRUE)
  result_nomsp <- mortality_scalers(fram_db, run_id = run_id, stock_id = 1L, msp = FALSE)

  expect_false(isTRUE(all.equal(
    sum(result_msp$fishery_mortality,   na.rm = TRUE),
    sum(result_nomsp$fishery_mortality, na.rm = TRUE)
  )))
})

test_that("mortality_scalers() errors on a transfer database (integration)", {
  skip_if_no_test_db()

  fram_db <- connection_chin_transfer(quiet = TRUE)

  withr::defer(disconnect_fram_db(fram_db))

  expect_error(
    mortality_scalers(fram_db, run_id = 1L, stock_id = 1L),
    class = "framrsquared_error"
  )
})
