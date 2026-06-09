mock_mortality_two_runs <- tibble::tibble(
  primary_key       = 1:4,
  run_id            = c(1, 2, 1, 2),
  fishery_id        = c(10L, 10L, 20L, 20L),
  time_step         = c(1L, 1L, 1L, 1L),
  stock_id          = c(1, 1, 1, 1),
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

make_reasonable_mock_chinook_db <- function(return_list = FALSE,
                                            msp_val = 1) {
  # AEQ = 0.5 for both stocks; no terminal fisheries
  table_list <- list(
    Mortality = mock_mortality_two_runs,
    RunID     = tibble::tibble(run_id = c(1, 2), base_period_id = c(1, 2)),
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
      model_stock_proportion = c(msp_val, msp_val)
    )
  )
  if(return_list){
    return(table_list)
  } else {
    return(make_queryable_mock_db_list(table_list, species = "CHINOOK"))
  }
}

## test input validation --------------------------------------

test_that("msp_mortality() errors when `fram_db` is not a fram database", {
  expect_error(msp_mortality(list(a = 1)), class = "framrsquared_error")
  expect_error(msp_mortality(mtcars), class = "framrsquared_error")

})

test_that("msp_mortality() errors when `run_id` is not null or a legal option", {

  fram_db <- make_reasonable_mock_chinook_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(msp_mortality(fram_db, run_id = "A"), class = "framrsquared_error")
  expect_error(msp_mortality(fram_db, run_id = list(1)), class = "framrsquared_error")
  expect_error(msp_mortality(fram_db, run_id = 99), class = "framrsquared_error")
  expect_no_error(msp_mortality(fram_db, run_id = 2))

})

test_that("msp_mortality() errors when fram_db is not chinook db", {
  fram_db <- make_mock_fram_db(type = "full", species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_error(msp_mortality(fram_db, class = "framrsquared_error"))
})

test_that("msp_mortality() errors when fram_db is not full db", {
  fram_db <- make_mock_fram_db(type = "transfer", species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_error(msp_mortality(fram_db, class = "framrsquared_error"))
})

test_that("msp_mortality() respects run_id argument if present", {
  fram_db <- make_reasonable_mock_chinook_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  mort_table <- make_reasonable_mock_chinook_db(return_list = TRUE)$Mortality

  suppressWarnings({
    results <- msp_mortality(fram_db)
  })

  expect_equal(nrow(mort_table), nrow(results))

  results <- msp_mortality(fram_db, run_id = 2)

  expect_equal(sum(mort_table$run_id == 2), nrow(results))
  expect_true(all(results$run_id == 2))
})


test_that("msp_mortality() runs calculation correctly", {
  msp_val  = 0.3
  fram_db <- make_reasonable_mock_chinook_db(msp_val = msp_val)
  withr::defer(disconnect_mock_fram_db(fram_db))

  mort_table <- make_reasonable_mock_chinook_db(return_list = TRUE)$Mortality |>
    dplyr::filter(run_id == 2)

  results <- msp_mortality(fram_db, run_id = 2)

  expect_true(all(results$landed_catch * msp_val ==
                    mort_table$landed_catch)
  )
})

test_that("msp_mortality() errors correctly if runid hasn't been run yet", {
  msp_val = 1
  ## set up table
  table_list <- list(
    Mortality = mock_mortality_two_runs,
    RunID     = tibble::tibble(run_id = c(1, 2, 3), base_period_id = c(1, 2, 2)),
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
      model_stock_proportion = c(msp_val, msp_val)
    ),
    TerminalFisheryFlag = tibble::tibble(
      base_period_id = integer(0),
      fishery_id     = integer(0),
      time_step      = integer(0),
      terminal_flag  = integer(0)
    )
  )

  fram_db <- make_queryable_mock_db_list(table_list, species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(msp_mortality(fram_db, run_id = 3),
               regexp = "must be represented in",
               class = "framrsquared_error"
  )

})

## baseperiod id is not represented in MSP

test_that("msp warns / errors when runs do not have corresponding MSP values",{
  fram_db <- make_reasonable_mock_chinook_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

    expect_warning(msp_mortality(fram_db),
                   class = "framrsquared_warning")

  expect_error(msp_mortality(fram_db, run_id = 1),
               class = "framrsquared_error")
})

