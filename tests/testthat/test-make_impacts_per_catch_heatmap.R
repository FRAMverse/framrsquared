## functions

make_reasonable_mock_chinook_db <- function(return_list = FALSE){
  ## fisheries 23 and 24
  ## stocks 1 and 2
  ## run_id 1, time_step 1
  ## WITHOUT AEQ:
  ## fishery 23 is 10% stock 1
  ## fishery 24 is 50% stock 1
  ## AEQ will be halve the effective catch of stock 2, so WITH AEQ:
  ## MSP doesn't matter, since it's a fishery-wide multiplier.

  ## dummy term table so the function doesn't error.
  mock_terminal_fishery_flag = data.frame(base_period_id = 1,
                                          fishery_id = 1000,
                                          time_step = 3,
                                          terminal_flag = 1)
  mock_aeq = data.frame(base_period_id = c(1, 1),
                        stock_id = 1:2,
                        age = c(3, 3),
                        time_step = c(2, 2),
                        aeq = c(1, .5),
                        primary_key = 1:2)
  mock_msp = data.frame(base_period_id = c(1, 1),
                        fishery_id = 23:24,
                        model_stock_proportion = c(1, 1),
                        primary_key = 10:11)

  mock_runs = data.frame(run_id = 1,
                         base_period_id = 1,
                         run_title = "run 1",
                         run_time_date = Sys.time(),
                         primary_key = 33
  )
  mock_fishery = data.frame(fishery_id = 23:24, ## COHO fisheries that pass the filter
                            fishery_name = c("Fishery A", "Fishery B"))
  mock_stocks = data.frame(stock_id = 1:3,
                           stock_name = c("stock 1", "stock 2", "mia stock 3"),
                           stock_long_name = c("stockton 1", "stockton 2", "miasma stockton 3"))
  mock_timesteps = data.frame(time_step_id = 1:5,
                              version_number = 1,
                              time_step_name = LETTERS[1:5],
                              time_step_title = LETTERS[6:10]) |>
    dplyr::mutate(species = "COHO")
  mock_mortality = expand.grid(run_id = 1,
                               stock_id = 1:2,
                               fishery_id = 23:24,
                               time_step = 2
  ) |>
    dplyr::mutate(age = 3,
                  non_retention = 0,
                  shaker = 0,
                  drop_off = 0,
                  encounter = 0,
                  msf_landed_catch = 0,
                  msf_non_retention = 0,
                  msf_shaker = 0,
                  msf_drop_off = 0,
                  msf_encounter = 0,
                  primary_key = 11:(10+length(age)))

  mock_mortality_lut = tibble::tribble(
    ~fishery_id, ~stock_id, ~landed_catch,
    23, 1, 1, ##fishery 23 will be hitting 90% stock 2, 10% stock 1
    23, 2, 9,
    24, 1, 2, ##fishery 24 will hit 50:50 stock 1 and 2
    24, 2, 2
  )

  mock_mortality <- mock_mortality |>
    dplyr::left_join(mock_mortality_lut, by = c("fishery_id", "stock_id")) |>
    dplyr::relocate(landed_catch, .after = "age")



  table_list <- list(RunID = mock_runs,
                     AEQ = mock_aeq,
                     FisheryModelStockProportion = mock_msp,
                     TerminalFisheryFlag = mock_terminal_fishery_flag,
                     Fishery = mock_fishery,
                     Stock = mock_stocks,
                     TimeStep = mock_timesteps,
                     Mortality = mock_mortality)
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

make_reasonable_mock_coho_db <- function(return_list = FALSE){
  mock_runs = data.frame(run_id = 1,
                         base_period_id = 1,
                         run_title = "run 1",
                         run_time_date = Sys.time()
  )
  mock_fishery = data.frame(fishery_id = 23:24, ## COHO fisheries that pass the filter
                            fishery_name = c("Fishery A", "Fishery B"))
  mock_stocks = data.frame(stock_id = 1:3,
                           stock_name = c("stock 1", "stock 2", "mia stock 3"),
                           stock_long_name = c("stockton 1", "stockton 2", "miasma stockton 3"))
  mock_timesteps = data.frame(time_step_id = 1:5,
                              version_number = 1,
                              time_step_name = LETTERS[1:5],
                              time_step_title = LETTERS[6:10]) |>
    dplyr::mutate(species = "COHO")
  mock_mortality = expand.grid(run_id = 1,
                               stock_id = 1:2,
                               fishery_id = 23:24,
                               time_step = 1
  ) |>
    dplyr::mutate(age = 3,
                  non_retention = 0,
                  shaker = 0,
                  drop_off = 0,
                  encounter = 0,
                  msf_landed_catch = 0,
                  msf_non_retention = 0,
                  msf_shaker = 0,
                  msf_drop_off = 0,
                  msf_encounter = 0)

  mock_mortality_lut = tibble::tribble(
    ~fishery_id, ~stock_id, ~landed_catch,
    23, 1, 1, ##fishery 23 will be hitting 90% stock 2, 10% stock 1
    23, 2, 9,
    24, 1, 2, ##fishery 24 will hit 50:50 stock 1 and 2
    24, 2, 2
  )

  mock_mortality <- mock_mortality |>
    dplyr::left_join(mock_mortality_lut, by = c("fishery_id", "stock_id")) |>
    dplyr::relocate(landed_catch, .after = "age")



  table_list <- list(RunID = mock_runs,
                     Fishery = mock_fishery,
                     Stock = mock_stocks,
                     TimeStep = mock_timesteps,
                     Mortality = mock_mortality,
                     FisheryModelStockProportion = data.frame(base_period_id = 0,
                                                              fishery_id = 0,
                                                              model_stock_proportion = 0))
  if(return_list){
    return(table_list)
  } else {
    return(make_queryable_mock_db_list(
      table_list = table_list,
      species = "COHO"
    )
    )
  }
}


## Test input validation-----------------------------------------------

## Note: want a valid sovleable mock database before trying to detect errors, otehrwise might get "false positive" errors
## Also note: need to set filters_list = NULL to avoid actual error unless using fisheries present in the
test_that(desc = "input validations correctly error out", {
  fram_db <- make_reasonable_mock_coho_db()

  withr::defer(disconnect_mock_fram_db(fram_db))

  # plot_impacts_per_catch_heatmap(fram_db, run_id = 1, stock_id = 1)

  ## fram_db
  expect_error(plot_impacts_per_catch_heatmap(fram_db = 10, run_id = 1, stock_id = 1),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = list(10), run_id = 1, stock_id = 1),
               class = "framrsquared_error")

  ## run_id

  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = "one", stock_id = 1),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1:3, stock_id = 1),
               class = "framrsquared_error")

  ## stock_id
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = "one"),
               class = "framrsquared_error")

  ## filters_list
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1,
                                              filters_list = 10),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1,
                                              filters_list = list(sum, "ten")),
               class = "framrsquared_error")

  ## filter_out
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, filter_out = "ten"),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, filter_out = list(23)),
               class = "framrsquared_error")

  ## msp
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, msp = "TEN"),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, msp = c(TRUE, FALSE)),
               class = "framrsquared_error")

  ## digits_round
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, digits_round = "ten"),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, digits_round = 1:5),
               class = "framrsquared_error")

  ## outer_text_size
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, outer_text_size = "ten"),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, outer_text_size = 1:5),
               class = "framrsquared_error")

  ## cell_text_size
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, cell_text_size = "ten"),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, cell_text_size = 1:5),
               class = "framrsquared_error")

  ## short_title
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, short_title = "TEN"),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, short_title = c(TRUE, FALSE)),
               class = "framrsquared_error")

  ## per_thousand_catch
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, per_thousand_catch = "TEN"),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, per_thousand_catch = c(TRUE, FALSE)),
               class = "framrsquared_error")

  ## verbose
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, verbose = "TEN"),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, verbose = c(TRUE, FALSE)),
               class = "framrsquared_error")

  ## warn
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, warn = "TEN"),
               class = "framrsquared_error")
  expect_error(plot_impacts_per_catch_heatmap(fram_db = fram_db, run_id = 1, stock_id = 1, warn = c(TRUE, FALSE)),
               class = "framrsquared_error")

})


test_that("make_impacts_per_catch_heatmap values are correct", {

  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  ## with per_thousand_catch = FALSE -----------
  suppressMessages({
    out <- plot_impacts_per_catch_heatmap(fram_db, run_id = 1, stock_id = 1)
  })

  expect_true("ggplot2::ggplot" %in% class(out))

  ## we made the first fishery a 1:9 ratio, so 10 quota to make 1 impact
  first_fishery_impacts <- out$data |>
    dplyr::filter(fishery_id == 23) |>
    dplyr::pull(catch_per_impact)
  expect_equal(first_fishery_impacts, 10)
  ## we made the second fishery a 1:1 ratio, so 2 quota to make 1 impact
  second_fishery_impacts <- out$data |>
    dplyr::filter(fishery_id == 24) |>
    dplyr::pull(catch_per_impact)
  expect_equal(second_fishery_impacts, 2)

  ## with per_thousand_catch = TRUE ------------

  suppressMessages({
    out <- plot_impacts_per_catch_heatmap(fram_db, run_id = 1, stock_id = 1,
                                          per_thousand_catch = TRUE)
  })


  ## we made the first fishery a 1:9 ratio, so 10 quota to make 1 impact
  first_fishery_impacts <- out$data |>
    dplyr::filter(fishery_id == 23) |>
    dplyr::pull(catch_per_impact)
  expect_equal(first_fishery_impacts, 100)
  ## we made the second fishery a 1:1 ratio, so 2 quota to make 1 impact
  second_fishery_impacts <- out$data |>
    dplyr::filter(fishery_id == 24) |>
    dplyr::pull(catch_per_impact)
  expect_equal(second_fishery_impacts, 500)
})


test_that("filter_out works correctly", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  ## with per_thousand_catch = FALSE -----------
  suppressMessages({
    out <- plot_impacts_per_catch_heatmap(fram_db, run_id = 1, stock_id = 1, filter_out = 23)
  })

  all_fishery_ids <- out$data$fishery_id
  expect_equal(all_fishery_ids, 24)

})

test_that("Custom filters work", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  ## with per_thousand_catch = FALSE -----------
  filter_fun_remove_f24 = function(.data){
    .data |>
      dplyr::filter(.data$fishery_id != 24)
  }
  suppressMessages({
    out <- plot_impacts_per_catch_heatmap(fram_db, run_id = 1, stock_id = 1, filters_list = list(filter_fun_remove_f24))
  })

  all_fishery_ids <- out$data$fishery_id
  expect_equal(all_fishery_ids, 23)

})

test_that("warn works", {
  fram_db <- make_reasonable_mock_coho_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages({
    expect_message(plot_impacts_per_catch_heatmap(fram_db, run_id = 1, stock_id = 1:2),
                   regexp = "Multiple stock IDs")
  })

  expect_no_message(plot_impacts_per_catch_heatmap(fram_db, run_id = 1, stock_id = 1:2,
                                                   warn = FALSE, verbose = FALSE))


})


## fisheries 23 and 24
## stocks 1 and 2
## run_id 1, time_step 1
## WITHOUT AEQ:
## fishery 23 is 10% stock 1
## fishery 24 is 50% stock 1
## AEQ will be halve the effective catch of stock 2, so WITH AEQ:
## fishery 23 is ~18% stock 1
## fishery 24 is ~66% stock 1.
##
## Okay, but that math isn't enough.
## In area 23, when we catch 1000 fish, we are catching
##  100 stock 1 + 900 stock 2
##  But our impacts are 100 stock 1 + 450 stock 2.
## And in area 24, when we catch 1000 fish, we are catching
## 500 stock 1 + 500 stock 2
##  But our impacts are 500 stock 1 + 250 stock 2.

test_that("AEQ is respected", {
  fram_db <- make_reasonable_mock_chinook_db()
  withr::defer(disconnect_mock_fram_db((fram_db)))

  out <- plot_impacts_per_catch_heatmap(fram_db, run_id = 1, stock_id = 2,
                                        filters_list = NULL, per_thousand_catch = TRUE,
                                        verbose = FALSE)
  first_fishery_impacts <- out$data |>
    dplyr::filter(fishery_id == 23) |>
    dplyr::pull(catch_per_impact)
  expect_equal(first_fishery_impacts, 450)
  ## we made the second fishery a 1:1 ratio, so 2 quota to make 1 impact
  second_fishery_impacts <- out$data |>
    dplyr::filter(fishery_id == 24) |>
    dplyr::pull(catch_per_impact)
  expect_equal(second_fishery_impacts, 250)
})


