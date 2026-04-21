test_that("standardize_species works", {
  expect_equal(standardize_species("chin"), "CHINOOK")
  expect_equal(standardize_species("coHo"), "COHO")
  expect_error(standardize_species("Cooho"))
})


test_that("validate_species works",{
  dat1 = data.frame(fishery_id = 1:5,
                    let = letters[1:5])
  dat2 = data.frame(fishery_id = 1:5,
                    let = letters[1:5])
  attr(dat2, "species") <- "CHINOOK"
  expect_equal(validate_species(dat1, "COHO"), "COHO")
  expect_equal(validate_species(dat2), "CHINOOK")
  expect_error(validate_spacies(dat1))
  expect_error(validate_species(dat2, "COHO"))
})


test_that("validate_same_bp works", {
  skip_if_no_test_db()
  local_mocked_bindings(
    validate_fram_db = function(...){TRUE},
    fram_database_type = function(...){list(type = "partial")}
  )
  fram_db <- connection_test_db("partial files/test_validate_same_bp.mdb")

  ## basic error / noerror tests:
  expect_error(validate_same_bp(fram_db,
                                run_ids = c(139, 144)))
  expect_error(validate_same_bp(fram_db,
                                run_ids = c(139, 144, 147, 155)))

  expect_no_error(validate_same_bp(fram_db,
                                run_ids = c(139, 140)))
  expect_no_error(validate_same_bp(fram_db,
                                   run_ids = 153:155))

  expect_error(validate_same_bp(fram_db,
                                run_ids = c(153:155, 139)))

  expect_no_error(validate_same_bp(fram_db,
                                   run_ids = c(153:155, 139),
                                   strict = FALSE)
  )

  ## check output structure

  res <- validate_same_bp(fram_db,
                          run_ids = 153:155)
  expect_equal(names(res),
                c("same_bp", "same_fishery_version",
                  "same_stock_version",
                  "same_time_step_version",
                  "base_periods_df"))
  ## checking the outputs when strict = FALSE or on success
  expect_all_true(c(res$same_bp,
                    res$same_stock_version,
                    res$same_fishery_version,
                    res$same_time_step_version))

  stocks_differ <- validate_same_bp(fram_db,
                                   run_ids = c(139, 144),
                                   strict = FALSE)
  expect_all_true(c(!stocks_differ$same_bp,
                    !stocks_differ$same_stock_version,
                    stocks_differ$same_fishery_version,
                    stocks_differ$same_time_step_version))

  fisheries_differ <- validate_same_bp(fram_db,
                                    run_ids = c(139, 147),
                                    strict = FALSE)
  expect_all_true(c(!fisheries_differ$same_bp,
                    fisheries_differ$same_stock_version,
                    !fisheries_differ$same_fishery_version,
                    fisheries_differ$same_time_step_version))

  timesteps_differ <- validate_same_bp(fram_db,
                                       run_ids = c(139, 150),
                                       strict = FALSE)
  expect_all_true(c(!timesteps_differ$same_bp,
                    timesteps_differ$same_stock_version,
                    timesteps_differ$same_fishery_version,
                    !timesteps_differ$same_time_step_version))

  ## Check dataframe of outputs
  each_different <- validate_same_bp(fram_db,
                                  run_ids = c(139, 144, 147, 150, 153),
                                  strict = FALSE)
  expect_equal(each_different$base_periods_df$base_period_id,
               4:8)
  expect_equal(each_different$base_periods_df$stock_version,
               c(5, 6, 5, 5, 6))
  expect_equal(each_different$base_periods_df$fishery_version,
               c(1, 1, 2, 1, 1))
  expect_equal(each_different$base_periods_df$time_step_version,
               c(1, 1, 1, 2, 2))

  disconnect_fram_db(fram_db)
})
