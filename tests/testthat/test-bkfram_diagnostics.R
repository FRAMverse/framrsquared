## filepath to built-in demo file.
filepath = system.file("BackFramCheck.Txt", package = "framrsquared.dev")

## parse_bkfram_check -----------------------------------

test_that("parse_bkfram_check input validation", {
  expect_no_error(parse_bkfram_check(filepath))
  expect_error(parse_bkfram_check("nonexistent.txt"),
               class = "framrsquared_error")
  expect_error(parse_bkfram_check(10),
               class = "framrsquared_error")
})

test_that("parse_bkfram_check output is a tibble", {
  dat <- parse_bkfram_check(filepath)
  expect_s3_class(dat, "tbl_df")
})

test_that("parse_bkfram_check column names are consistent", {
  dat <- parse_bkfram_check(filepath)
  expect_named(dat, c("iteration", "stock_id", "escapement", "escapement_target",
                      "esc_target_ratio", "old_scalar", "new_scalar", "starting_cohort",
                      "stock_name") )
})

test_that("parse_bkfram_check rows make sense", {
  ## should have the same number of each iteration entry, the same number of each stock entry, and those should
  ## have reciprocal values.
  dat <- parse_bkfram_check(filepath)

  vals.iters = table(table(dat$iteration))
  expect_equal(length(vals.iters), 1)

  vals.stocks = table(table(dat$stock_id))
  expect_equal(length(vals.stocks), 1)

  expect_equal(as.numeric(vals.iters), as.numeric(names(vals.stocks)))
  expect_equal(as.numeric(vals.stocks), as.numeric(names(vals.iters)))

})

test_that("read of test file is consistent", {
  dat <- parse_bkfram_check(filepath)
  expect_snapshot_value(dat, style = "json2")
})

## numerify_text ---------------------------------------------------------------

test_that("numerify text correctly parses FRAM check text", {
  x = c(1, 2, "*", "NaN", "-")
  expect_equal(numerify_text(x), c(1, 2, NA, NA, NA))
  expect_true(is.numeric(numerify_text(x)))

  expect_warning(numerify_text(letters))
})

## aggregate_bkfram_check --------------------------------------------------------

test_that("aggregate_bkfram_check returns tibble", {
  dat <- parse_bkfram_check(filepath)
  dat2 <- aggregate_bkfram_check(dat)

  expect_s3_class(dat2, "tbl_df")
})

test_that("aggregate_bkfram_check columns match expectation", {
  raw <- parse_bkfram_check(filepath)
  dat <- aggregate_bkfram_check(raw)
  expect_named(dat,
               c("iteration", "stock_aggregate_name", "stock_aggregate_id",
                 "escapement", "escapement_target", "starting_cohort", "stock_id",
                 "stock_name", "missing_start_cohort", "esc_target_ratio")
  )
})

test_that("aggregate_bkfram_check results pass gut check", {
  raw <- parse_bkfram_check(filepath)
  dat <- aggregate_bkfram_check(raw)
  expect_equal(nrow(dat) * 2, nrow(raw))
  expect_equal(sum(dat$escapement), sum(raw$escapement))
  expect_equal(sum(dat$escapement_target), sum(raw$escapement_target))
})

test_that("aggregate_bkfram_check mapping is correct", {
  raw <- parse_bkfram_check(filepath)
  dat <- aggregate_bkfram_check(raw)

  raw_lut <- raw |>
    dplyr::select("stock_id", raw_stock_name = "stock_name") |>
    dplyr::distinct()


  dat_unnested <- tidyr::unnest(dat, c(stock_id, stock_name)) |>
    dplyr::left_join(raw_lut, by = "stock_id") |>
    dplyr::select("stock_aggregate_name", "stock_aggregate_id", "stock_id", "stock_name") |>
    ## recreate names. Do they match?
    dplyr::mutate(stock_name_test = gsub("*.-", "", stock_name),
                  ##approx recreate stock ids. Should match within 1.
                  stock_id_test = stock_aggregate_id * 2)

  expect_all_true(dat_unnested$stock_aggregate_name == dat_unnested$stock_name_test)
  expect_all_true( abs(dat_unnested$stock_id_test - dat_unnested$stock_id) <= 1)
})

## plot_bkfram_convergence_trace --------------------------------------------------

test_that("plot_bkfram_convergence_trace input validation", {
  expect_no_error(plot_bkfram_convergence_trace(filepath, stock_id = 10))
  expect_error(plot_bkfram_convergence_trace(10, stock_id = 10),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_trace(filepath, stock_id = "ten"),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_trace(10, stock_id = 10, aggregate_stocks = 1),
               class = "framrsquared_error")
})

test_that("plot_bkfram_convergence_trace output", {
  out <- plot_bkfram_convergence_trace(filepath, stock_id = 10)
  expect_s3_class(out, "ggplot")
})

test_that("plot_bkfram_convergence_trace handles `stock` correctly", {
  out <- plot_bkfram_convergence_trace(filepath, stock_id = 1:2)
  dat <- out$data

  expect_equal(length(unique(dat$stock_label)), 1)

  out <- plot_bkfram_convergence_trace(filepath, stock_id = 1:6)
  dat <- out$data

  expect_equal(length(unique(dat$stock_label)), 3)


  out <- plot_bkfram_convergence_trace(filepath, stock_id = 1:2, aggregate_stocks = FALSE)
  dat <- out$data

  expect_equal(length(unique(dat$stock_label)), 2)

})



## plot_bkfram_convergence_bar --------------------------------------------------

test_that("plot_bkfram_convergence_bar works with valid args", {
  expect_no_error(plot_bkfram_convergence_bar(filepath,
                                              iteration = 10,
                                              plot_ratio = TRUE,
                                              aggregate_stocks = TRUE,
                                              thresh = 0.01,
                                              verbose = TRUE
  ))

  expect_no_error(plot_bkfram_convergence_bar(filepath,
                                              iteration = 10,
                                              plot_ratio = FALSE,
                                              aggregate_stocks = FALSE,
                                              thresh = 0.02,
                                              verbose = FALSE
  ))
})

test_that("plot_bkfram_convergence_bar input validation", {

  expect_error(plot_bkfram_convergence_bar(10),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_bar(filepath, iteration = "ten"),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_bar(filepath, iteration = 1:5),
               class = "framrsquared_error")


  expect_error(plot_bkfram_convergence_bar(filepath, iteration = 10, plot_ratio = c(TRUE, FALSE)),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_bar(filepath, iteration = 10, plot_ratio = 10),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_bar(filepath, iteration = 10, aggregate_stocks = c(TRUE, FALSE)),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_bar(filepath, iteration = 10, aggregate_stocks = 10),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_bar(filepath, iteration = 10, thresh = TRUE),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_bar(filepath, iteration = 10, thresh = 1:5),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_bar(filepath, iteration = 10, verbose = c(TRUE, FALSE)),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_bar(filepath, iteration = 10, verbose = 10),
               class = "framrsquared_error")

})


test_that("plot_bkfram_convergence_bar aggregation and non-aggregation works", {
  expect_no_error(plot_bkfram_convergence_bar(filepath,
                                              iteration = 10,
                                              aggregate_stocks = TRUE))

  expect_no_error(plot_bkfram_convergence_bar(filepath,
                                              iteration = 10,
                                              aggregate_stocks = FALSE))

})


test_that("plot_bkfram_convergence_bar messages on all convergence", {
  expect_message(plot_bkfram_convergence_bar(filepath),
                 regexp = "No stocks outside of"
  )

})


## plot_bkfram_convergence_scatter --------------------------------------------------

test_that("plot_bkfram_convergence_scatter works with valid args", {
  expect_no_error(plot_bkfram_convergence_scatter(filepath,
                                                  iteration = 30,
                                                  aggregate_stocks = TRUE,
                                                  thresh = 0.01,
                                                  verbose = TRUE,
                                                  max_n = 11,
                                                  label_size = 5
  ))

  expect_no_error(plot_bkfram_convergence_scatter(filepath,
                                                  iteration = 50,
                                                  aggregate_stocks = FALSE,
                                                  thresh = 0.05,
                                                  verbose = FALSE,
                                                  max_n = 20,
                                                  label_size = 3
  ))
})

test_that("plot_bkfram_convergence_scatter input validation", {

  expect_error(plot_bkfram_convergence_scatter(10),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = "ten"),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 1:5),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, aggregate_stocks = c(TRUE, FALSE)),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, aggregate_stocks = 10),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, thresh = TRUE),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, thresh = 1:5),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, verbose = c(TRUE, FALSE)),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, verbose = 10),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, max_n = TRUE),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, max_n = 1:5),
               class = "framrsquared_error")

  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, label_size = TRUE),
               class = "framrsquared_error")
  expect_error(plot_bkfram_convergence_scatter(filepath, iteration = 10, label_size = 1:5),
               class = "framrsquared_error")
})


test_that("plot_bkfram_convergence_scatter max_n works", {

  expect_message(
    gp <- plot_bkfram_convergence_scatter(filepath,
                                          iteration = 3),
    regexp = "Plotting most extreme 10"
  )
  expect_equal(nrow(gp$data), 10)

  expect_message(
    gp <- plot_bkfram_convergence_scatter(filepath,
                                          iteration = 3,
                                          max_n = 15),
    regexp = "Plotting most extreme 15"
  )
  expect_equal(nrow(gp$data), 15)

})

test_that("plot_bkfram_convergence_scatter aggregation and non-aggregation works", {
  expect_no_error(plot_bkfram_convergence_scatter(filepath,
                                                  iteration = 10,
                                                  aggregate_stocks = TRUE))

  expect_no_error(plot_bkfram_convergence_scatter(filepath,
                                                  iteration = 50,
                                                  aggregate_stocks = FALSE))

})



test_that("plot_bkfram_convergence_scatter label_size works", {
  gp <- plot_bkfram_convergence_scatter(filepath,
                                        iteration = 10,
                                        aggregate_stocks = TRUE,
                                        label_size = 6)

  expect_equal(gp$layers$geom_label_repel$aes_params[[1]], 6)

  gp <- plot_bkfram_convergence_scatter(filepath,
                                        iteration = 10,
                                        aggregate_stocks = TRUE,
                                        label_size = 2)

  expect_equal(gp$layers$geom_label_repel$aes_params[[1]], 2)

})

test_that("plot_bkfram_convergence_scatter messages on all convergence", {
  expect_message(plot_bkfram_convergence_scatter(filepath),
                 regexp = "No stocks outside of"
  )

})
