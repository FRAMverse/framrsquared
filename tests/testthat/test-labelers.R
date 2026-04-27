# ── Helpers -------------------------------------------------------------------

make_stocks_mock_db <- function(species = "CHINOOK") {
  run_id  <- data.frame(run_id = 1L, base_period_id = 10L)
  base_id <- data.frame(base_period_id = 10L,
                        stock_version  = 1L,
                        species_name   = species[1])
  stock   <- data.frame(stock_id        = c(1L, 2L),
                        stock_version   = c(1L, 1L),
                        species         = species,
                        stock_long_name = c("Alpha Stock", "Beta Stock"),
                        stock_name      = c("Alpha", "Beta"))

  make_queryable_mock_db_list(
    table_list = list(RunID = run_id, BaseID = base_id, Stock = stock),
    species    = species
  )
}

make_fisheries_mock_db <- function(species = "CHINOOK") {
  run_id  <- data.frame(run_id = 1L, base_period_id = 10L)
  base_id <- data.frame(base_period_id   = 10L,
                        fishery_version  = 1L,
                        species_name     = species[1])
  fishery <- data.frame(fishery_id      = c(1L, 2L),
                        version_number  = c(1L, 1L),
                        species         = species,
                        fishery_title   = c("Troll Fishery", "Net Fishery"),
                        fishery_name    = c("Troll", "Net"))

  make_queryable_mock_db_list(
    table_list = list(RunID = run_id, BaseID = base_id, Fishery = fishery),
    species    = species
  )
}

make_timesteps_mock_db <- function(species = "CHINOOK") {
  time_step <- data.frame(
    time_step_id   = c(1L, 2L, 3L, 4L),
    time_step_name = c("Oct-Apr", "May", "Jun-Jul", "Aug-Sep"),
    species        = rep(species, 4)
  )

  make_queryable_mock_db_list(
    table_list = list(TimeStep = time_step),
    species    = species
  )
}

# UNIT TESTS -------------------------------------------------------------------

## --- label_stocks_db() -------------------------------------------------------

test_that("label_stocks_db() errors on non-dataframe input", {
  fram_db <- make_stocks_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(label_stocks_db(list(run_id = 1, stock_id = 1), fram_db),
               class = "framrsquared_error")
  expect_error(label_stocks_db(c(1, 2, 3), fram_db),
               class = "framrsquared_error")
})

test_that("label_stocks_db() errors on non-full database", {
  fram_db <- make_mock_fram_db(type = "transfer")
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data <- data.frame(run_id = 1L, stock_id = 1L)
  expect_error(label_stocks_db(.data, fram_db), class = "framrsquared_error")
})

test_that("label_stocks_db() adds stock_label column", {
  fram_db <- make_stocks_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(run_id = 1L, stock_id = 1L)
  result <- label_stocks_db(.data, fram_db)

  expect_true("stock_label" %in% names(result))
})

test_that("label_stocks_db() positions stock_label after stock_id", {
  fram_db <- make_stocks_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(run_id = 1L, stock_id = 1L)
  result <- label_stocks_db(.data, fram_db)


  col_positions <- which(names(result) %in% c("stock_id", "stock_label"))
  names(col_positions) = c("stock_id", "stock_label")
  expect_equal(col_positions[["stock_label"]], col_positions[["stock_id"]] + 1L)
})

test_that("label_stocks_db() correctly joins stock labels", {
  fram_db <- make_stocks_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data <- data.frame(run_id   = c(1L, 1L),
                      stock_id = c(1L, 2L))
  result <- label_stocks_db(.data, fram_db)

  expect_equal(result$stock_label, c("Alpha Stock", "Beta Stock"))
})

test_that("label_stocks_db() returns NA stock_label for unmatched rows", {
  fram_db <- make_stocks_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(run_id = 1L, stock_id = 99L)
  result <- label_stocks_db(.data, fram_db)

  expect_true(is.na(result$stock_label))
})

test_that("label_stocks_db() sets species attribute on result", {
  fram_db <- make_stocks_mock_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(run_id = 1L, stock_id = 1L)
  result <- label_stocks_db(.data, fram_db)

  expect_equal(attr(result, "species"), "CHINOOK")
})

test_that("label_stock_db() only joins stocks for the current species", {
  fram_db <- make_stocks_mock_db(species = c("CHINOOK", "COHO"))
  data <- data.frame(run_id = c(1,1), stock_id = c(1,2))
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- label_stocks_db(data, fram_db)

  expect_equal(result$stock_label[1], "Alpha Stock")
  expect_true(is.na(result$stock_label[2]))
})


test_that("label_stocks_db() only joins stocks for the current base period", {

  run_id  <- data.frame(run_id = 1L, base_period_id = 10L)
  base_id <- data.frame(base_period_id = 10L,
                        stock_version  = 2,
                        species_name   = "CHINOOK")
  stock   <- data.frame(stock_id        = c(1L, 2L, 1, 2),
                        stock_version   = c(1L, 1L, 2, 2),
                        species         = "CHINOOK",
                        stock_long_name = c("Alpha Stock", "Beta Stock", "Gamma Stock", "Zeta Stock"),
                        stock_name      = c("Alpha", "Beta", "Gamma", "Zeta"))

  fram_db <- make_queryable_mock_db_list(
    table_list = list(RunID = run_id, BaseID = base_id, Stock = stock),
    species    = "CHINOOK"
  )

  data <- data.frame(run_id = c(1,1), stock_id = c(1,2))
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- label_stocks_db(data, fram_db)

  expect_equal(result$stock_label, c("Gamma Stock", "Zeta Stock"))
})



## --- label_fisheries_db() ----------------------------------------------------

test_that("label_fisheries_db() errors on non-dataframe input", {
  fram_db <- make_fisheries_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(label_fisheries_db(list(run_id = 1, fishery_id = 1), fram_db),
               class = "framrsquared_error")
  expect_error(label_fisheries_db(c(1, 2, 3), fram_db),
               class = "framrsquared_error")
})

test_that("label_fisheries_db() errors on non-full database", {
  fram_db <- make_mock_fram_db(type = "transfer")
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data <- data.frame(run_id = 1L, fishery_id = 1L)
  expect_error(label_fisheries_db(.data, fram_db), class = "framrsquared_error")
})

test_that("label_fisheries_db() adds fishery_label column", {
  fram_db <- make_fisheries_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(run_id = 1L, fishery_id = 1L)
  result <- label_fisheries_db(.data, fram_db)

  expect_true("fishery_label" %in% names(result))
})

test_that("label_fisheries_db() positions fishery_label after fishery_id", {
  fram_db <- make_fisheries_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(run_id = 1L, fishery_id = 1L)
  result <- label_fisheries_db(.data, fram_db)

  col_positions <- which(names(result) %in% c("fishery_id", "fishery_label"))
  names(col_positions) <- c("fishery_id", "fishery_label")
  expect_equal(col_positions[["fishery_label"]], col_positions[["fishery_id"]] + 1L)
})

test_that("label_fisheries_db() correctly joins fishery labels", {
  fram_db <- make_fisheries_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data <- data.frame(run_id     = c(1L, 1L),
                      fishery_id = c(1L, 2L))
  result <- label_fisheries_db(.data, fram_db)

  expect_equal(result$fishery_label, c("Troll Fishery", "Net Fishery"))
})

test_that("label_fisheries_db() returns NA fishery_label for unmatched rows", {
  fram_db <- make_fisheries_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(run_id = 1L, fishery_id = 99L)
  result <- label_fisheries_db(.data, fram_db)

  expect_true(is.na(result$fishery_label))
})

test_that("label_fisheries_db() sets species attribute on result", {
  fram_db <- make_fisheries_mock_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(run_id = 1L, fishery_id = 1L)
  result <- label_fisheries_db(.data, fram_db)

  expect_equal(attr(result, "species"), "COHO")
})

test_that("label_fisheries_db() only joins fisheries for the current species", {
  fram_db <- make_fisheries_mock_db(species = c("CHINOOK", "COHO"))
  data <- data.frame(run_id = c(1,1), fishery_id = c(1,2))
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- label_fisheries_db(data, fram_db)

  expect_equal(result$fishery_label[1], "Troll Fishery")
  expect_true(is.na(result$fishery_label[2]))
})

test_that("label_fisheries_db() only joins fisheries for the current base period", {

  run_id  <- data.frame(run_id = 1L, base_period_id = 10L)
  base_id <- data.frame(base_period_id = 10L,
                        fishery_version  = 2,
                        species_name   = "CHINOOK")
  fishery <- data.frame(fishery_id      = c(1L, 2L, 1, 2),
                        version_number  = c(1L, 1L, 2, 2),
                        species         = "CHINOOK",
                        fishery_title   = c("Troll Fishery", "Net Fishery", "Test Fishery", "Nemo Fishery"),
                        fishery_name    = c("Troll", "Net", "Test", "Nemo"))

  fram_db <- make_queryable_mock_db_list(
    table_list = list(RunID = run_id, BaseID = base_id, Fishery = fishery),
    species    = "CHINOOK"
  )
  data <- data.frame(run_id = c(1,1), fishery_id = c(1,2))
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- label_fisheries_db(data, fram_db)

  expect_equal(result$fishery_label, c("Test Fishery", "Nemo Fishery"))
})

## --- label_timesteps_db() ----------------------------------------------------

test_that("label_timesteps_db() errors on non-dataframe input", {
  fram_db <- make_timesteps_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(label_timesteps_db(list(time_step = 1L), fram_db),
               class = "framrsquared_error")
  expect_error(label_timesteps_db(1:5, fram_db),
               class = "framrsquared_error")
})

test_that("label_timesteps_db() errors on non-full database", {
  fram_db <- make_mock_fram_db(type = "transfer")
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data <- data.frame(time_step = 1L)
  expect_error(label_timesteps_db(.data, fram_db), class = "framrsquared_error")
})

test_that("label_timesteps_db() adds time_step_label column", {
  fram_db <- make_timesteps_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(time_step = 1L)
  result <- label_timesteps_db(.data, fram_db)

  expect_true("time_step_label" %in% names(result))
})

test_that("label_timesteps_db() returns time_step_label as a factor", {
  fram_db <- make_timesteps_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(time_step = 1L)
  result <- label_timesteps_db(.data, fram_db)

  expect_s3_class(result$time_step_label, "factor")
})

test_that("label_timesteps_db() formats labels as '{id} ({name})'", {
  fram_db <- make_timesteps_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  .data  <- data.frame(time_step = 1L)
  result <- label_timesteps_db(.data, fram_db)

  expect_equal(as.character(result$time_step_label), "1 (Oct-Apr)")
})

test_that("label_timesteps_db() only joins timesteps for the current species", {
  time_step <- data.frame(
    time_step_id   = c(1L, 2L, 1L, 2L),
    time_step_name = c("Oct-Apr", "May", "T1", "T2"),
    species        = c("CHINOOK", "CHINOOK", "COHO", "COHO")
  )
  chin_db <- make_queryable_mock_db_list(
    table_list = list(TimeStep = time_step),
    species    = "CHINOOK"
  )
  withr::defer(disconnect_mock_fram_db(chin_db))

  result <- label_timesteps_db(data.frame(time_step = 1L), chin_db)

  expect_equal(as.character(result$time_step_label), "1 (Oct-Apr)")
  expect_false(as.character(result$time_step_label) == "1 (T1)")
})

## --- label_flags() -----------------------------------------------------------

test_that("label_flags() errors on non-dataframe input", {
  expect_error(label_flags(list(fishery_flag = 1L)), class = "framrsquared_error")
  expect_error(label_flags(c(1L, 2L, 3L)),          class = "framrsquared_error")
})

test_that("label_flags() warns when neither flag column is present", {
  .data <- data.frame(run_id = 1L)
  attr(.data, "species") <- "CHINOOK"

  expect_message(label_flags(.data, warn = TRUE),
                 regexp = "Missing")
})

test_that("label_flags() does not warn when warn = FALSE and no flag columns", {
  .data <- data.frame(run_id = 1L)
  attr(.data, "species") <- "CHINOOK"

  expect_no_message(label_flags(.data, warn = FALSE))
})

test_that("label_flags() returns input unchanged when no flag columns present", {
  .data <- data.frame(run_id = 1L)
  attr(.data, "species") <- "CHINOOK"

  result <- label_flags(.data, warn = FALSE)
  expect_equal(names(result), names(.data))
})

test_that("label_flags() adds fishery_flag_label when fishery_flag is present", {
  .data <- data.frame(fishery_flag = c(0L, 1L, 2L))
  attr(.data, "species") <- "CHINOOK"

  result <- label_flags(.data)
  expect_true("fishery_flag_label" %in% names(result))
})

test_that("label_flags() fishery_flag_label has correct translations", {
  .data <- data.frame(fishery_flag = c(0L, 1L, 2L, 7L, 8L))
  attr(.data, "species") <- "CHINOOK"

  result <- label_flags(.data)
  expect_equal(result$fishery_flag_label,
               c("ZERO", "Fishery Scaler", "Fishery Quota", "MSF Scaler", "MSF Quota"))
})

test_that("label_flags() adds non_retention_flag_label for CHINOOK", {
  .data <- data.frame(non_retention_flag = 0:4)
  attr(.data, "species") <- "CHINOOK"

  result <- label_flags(.data)
  expect_true("non_retention_flag_label" %in% names(result))
  expect_equal(result$non_retention_flag_label,
               c("ZERO", "Computed CNR", "Ratio of CNR Days",
                 "Legal/Sublegal Encounters", "Total Encounters"))
})

test_that("label_flags() sets non_retention_flag_label to 'Total dead fish' for COHO", {
  .data <- data.frame(non_retention_flag = 0:2)
  attr(.data, "species") <- "COHO"

  result <- label_flags(.data)
  expect_true("non_retention_flag_label" %in% names(result))
  expect_true(all(result$non_retention_flag_label == "Total dead fish"))
})

test_that("label_flags() handles both flag columns simultaneously", {
  .data <- data.frame(fishery_flag       = c(1L, 2L),
                      non_retention_flag = c(0L, 1L))
  attr(.data, "species") <- "CHINOOK"

  result <- label_flags(.data)
  expect_true("fishery_flag_label" %in% names(result))
  expect_true("non_retention_flag_label" %in% names(result))
})

test_that("label_flags() uses species argument when attribute is absent", {
  .data <- data.frame(non_retention_flag = 0:2)

  result <- label_flags(.data, species = "COHO")
  expect_true(all(result$non_retention_flag_label == "Total dead fish"))
})
