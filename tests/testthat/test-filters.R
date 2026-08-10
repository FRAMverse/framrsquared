## filters.R tests

## Helpers ------------------------------------------------------------------

# Minimal fishery data frame with species attribute for testing.
# Contains all possible fishery IDs (1:200) so every filter hits some rows.
make_fishery_df <- function(species) {
  df <- tibble::tibble(fishery_id = 1:200)
  attr(df, "species") <- species
  df
}

# Minimal stock data frame with species attribute for testing.
make_stock_df <- function(species) {
  df <- tibble::tibble(stock_id = 1:100)
  attr(df, "species") <- species
  df
}


## Input validation ----------------------------------------------------------

test_that("fishery filters error when .data is not a data frame", {
  expect_error(filter_sport(list(), species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_net(list(), species = "CHINOOK"), class = "framrsquared_error")
})

test_that("fishery filters error when fishery_id column is missing", {
  df <- tibble::tibble(x = 1)
  expect_error(filter_sport(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_net(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_puget_sound(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_wa(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_bc(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_ak(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_ca(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_or(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_coast(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_marine(df, species = "CHINOOK"), class = "framrsquared_error")
  expect_error(filter_commercial_wa_nt(df, species = "CHINOOK"), class = "framrsquared_error")
})

test_that("fishery filters error when species cannot be determined", {
  # No species attribute and no species arg
  df <- tibble::tibble(fishery_id = 1:10)
  expect_error(filter_sport(df), class = "framrsquared_error")
  expect_error(filter_net(df), class = "framrsquared_error")
})

test_that("fishery filter errors when species arg conflicts with data attribute", {
  df <- make_fishery_df("CHINOOK")
  expect_error(filter_sport(df, species = "COHO"), class = "framrsquared_error")
})

test_that("filter_stt() errors for CHINOOK", {
  expect_error(
    filter_stt(make_fishery_df("CHINOOK")),
    class = "framrsquared_error"
  )
})

test_that("filter_stt_nt() errors for CHINOOK", {
  expect_error(
    filter_stt_nt(make_fishery_df("CHINOOK")),
    class = "framrsquared_error"
  )
})

test_that("stock filters error when stock_id column is missing", {
  df <- tibble::tibble(x = 1)
  attr(df, "species") <- "COHO"
  expect_error(filter_hatchery(df), class = "framrsquared_error")
  expect_error(filter_wild(df), class = "framrsquared_error")
  expect_error(filter_mixed(df), class = "framrsquared_error")
})

test_that("stock filters error for CHINOOK", {
  df <- make_stock_df("CHINOOK")
  expect_error(filter_hatchery(df), class = "framrsquared_error")
  expect_error(filter_wild(df), class = "framrsquared_error")
  expect_error(filter_mixed(df), class = "framrsquared_error")
})


## filter_sport() -----------------------------------------------------------

test_that("filter_sport() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_sport(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_sport() returns expected IDs for COHO", {
  expect_snapshot_value(filter_sport(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_sport() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("CHINOOK")
  ids <- filter_sport(df, return_ids = TRUE)
  result <- filter_sport(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_net() -----------------------------------------------------------

test_that("filter_net() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_net(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_net() returns expected IDs for COHO", {
  expect_snapshot_value(filter_net(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_net() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("COHO")
  ids <- filter_net(df, return_ids = TRUE)
  result <- filter_net(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_puget_sound() -----------------------------------------------------

test_that("filter_puget_sound() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_puget_sound(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_puget_sound() returns expected IDs for COHO", {
  expect_snapshot_value(filter_puget_sound(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_puget_sound() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("CHINOOK")
  ids <- filter_puget_sound(df, return_ids = TRUE)
  result <- filter_puget_sound(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_wa() --------------------------------------------------------------

test_that("filter_wa() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_wa(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_wa() returns expected IDs for COHO", {
  expect_snapshot_value(filter_wa(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_wa() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("COHO")
  ids <- filter_wa(df, return_ids = TRUE)
  result <- filter_wa(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_bc() --------------------------------------------------------------

test_that("filter_bc() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_bc(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_bc() returns expected IDs for COHO", {
  expect_snapshot_value(filter_bc(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_bc() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("CHINOOK")
  ids <- filter_bc(df, return_ids = TRUE)
  result <- filter_bc(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_ak() --------------------------------------------------------------

test_that("filter_ak() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_ak(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_ak() returns expected IDs for COHO", {
  expect_snapshot_value(filter_ak(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_ak() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("COHO")
  ids <- filter_ak(df, return_ids = TRUE)
  result <- filter_ak(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_ca() --------------------------------------------------------------

test_that("filter_ca() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_ca(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_ca() returns expected IDs for COHO", {
  expect_snapshot_value(filter_ca(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_ca() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("CHINOOK")
  ids <- filter_ca(df, return_ids = TRUE)
  result <- filter_ca(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_or() --------------------------------------------------------------

test_that("filter_or() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_or(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_or() returns expected IDs for COHO", {
  expect_snapshot_value(filter_or(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_or() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("COHO")
  ids <- filter_or(df, return_ids = TRUE)
  result <- filter_or(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_coast() -----------------------------------------------------------

test_that("filter_coast() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_coast(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_coast() returns expected IDs for COHO", {
  expect_snapshot_value(filter_coast(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_coast() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("CHINOOK")
  ids <- filter_coast(df, return_ids = TRUE)
  result <- filter_coast(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_marine() ----------------------------------------------------------

test_that("filter_marine() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_marine(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_marine() returns expected IDs for COHO", {
  expect_snapshot_value(filter_marine(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_marine() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("COHO")
  ids <- filter_marine(df, return_ids = TRUE)
  result <- filter_marine(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_commercial_wa_nt() ------------------------------------------------

test_that("filter_commercial_wa_nt() returns expected IDs for CHINOOK", {
  expect_snapshot_value(filter_commercial_wa_nt(make_fishery_df("CHINOOK"), return_ids = TRUE))
})

test_that("filter_commercial_wa_nt() returns expected IDs for COHO", {
  expect_snapshot_value(filter_commercial_wa_nt(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_commercial_wa_nt() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("CHINOOK")
  ids <- filter_commercial_wa_nt(df, return_ids = TRUE)
  result <- filter_commercial_wa_nt(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_stt() -------------------------------------------------------------

test_that("filter_stt() returns expected IDs for COHO", {
  expect_snapshot_value(filter_stt(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_stt() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("COHO")
  ids <- filter_stt(df, return_ids = TRUE)
  result <- filter_stt(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_stt_nt() ----------------------------------------------------------

test_that("filter_stt_nt() returns expected IDs for COHO", {
  expect_snapshot_value(filter_stt_nt(make_fishery_df("COHO"), return_ids = TRUE))
})

test_that("filter_stt_nt() keeps only rows with matching fishery_id", {
  df <- make_fishery_df("COHO")
  ids <- filter_stt_nt(df, return_ids = TRUE)
  result <- filter_stt_nt(df)
  expect_true(all(result$fishery_id %in% ids))
  expect_false(any(setdiff(df$fishery_id, ids) %in% result$fishery_id))
})


## filter_hatchery() --------------------------------------------------------

test_that("filter_hatchery() returns expected IDs for COHO", {
  expect_snapshot_value(filter_hatchery(make_stock_df("COHO"), return_ids = TRUE))
})

test_that("filter_hatchery() keeps only rows with matching stock_id", {
  df <- make_stock_df("COHO")
  ids <- filter_hatchery(df, return_ids = TRUE)
  result <- filter_hatchery(df)
  expect_true(all(result$stock_id %in% ids))
  expect_false(any(setdiff(df$stock_id, ids) %in% result$stock_id))
})


## filter_wild() ------------------------------------------------------------

test_that("filter_wild() returns expected IDs for COHO", {
  expect_snapshot_value(filter_wild(make_stock_df("COHO"), return_ids = TRUE))
})

test_that("filter_wild() keeps only rows with matching stock_id", {
  df <- make_stock_df("COHO")
  ids <- filter_wild(df, return_ids = TRUE)
  result <- filter_wild(df)
  expect_true(all(result$stock_id %in% ids))
  expect_false(any(setdiff(df$stock_id, ids) %in% result$stock_id))
})


## filter_mixed() -----------------------------------------------------------

test_that("filter_mixed() returns expected IDs for COHO", {
  expect_snapshot_value(filter_mixed(make_stock_df("COHO"), return_ids = TRUE))
})

test_that("filter_mixed() keeps only rows with matching stock_id", {
  df <- make_stock_df("COHO")
  ids <- filter_mixed(df, return_ids = TRUE)
  result <- filter_mixed(df)
  expect_true(all(result$stock_id %in% ids))
  expect_false(any(setdiff(df$stock_id, ids) %in% result$stock_id))
})
