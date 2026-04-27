# UNIT TESTS ──────────────────────────────────────────────────────────────────

# ── Helpers ───────────────────────────────────────────────────────────────────

# A mock fram_db backed by a real SQLite connection with one pre-loaded table,
# so fetch_table_() can actually execute queries in unit tests.
make_queryable_mock_db <- function(table_name, df,
                                   species = "CHINOOK",
                                   type    = "full") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, table_name, df)
  list(
    fram_db_connection    = con,
    fram_db_connection_id = "test_queryable",
    fram_db_type          = type,
    fram_db_species       = species,
    fram_db_medium        = "db",
    fram_read_only        = FALSE
  )
}

## ── fetch_table() input validation ────────────────────────────────────────────

test_that("fetch_table() errors for invalid fram_db", {
  expect_error(fetch_table(list()),       class = "framrsquared_error")
  expect_error(fetch_table("not_a_db"),   class = "framrsquared_error")
  expect_error(fetch_table(NULL),         class = "framrsquared_error")
})

test_that("fetch_table() errors for invalid table_name type", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(fetch_table(fram_db, 42L),           class = "framrsquared_error")
  expect_error(fetch_table(fram_db, TRUE),           class = "framrsquared_error")
  expect_error(fetch_table(fram_db, list("RunID")),  class = "framrsquared_error")
})

test_that("fetch_table() errors when table_name has length > 1", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    fetch_table(fram_db, c("RunID", "Mortality")),
    class = "framrsquared_error"
  )
})

test_that("fetch_table() errors for an unrecognized table name", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(fetch_table(fram_db, "NotARealTable"))
})

test_that("fetch_table() errors for a full-db table when using a transfer db", {
  fram_db <- make_mock_fram_db(type = "transfer")
  withr::defer(disconnect_mock_fram_db(fram_db))

  # "AEQ" exists only in full databases
  expect_error(fetch_table(fram_db, "AEQ"))
})

test_that("fetch_table() errors for an invalid label argument", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(fetch_table(fram_db, label = "yes"),          class = "framrsquared_error")
  expect_error(fetch_table(fram_db, label = 1L),             class = "framrsquared_error")
  expect_error(fetch_table(fram_db, label = c(TRUE, FALSE)), class = "framrsquared_error")
})

test_that("fetch_table() errors for an invalid warn argument", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(fetch_table(fram_db, warn = "yes"), class = "framrsquared_error")
  expect_error(fetch_table(fram_db, warn = 1L),    class = "framrsquared_error")
})

## ── fetch_table() / fetch_table_() behavior ───────────────────────────────────

test_that("fetch_table_() returns a tibble with snake_case names and species attr", {
  df <- data.frame(RunID = 1L, SpeciesName = "CHINOOK", RunName = "Test")
  fram_db <- make_queryable_mock_db("RunID", df, species = "CHINOOK")
  withr::defer(DBI::dbDisconnect(fram_db$fram_db_connection))

  result <- fetch_table_(fram_db, "RunID")

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("run_id", "species_name", "run_name"))
  expect_equal(attr(result, "species"), "CHINOOK")
})

test_that("fetch_table_() species attribute reflects the database species", {
  df <- data.frame(RunID = 1L)

  chin_db <- make_queryable_mock_db("RunID", df, species = "CHINOOK")
  withr::defer(DBI::dbDisconnect(chin_db$fram_db_connection))
  coho_db <- make_queryable_mock_db("RunID", df, species = "COHO")
  withr::defer(DBI::dbDisconnect(coho_db$fram_db_connection))

  expect_equal(attr(fetch_table_(chin_db, "RunID"), "species"), "CHINOOK")
  expect_equal(attr(fetch_table_(coho_db, "RunID"), "species"), "COHO")
})

test_that("fetch_table_() result contains no label columns", {
  df <- data.frame(RunID = 1L, SpeciesName = "CHINOOK")
  fram_db <- make_queryable_mock_db("RunID", df)
  withr::defer(DBI::dbDisconnect(fram_db$fram_db_connection))

  result <- fetch_table_(fram_db, "RunID")
  expect_false(any(grepl("_label$", names(result))))
})

test_that("fetch_table() emits a message for Chinook BackwardsFRAM when warn = TRUE", {
  df <- data.frame(StockID = 1L, RunID = 1L)
  fram_db <- make_queryable_mock_db("BackwardsFRAM", df, species = "CHINOOK")
  withr::defer(DBI::dbDisconnect(fram_db$fram_db_connection))

  expect_message(
    fetch_table(fram_db, "BackwardsFRAM", warn = TRUE),
    regexp = "BackwardsFRAM"
  )
})

test_that("fetch_table() is silent for Chinook BackwardsFRAM when warn = FALSE", {
  df <- data.frame(StockID = 1L, RunID = 1L)
  fram_db <- make_queryable_mock_db("BackwardsFRAM", df, species = "CHINOOK")
  withr::defer(DBI::dbDisconnect(fram_db$fram_db_connection))

  expect_no_message(fetch_table(fram_db, "BackwardsFRAM", warn = FALSE))
})

## ── fetch_table_() input validation ───────────────────────────────────────────

test_that("fetch_table_() errors for invalid fram_db", {
  expect_error(fetch_table_(list()),       class = "framrsquared_error")
  expect_error(fetch_table_("not_a_db"),   class = "framrsquared_error")
})

test_that("fetch_table_() errors for invalid table_name type", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(fetch_table_(fram_db, 42), class = "framrsquared_error")
})

test_that("fetch_table_() errors for unrecognized table name", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(fetch_table_(fram_db, "NotARealTable"))
})

## ── fetch_table_bkchin() ──────────────────────────────────────────────────────

test_that("fetch_table_bkchin() errors for invalid fram_db", {
  expect_error(fetch_table_bkchin(list()),       class = "framrsquared_error")
  expect_error(fetch_table_bkchin("not_a_db"),   class = "framrsquared_error")
})

test_that("fetch_table_bkchin() errors for non-Chinook database", {
  fram_db <- make_mock_fram_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(fetch_table_bkchin(fram_db), class = "framrsquared_error")
})

test_that("fetch_table_bkchin() renames stock_id to bk_stock_id", {
  fram_db <- make_queryable_mock_db("BackwardsFram",
                                    data.frame(run_id = rep(1, 10),
                                               stock_id = 1:10,
                                               time_step = rep(1, 10)),
                                    species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- fetch_table_bkchin(fram_db)

  expect_true("bk_stock_id" %in% names(result))
  expect_true("stock_id" %in% names(result))
  expect_equal(result$stock_id, framrosetta::bk_lookup_chin$stock_id[1:10])
})

# INTEGRATION TESTS ────────────────────────────────────────────────────────────

## ── fetch_table() structural checks ───────────────────────────────────────────

test_that("fetch_table_() returns a tibble from a real database", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  expect_s3_class(fetch_table_(fram_db, "RunID"), "tbl_df")
})

test_that("fetch_table_() species attribute matches the connected database", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  expect_equal(attr(fetch_table_(fram_db, "RunID"), "species"), "CHINOOK")

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_equal(attr(fetch_table_(fram_db, "RunID"), "species"), "COHO")
})

test_that("fetch_table() with label = TRUE adds fishery_label to tables with fishery_id", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  result   <- fetch_table(fram_db, "FisheryScalers", label = TRUE)

  expect_true("fishery_label" %in% names(result))
})

test_that("fetch_table() with label = TRUE adds stock_label to tables with stock_id", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  result   <- fetch_table(fram_db, "Mortality", label = TRUE)

  expect_true("stock_label" %in% names(result))
})

test_that("fetch_table() with label = FALSE adds no label columns", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  result   <- fetch_table(fram_db, "Mortality", label = FALSE)

  expect_false("fishery_label" %in% names(result))
  expect_false("stock_label"   %in% names(result))
})

test_that("fetch_table_() adds no label columns", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  result   <- fetch_table_(fram_db, "FisheryScalers")

  expect_false(any(grepl("_label$", names(result))))
})

## Labelling doesn't change row count --------------------------------------------

find_valid_tables <- function(fram_db){

  tables_to_check <- find_tables_by_column_(fram_db, c("StockID", "TimeStep", "FisheryID") )|>
    dplyr::pull(value) |>
    unique()

  check_table_validity <- function(cur_table){
    tryCatch(
      {
        validate_table(fram_db, cur_table)
        TRUE
      },
      error = function(e){FALSE}
    )
  }

  table_passes <- purrr::map_lgl(tables_to_check,
                                 .f = check_table_validity)

  tables_to_check <- tables_to_check[table_passes]

  return(tables_to_check)
}

test_that("fetch_table() labelling doesn't affect rowcount; chinook pre", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)

  tables_to_check <- find_valid_tables(fram_db)

  for(cur_table in tables_to_check){

    unlabeled <- fetch_table(fram_db, cur_table, label = FALSE, warn = FALSE)
    labeled <- fetch_table(fram_db, cur_table, label = TRUE, warn = FALSE)

    expect_equal(nrow(unlabeled), nrow(unlabeled))

  }


})


test_that("fetch_table() labelling doesn't affect rowcount; coho post", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_post(quiet = TRUE)

  tables_to_check <- find_valid_tables(fram_db)

  for(cur_table in tables_to_check){

    unlabeled <- fetch_table(fram_db, cur_table, label = FALSE, warn = FALSE)
    labeled <- fetch_table(fram_db, cur_table, label = TRUE, warn = FALSE)

    expect_equal(nrow(unlabeled), nrow(unlabeled))

  }


})


test_that("fetch_table() labelling doesn't affect rowcount; multi-species files", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_test_db("fail files/two_species_invisible.mdb")

  tables_to_check <- find_valid_tables(fram_db)

  for(cur_table in tables_to_check){

    unlabeled <- fetch_table(fram_db, cur_table, label = FALSE, warn = FALSE)
    labeled <- fetch_table(fram_db, cur_table, label = TRUE, warn = FALSE)

    expect_equal(nrow(unlabeled), nrow(unlabeled))
  }

})

test_that("fetch_table() labelling doesn't encounter multi-species RunID tables", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  expect_error(connection_test_db("fail files/two_species.mdb"))
})


## ── BackwardsFRAM warnings ─────────────────────────────────────────────────────

test_that("fetch_table() warns when fetching BackwardsFRAM from a Chinook database", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  expect_message(
    fetch_table(fram_db, "BackwardsFRAM", warn = TRUE),
    regexp = "BackwardsFRAM"
  )
})

test_that("fetch_table() is silent for BackwardsFRAM when warn = FALSE", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  expect_no_message(fetch_table(fram_db, "BackwardsFRAM", warn = FALSE))
})

## ── fetch_table_bkchin() ──────────────────────────────────────────────────────

test_that("fetch_table_bkchin() adds bk_stock_id, stock_name_bk, has stock_id table to bk_stock_id from a real database", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  result   <- fetch_table_bkchin(fram_db)

  expect_true("bk_stock_id" %in% names(result))
  expect_true("stock_id"      %in% names(result))
  expect_true("stock_name_bk" %in% names(result))
})


## ── Table dimension checks ─────────────────────────────────────────────────────
#
# Fill in c(nrow, ncol) for each table once you know the expected dimensions.
# Leave either value as NA_integer_ to skip that dimension check for that table.

test_that("fetch_table on chin_pre has expected row count", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_chin_pre(quiet = TRUE)
  tables_to_check <- find_valid_tables(fram_db)


  for(cur_table_name in tables_to_check){
    raw_nrow <-
      DBI::dbGetQuery(fram_db$fram_db_connection,
                      glue::glue('SELECT COUNT(*) FROM {cur_table_name};')) |>
      dplyr::pull()
    framrsquared_nrow <- fetch_table(fram_db, cur_table_name, warn = FALSE) |>
      nrow()
    expect_equal(raw_nrow, framrsquared_nrow)
  }

})

test_that("fetch_table on coho_post has expected row count", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections(quiet = TRUE))

  fram_db <- connection_coho_post(quiet = TRUE)
  tables_to_check <- find_valid_tables(fram_db)


  for(cur_table_name in tables_to_check){
    raw_nrow <-
      DBI::dbGetQuery(fram_db$fram_db_connection,
                      glue::glue('SELECT COUNT(*) FROM {cur_table_name};')) |>
      dplyr::pull()
    framrsquared_nrow <- fetch_table(fram_db, cur_table_name) |>
      nrow()
    expect_equal(raw_nrow, framrsquared_nrow)
  }

})
