# ── Helpers -------------------------------------------------------------------

# Creates a minimal, well-formed SQLite .db file that connect_fram_db() will
# accept.  Matches the "transfer" table set expected by fram_database_type().
# Copies from the empty.mdb fixture file, as there are not easy ways to make a
# viable .mdb file from scratch. Make sure to remove the file!
make_transfer_db_file <- function(species = "CHINOOK") {

  testthat::skip_on_os(c("mac", "linux"))  # Access ODBC driver is Windows-only

  path <- tempfile(fileext = ".mdb")
  file.copy(test_path("fixtures", "empty.mdb"),
            path)
  con  <- DBI::dbConnect(
    drv = odbc::odbc(),
    .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", path, ";")
  )

  transfer_tables <- c(
    "BackwardsFRAM", "BaseID", "Cohort", "Escapement",
    "FisheryMortality", "FisheryScalers", "Mortality",
    "NonRetention", "PSCMaxER", "SizeLimits",
    "SLRatio", "StockFisheryRateScaler", "StockRecruit", "TAAETRSList"
  )
  for (tbl in transfer_tables) {
    DBI::dbExecute(con, paste0("CREATE TABLE ", tbl, " (id INTEGER)"))
  }

  # RunID needs a SpeciesName column for fram_database_species()
  DBI::dbExecute(con, "CREATE TABLE RunID (RunID INTEGER, SpeciesName TEXT)")
  DBI::dbExecute(con, paste0("INSERT INTO RunID VALUES (1, '", species, "')"))

  DBI::dbDisconnect(con)
  return(path)
}

# UNIT TESTS -------------------------------------------------------------------

## --- connect_fram_db(): input validation (no file required) ------------------

test_that("connect_fram_db() errors when the file does not exist", {
  expect_error(connect_fram_db("/no/such/file.db"), class = "framrsquared_error")
})

test_that("connect_fram_db() errors on an unsupported file extension", {
  csv_path <- tempfile(fileext = ".csv")
  writeLines("a,b", csv_path)
  withr::defer(unlink(csv_path))

  expect_error(connect_fram_db(csv_path, quiet = TRUE), class = "framrsquared_error")
})

test_that("connect_fram_db() errors when read_only is not a logical", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  expect_error(connect_fram_db(db_path, quiet = TRUE, read_only = "yes"),  class = "framrsquared_error")
  expect_error(connect_fram_db(db_path, quiet = TRUE, read_only = 1L),     class = "framrsquared_error")
})

test_that("connect_fram_db() errors when read_only has length > 1", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  expect_error(connect_fram_db(db_path, quiet = TRUE, read_only = c(TRUE, FALSE)),
               class = "framrsquared_error")
})

test_that("connect_fram_db() errors when quiet is not a logical", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  expect_error(connect_fram_db(db_path, quiet = "yes"), class = "framrsquared_error")
  expect_error(connect_fram_db(db_path, quiet = 1L),    class = "framrsquared_error")
})


## --- connect_fram_db(): successful connection --------------------------------

test_that("connect_fram_db() returns a list with the required names", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  fram_db <- connect_fram_db(db_path, quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  expect_type(fram_db, "list")
  expect_named(fram_db,
               c("fram_db_connection", "fram_db_connection_id",
                 "fram_db_type", "fram_db_species",
                 "fram_db_medium", "fram_read_only"),
               ignore.order = TRUE)
})

test_that("connect_fram_db() returns a valid DBI connection", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  fram_db <- connect_fram_db(db_path, quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  expect_true(DBI::dbIsValid(fram_db$fram_db_connection))
})

test_that("connect_fram_db() detects the correct database type", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  fram_db <- connect_fram_db(db_path, quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  expect_equal(fram_db$fram_db_type, "transfer")
})

test_that("connect_fram_db() detects the correct species", {
  chin_path <- make_transfer_db_file(species = "CHINOOK")
  coho_path <- make_transfer_db_file(species = "COHO")
  withr::defer(unlink(c(chin_path, coho_path)))

  chin_db <- connect_fram_db(chin_path, quiet = TRUE)
  coho_db <- connect_fram_db(coho_path, quiet = TRUE)
  withr::defer(disconnect_fram_db(chin_db))
  withr::defer(disconnect_fram_db(coho_db))

  expect_equal(chin_db$fram_db_species, "CHINOOK")
  expect_equal(coho_db$fram_db_species, "COHO")
})

test_that("connect_fram_db() sets fram_db_medium to the file extension", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  fram_db <- connect_fram_db(db_path, quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  expect_equal(fram_db$fram_db_medium, "mdb")
})

test_that("connect_fram_db() passes read_only through to the returned object", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  ro  <- connect_fram_db(db_path, read_only = TRUE,  quiet = TRUE)
  rw  <- connect_fram_db(db_path, read_only = FALSE, quiet = TRUE)
  withr::defer(disconnect_fram_db(ro))
  withr::defer(disconnect_fram_db(rw))

  expect_true(ro$fram_read_only)
  expect_false(rw$fram_read_only)
})

test_that("connect_fram_db() registers the connection in .fram_connections", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  fram_db <- connect_fram_db(db_path, quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  expect_true(exists(fram_db$fram_db_connection_id, envir = .fram_connections))
})


## --- disconnect_fram_db() ----------------------------------------------------

test_that("disconnect_fram_db() errors on an invalid fram_db", {
  expect_error(disconnect_fram_db(list()),         class = "framrsquared_error")
  expect_error(disconnect_fram_db("not_a_db"),     class = "framrsquared_error")
})

test_that("disconnect_fram_db() errors when quiet is not a logical", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  fram_db <- connect_fram_db(db_path, quiet = TRUE)
  withr::defer(disconnect_fram_db(fram_db))

  expect_error(disconnect_fram_db(fram_db, quiet = "yes"), class = "framrsquared_error")
})

test_that("disconnect_fram_db() invalidates the DBI connection", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  fram_db <- connect_fram_db(db_path, quiet = TRUE)
  disconnect_fram_db(fram_db)

  expect_false(DBI::dbIsValid(fram_db$fram_db_connection))
})

test_that("disconnect_fram_db() removes the connection from .fram_connections", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  fram_db <- connect_fram_db(db_path, quiet = TRUE)
  con_id  <- fram_db$fram_db_connection_id
  disconnect_fram_db(fram_db)

  expect_false(exists(con_id, envir = .fram_connections))
})



## --- list_extant_fram_connections() ------------------------------------------

test_that("list_extant_fram_connections() returns the connection count invisibly", {
  disconnect_all_fram_connections(quiet = TRUE)

  suppressMessages({
    result <- withVisible(list_extant_fram_connections())
  })
  expect_false(result$visible)
  expect_equal(result$value, 0L)
})

test_that("list_extant_fram_connections() returns the correct count", {
  disconnect_all_fram_connections(quiet = TRUE)
  db_path <- make_transfer_db_file()
  withr::defer({
    disconnect_all_fram_connections(quiet = TRUE)
    unlink(db_path)
  })

  con1 <- connect_fram_db(db_path, quiet = TRUE)
  con2 <- connect_fram_db(db_path, quiet = TRUE)
  con3 <- connect_fram_db(db_path, quiet = TRUE)
  disconnect_fram_db(con1)

  expect_equal(suppressMessages(list_extant_fram_connections()), 2L)
})

## --- disconnect_all_fram_connections() ---------------------------------------

test_that("disconnect_all_fram_connections() disconnects all active connections", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  db1 <- connect_fram_db(db_path, quiet = TRUE)
  db2 <- connect_fram_db(db_path, quiet = TRUE)

  disconnect_all_fram_connections(quiet = TRUE)

  expect_false(DBI::dbIsValid(db1$fram_db_connection))
  expect_false(DBI::dbIsValid(db2$fram_db_connection))
  expect_true(suppressMessages({list_extant_fram_connections()}) == 0)
})

test_that("disconnect_all_fram_connections() empties .fram_connections", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  connect_fram_db(db_path, quiet = TRUE)
  connect_fram_db(db_path, quiet = TRUE)

  disconnect_all_fram_connections(quiet = TRUE)

  expect_equal(length(.fram_connections), 0L)
})

test_that("disconnect_all_fram_connections() runs without error when no connections exist", {
  disconnect_all_fram_connections(quiet = TRUE)  # clear any state first
  expect_no_error(disconnect_all_fram_connections(quiet = TRUE))
})

test_that("disconnect_all_fram_connections() handles already-closed connections gracefully", {
  db_path <- make_transfer_db_file()
  withr::defer(unlink(db_path))

  fram_db <- connect_fram_db(db_path, quiet = TRUE)
  # Manually close the raw connection, leaving the entry in .fram_connections
  DBI::dbDisconnect(fram_db$fram_db_connection)

  expect_no_error(disconnect_all_fram_connections(quiet = TRUE))
  expect_equal(length(.fram_connections), 0L)
})

