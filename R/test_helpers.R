## making and working with mock database ---------------------------

# Minimal mock fram_db list (valid structure, invalid connection)
make_mock_fram_db <- function(type = "full", species = "CHINOOK", read_only = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  list(
    fram_db_connection = con,
    fram_db_connection_id = '10',
    fram_db_type       = type,
    fram_db_species    = species,
    fram_db_medium = "mdb",
    fram_read_only     = read_only
  )
}

## for multiple tables. NOTE: `table_list` MUST be a named list of dataframes
make_queryable_mock_db_list <- function(table_list,
                                        species = "CHINOOK",
                                        type    = "full") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  for(i in 1:length(table_list)){
    DBI::dbWriteTable(con, names(table_list)[i], table_list[[i]])
  }
  list(
    fram_db_connection    = con,
    fram_db_connection_id = "test_queryable",
    fram_db_type          = type,
    fram_db_species       = species,
    fram_db_medium        = "db",
    fram_read_only        = FALSE
  )
}

## disconnect mocked db
disconnect_mock_fram_db <- function(db){
  DBI::dbDisconnect(db$fram_db_connection)
}

## turn mocked db into list for review
listify_tables <- function(fram_db){

  all_tables <- DBI::dbListTables(fram_db$fram_db_connection)

  res <- list()
  for(cur_table in all_tables){
    res[[cur_table]] = fetch_table_(fram_db, cur_table)
  }
  return(res)
}

## connecting to local test files files ----------------------------------------------

db_test_path <- function(...) {
  path <- Sys.getenv("FRAMRSQUARED_TEST_DIR", unset = NA)
  if (is.na(path)) return(NA_character_)
  file.path(path, ...)
}

connection_test_db <- function(relative_to_original_databases = "",
                               read_only = TRUE,
                               quiet = TRUE
){
  connect_fram_db(paste0(db_test_path(), "/original_databases/", relative_to_original_databases),
                  read_only = read_only,
                  quiet = quiet)
}

skip_if_no_test_db <- function() {
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    testthat::skip("Test database not available, and/or FRAMRSQUARED_TEST_DIR environmental variable not defined.")
  }
}

## helper functions to make it easy to do ad-hoc tests

connection_coho_pre <- function(quiet = FALSE){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    fram_abort("Test database not available, and/or FRAMRSQUARED_TEST_DIR environmental variable not defined.")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/coho_pre.mdb"), quiet = quiet, read_only = TRUE))
  }
}

connection_coho_post <- function(quiet = FALSE){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    fram_abort("Test database not available, and/or FRAMRSQUARED_TEST_DIR environmental variable not defined.")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/coho_post.mdb"), quiet = quiet, read_only = TRUE))
  }
}

connection_coho_transfer <- function(quiet = FALSE){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    fram_abort("Test database not available, and/or FRAMRSQUARED_TEST_DIR environmental variable not defined.")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/coho_transfer.mdb"), quiet = quiet, read_only = TRUE))
  }
}


connection_chin_pre <- function(quiet = FALSE){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    fram_abort("Test database not available, and/or FRAMRSQUARED_TEST_DIR environmental variable not defined.")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/chin_pre.mdb"), quiet = quiet, read_only = TRUE))
  }
}

connection_chin_post <- function(quiet = FALSE){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    fram_abort("Test database not available, and/or FRAMRSQUARED_TEST_DIR environmental variable not defined.")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/chin_post.mdb"), quiet = quiet, read_only = TRUE))
  }
}

connection_chin_transfer <- function(quiet = FALSE){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    fram_abort("Test database not available, and/or FRAMRSQUARED_TEST_DIR environmental variable not defined.")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/chin_transfer.mdb"), quiet = quiet, read_only = TRUE))
  }
}
