
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
