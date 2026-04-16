db_test_path <- function(...) {
  path <- Sys.getenv("FRAMRSQURED_TEST_DIR", unset = NA)
  if (is.na(path)) return(NA_character_)
  file.path(path, ...)
}

skip_if_no_test_db <- function() {
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    testthat::skip("Test database not available")
  }
}

## helper functions to make it easy to do ad-hoc tests

connection_coho_pre <- function(){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    cli::cli_abort("Test database not available")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/coho_pre.mdb"), read_only = TRUE))
  }
}

connection_coho_post <- function(){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    cli::cli_abort("Test database not available")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/coho_post.mdb"), read_only = TRUE))
  }
}

connection_coho_transfer <- function(){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    cli::cli_abort("Test database not available")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/coho_transfer.mdb"), read_only = TRUE))
  }
}


connection_chin_pre <- function(){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    cli::cli_abort("Test database not available")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/chin_pre.mdb"), read_only = TRUE))
  }
}

connection_chin_post <- function(){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    cli::cli_abort("Test database not available")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/chin_post.mdb"), read_only = TRUE))
  }
}

connection_chin_transfer <- function(){
  path <- db_test_path()
  if (is.na(path) || !file.exists(path)) {
    cli::cli_abort("Test database not available")
  } else {
    return(connect_fram_db(paste0(path, "/original_databases/chin_transfer.mdb"), read_only = TRUE))
  }
}
