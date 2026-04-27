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
