# documentation here
# verifies fram database and returns information about it
connect_fram_db <-
  function(db_path,
           enforce_type = c('full', 'transfer')) {

    # verify file exists
    if (!file.exists(db_path)) {
      rlang::abort('Database file doesn\'t exist. Check path.')
    }

    # more db checks
    if (tolower(strsplit(db_path, '.', fixed = T)[[1]][[2]]) != 'mdb') {
      rlang::abort('Must provide a valid .mdb access file.')
    }

    # connect to database
    con <- DBI::dbConnect(
      drv = odbc::odbc(),
      .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", db_path, ";"))

    # returns database type, checks if fram database is valid
    fram_db_type <- fram_database_type(con)

    fram_db_species <- fram_database_species(con)

    rlang::inform('Successfully connected to FRAM database')

    return(
      list(
        fram_db_connection = con, # pass connection back
        fram_db_type = fram_db_type$type,
        fram_db_species = fram_db_species
      )
    )


  }


# documentation
disconnect_fram_db <- function(fram_db){
  db_var_name <- deparse(substitute(fram_db))
  DBI::dbDisconnect(fram_db$fram_db_connection)
  rlang::inform(glue::glue('Successfully disconnected from FRAM database ({db_var_name})'))
}

