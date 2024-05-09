#' This is a connection object to a FRAM database. Returns a list
#' used throughout the rest of this package which carries meta data.
#' @param db_path Path to a FRAM database.
#' @param enforce_type Not used
#' @param read_only Optional argument to flag this connection as read-only (if set to `TRUE`). If `TRUE`, framrsquared functions that modify
#' the database will abort rather than run. Use as a safety feature when working with a database that *must not* be modified.
#' @details
#' Additional details...
#'
#' @export
#' @examples
#' \dontrun{fram_db <- connect_fram_db('<path>')}
#'
connect_fram_db <-
  function(db_path,
           enforce_type = c('full', 'transfer'),
           read_only = FALSE) {
    lifecycle::deprecate_warn("1.0.0", "expand_()", "expand()")
    # verify file exists
    if (!file.exists(db_path)) {
      cli::cli_abort('Database file doesn\'t exist. Check path.')
    }

    # more db checks
    if (!tools::file_ext(db_path) %in% c('mdb', 'db')) {
      cli::cli_abort('Must provide a valid .mdb access file or SQLite .db file')
    }

    # connect to database
    if(tools::file_ext(db_path) == 'mdb'){
      con <- DBI::dbConnect(
        drv = odbc::odbc(),
        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", db_path, ";"))

    } else if (tools::file_ext(db_path) == 'db') {
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        db_path
      )

    } else {
      cli::cli_abort('Something went wrong connecting to a database')
    }

    # returns database type, checks if fram database is valid
    fram_db_type <- fram_database_type(con)

    fram_db_species <- fram_database_species(con)

    cli::cli_alert_success('Successfully connected to FRAM database')

    welcome(con)

    return(
      list(
        fram_db_connection = con, # pass connection back
        fram_db_type = fram_db_type$type,
        fram_db_species = fram_db_species,
        fram_db_medium = tools::file_ext(db_path),
        fram_read_only = read_only
      )
    )


  }


#' Safely disconnect from FRAM database
#' @param fram_db FRAM database R object
#' @export
#' @examples
#' \dontrun{disconnect_fram_db(fram_db)}
#'
disconnect_fram_db <- function(fram_db){
  db_var_name <- deparse(substitute(fram_db))
  DBI::dbDisconnect(fram_db$fram_db_connection)
  cli::cli_alert_success(glue::glue('Successfully disconnected from FRAM database ({db_var_name})'))
}

