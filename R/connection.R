#' Connect to FRAM database
#'
#' This produces a connection object to a FRAM database, which is a necessary precursor
#' for almost all framrsquared functions. For most users, can just treat the connection
#' object as a black box that's used as an argument in other functions. See details for an
#' explanation of the returned object.
#'
#'
#'
#' @param db_path Path to a FRAM database.
#' @param read_only Logical, defaults to FALSE. Optional argument to flag this connection as read-only (if set to `TRUE`). If `TRUE`, framrsquared functions that modify
#' the database will abort rather than run. Use as a safety feature when working with a database that *must not* be modified.
#' @param quiet Logical, defaults to FALSE. Optional argument; when TRUE, silences success message and database summary.
#'
#' @details
#' The returned object of `connect_fram_db()` is a list of useful objects for other framrsquared functions.
#' \describe{
#'   \item{`$fram_db_connection`}{connection object, used for SQL calls.}
#'   \item{`$fram_db_type`}{"full" or "transfer", useful for validation for specific functions.}
#'   \item{`$fram_db_species`}{"COHO" or "CHINOOK"}
#'   \item{`$fram_db_species`}{filetype, typeically "mdb"}
#'   \item{`$fram_read_only`}{Optional user-specified safety measure, TRUE or FALSE. Functions that modify the fram database should error out if TRUE.}
#' }
#'
#' @export
#' @seealso [disconnect_fram_db()]
#' @examples
#' \dontrun{fram_db <- connect_fram_db('<path>')
#' fram_db |> fetch_table("Mortality")}
#'
connect_fram_db <-
  function(db_path,
           read_only = FALSE,
           quiet = FALSE) {
    # verify file exists
    if (!file.exists(db_path)) {
      cli::cli_abort('Database file doesn\'t exist. Check path.')
    }

    # more db checks
    if (!tools::file_ext(db_path) %in% c('mdb', 'db')) {
      cli::cli_abort('Must provide a valid .mdb access file or SQLite .db file')
    }

    if(!is.logical(read_only) | length(read_only) != 1){
      cli::cli_abort("`read_only` must be a logical of length 1")
    }

    if(!is.logical(quiet) | length(quiet) != 1){
      cli::cli_abort("`quiet` must be a logical of length 1")
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

    if(!quiet){cli::cli_alert_success('Successfully connected to FRAM database')}

    if(!quiet && !tools::file_ext(db_path) == 'db'){welcome(con)}

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
#' @param quiet Logical. Optional; when true, silences success message.
#' @export
#' @seealso [connect_fram_db()]
#' @examples
#' \dontrun{disconnect_fram_db(fram_db)}
#'
disconnect_fram_db <- function(fram_db,
                               quiet = TRUE){
  validate_fram_db(fram_db)

  if(!is.logical(quiet) | length(quiet) != 1){
    cli::cli_abort("`quiet` must be a logical of length 1")
  }

  db_var_name <- deparse(substitute(fram_db))
  DBI::dbDisconnect(fram_db$fram_db_connection)
  if(!quiet){
    cli::cli_alert_success(glue::glue('Successfully disconnected from FRAM database ({db_var_name})'))
  }
}

