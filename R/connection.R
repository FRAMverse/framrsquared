#' Connect to FRAM database
#'
#' This produces a connection object to a FRAM database, which is a necessary precursor
#' for almost all framrsquared functions. For most users, can just treat the connection
#' object as a black box that's used as an argument in other functions. See details for an explanation of the returned object.
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
#'   \item{`$fram_db_connection_id`}{connection name in .fram_connections, used for orphan cleanup via `disconnect_all_fram_connections()`.}
#'   \item{`$fram_db_type`}{"full" or "transfer", useful for validation for specific functions.}
#'   \item{`$fram_db_species`}{"COHO" or "CHINOOK"}
#'   \item{`$fram_db_species`}{filetype, typeically "mdb"}
#'   \item{`$fram_read_only`}{Optional user-specified safety measure, TRUE or FALSE. Functions that modify the fram database should error out if TRUE.}
#' }
#'
#' framrsquared creates a special environment in the global environment, `.fram_connections`. Whenever a new connection is made with `connect_fram_db`, it is added to that environment as well; whenever the connection is closed with `disconnect_fram_db()`, the connection is removed from that environment. This tracking allows `disconnect_all_fram_connections()` to clear out any orphan connections made by assigning a connection to an existing connection object.
#'
#' @export
#' @seealso [disconnect_fram_db()], [disconnect_all_fram_connections()]
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

    con_id <- as.character(as.numeric(Sys.time()) * 1000000)

    con_obj <- list(
      fram_db_connection = con, # pass connection back
      fram_db_connection_id = con_id,
      fram_db_type = fram_db_type$type,
      fram_db_species = fram_db_species,
      fram_db_medium = tools::file_ext(db_path),
      fram_read_only = read_only
    )

    .fram_connections[[con_id]] <- con_obj

    return(con_obj)

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
  validate_flag(quiet)

  db_var_name <- deparse(substitute(fram_db))
  DBI::dbDisconnect(fram_db$fram_db_connection)

  rm(list = fram_db$fram_db_connection_id, envir = .fram_connections)

  if(!quiet){
    cli::cli_alert_success(glue::glue('Successfully disconnected from FRAM database ({db_var_name})'))
  }
}

#' Clear all connections
#'
#' It is relatively easy to create an orphan connection using framrsquared by assigning a connection to `fram_db`, and then assigning another connection to `fram_db` without disconnecting the first connection using `disconnect_fram_db()`. Orphaned connections can make it frustrating to work with database files (moving, deleting, etc) without restarting rstudio or rebooting your computer. `disconnect_all_fram_connections()` disconnects any existing connections made by framrsquared in this R session.
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' fram_db = connect_fram_db("Chin2025.mdb")
#' fram_db = connect_fram_db("Chin2025.mdb")
#' disconnect_fram_db(fram_db)
#'
#' list_extant_fram_connections()
#' ## oops, still a connection left.
#'
#' disconnect_all_fram_connections()
#'
#' list_extant_fram_connections
#' }
disconnect_all_fram_connections <- function(){
  total_connections = length(.fram_connections)
  for (con_id in names(.fram_connections)) {
    con <- .fram_connections[[con_id]]
    if (DBI::dbIsValid(con$fram_db_connection)) {
      DBI::dbDisconnect(con$fram_db_connection)
    }
    rm(list = con_id, envir = .fram_connections)
  }
  cli::cli_alert_success("Disconnected all ({total_connections}) extant connections to FRAM databases")
}


#' Describe existing fram connections (including orphans)
#'
#' Provides information in the terminal, including the number of existing FRAM database connections created in this R session using framrsquared, as well as the files those connections connect to. Note that there may be multiple connections to the same file.
#'
#' @export
#'
list_extant_fram_connections = function(){
  cli::cli_alert("{length(.fram_connections)} existing connections to FRAM databases.")

  if(length(.fram_connections) > 0){
    objs <- names(.fram_connections)
    db_names <- purrr::map_chr(objs,
                               .f = \(x){DBI::dbGetInfo(.fram_connections[[x]]$fram_db_connection)$dbname}) |>
      unique()
    cli::cli_alert_warning("The following databases have extant connections to them:")
    names(db_names) = rep("*", length(db_names))
    cli::cli_bullets(db_names)
  }
}
