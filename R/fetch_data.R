#' Fetch a complete table from a FRAM database.
#'
#' Returns a cleaned tibble, with column labels that were camel case (e.g., TimeStep) converted to snake case (e.g., time_step).
#' **WARNING**: the Chinook "BackwardsFRAM" table uses a *different* stock_id numbering system from every other table. To avoid errors when joining that with other tables, instead fetch with [fetch_table_bkchin()]
#'
#' @param fram_db FRAM database object
#' @param table_name Atomic character of name of table to be fetched. If not given, a list of options will be printed.
#' @param label Logical, defaults to TRUE. Add human-readable columns for flags, fisheries, stocks?
#' @param warn Print a warning when fetching BackwardsFRAM table from a Chinook database? Logical, defaults to `TRUE`.
#' @export
#' @examples
#' \dontrun{
#' fram_db <- connect_fram_db("validat2024.mdb")
#' fram_db |> fetch_table('Mortality')}
#'

fetch_table <- function(fram_db, table_name = NULL, label = TRUE, warn = TRUE){
  ## adding input checking
  validate_fram_db(fram_db)
  validate_flag(warn)
  if(!is.null(table_name)){
    if(!is.character(table_name) | length(table_name)!= 1){
      cli::cli_abort("`table` must be a character atomic.")
    }
  }
  all_tables = provide_table_names(is_full = TRUE)
  limited_tables = provide_table_names(is_full = FALSE)



  if (is.null(table_name)) {
    if (fram_db$fram_db_type == 'full') {
      cli::cli_alert_info('A table name must be provided, see available options:')
      fmt <- cli::ansi_columns(
        all_tables,
        fill = "rows",
        max_cols = 4,
        align = "center",
        sep = ""
      )
      cli::boxx(
        fmt,
        padding = c(0, 1, 0, 1),
        header = cli::col_cyan("FRAM tables (full database)")
      )
    } else{

      fmt <- cli::ansi_columns(
        limited_tables,
        fill = "rows",
        max_cols = 4,
        align = "center",
        sep = "   "
      )
      cli::boxx(
        fmt,
        padding = c(0, 1, 0, 1),
        header = cli::col_cyan("FRAM tables (transfer database)")
      )
    }
  } else{
    validate_table(fram_db, table_name)


    if(fram_db$fram_db_species == "CHINOOK" & table_name == "BackwardsFRAM"){
      if(warn){
        cli::cli_alert_danger("Chinook BackwardsFRAM tables use different numbering for stock_id!\n This can cause problems when merging with other tables!\n Recommend fetch_table_bkchin() instead.")
      }
      label = FALSE
    }

    output_table <- DBI::dbGetQuery(fram_db$fram_db_connection,
                                    glue::glue('SELECT * FROM {table_name};')) |>
      fram_clean_tables()

    attr(output_table, 'species') <- fram_db$fram_db_species

    if(label){
      output_table <- output_table |>
        label_flags(warn = FALSE)
      if("fishery_id" %in% names(output_table) & fram_db$fram_db_type == "full"){
        output_table <- output_table |>
          label_fisheries_db(fram_db)
      }
      if("stock_id" %in% names(output_table) & fram_db$fram_db_type == "full"){
          output_table <- output_table |>
            label_stocks_db(fram_db)
      }
    }

    if(table_name == "Mortality" & warn == TRUE){
      neg_mort_runs <- output_table |>
        dplyr::filter(dplyr::if_any(.data$landed_catch:.data$msf_encounter, ~ . < 0)) |>
        dplyr::pull(.data$run_id) |>
        unique()
      if(length(neg_mort_runs>0)){
        cli::cli_alert_danger("DANGER!! Run ID(s) {neg_mort_runs} have one or more negative mortality or encounter values in the 'Mortality' table!")
      }
    }

    return(output_table)
  }
}

## alias for fetch_table with label = FALSE
fetch_table_ <- function(fram_db, table_name = NULL, warn = TRUE){
  fetch_table(fram_db = fram_db,
              table_name = table_name,
              warn = warn,
              label = FALSE)
}

#' Safely fetch Chinook BackwardsFRAM table
#'
#' The BackwardsFRAM table uses a stock_id different numbering system from all other tables, and includes
#' sums of joint stocks (e.g. for a marked and unmarked pair of stocks, BackwardsFRAM will typically have an additional
#' stock which represents the sum of those two). Because the numbering is different but the column name is the same,
#' joining the Chinook BackwardsFRAM table with other tables can cause problems.
#'
#' This function augments fetch_table by renaming the `stock_id` column to `bk_stock_id`, and
#' then adding on the associated stock_id (with NAs when the bkfram stock is one of these new "sum" stocks
#' and the associated bkfram stock names). This function only works for Chinook databases.
#'
#' **The resulting dataframe will necessarily NOT be an exact match to the BackwardsFRAM table in the FRAM database. The stock_id column will differ (containing normal stock ID values instead of bk stock ID values), and there will be two additional columns.**
#'
#' @param fram_db FRAM database object
#'
#' @export
#'
#' @examples
#' #' @examples
#' \dontrun{
#' ##Potentially problematic stock_id won't align with other tables
#' fram_db |> fetch_table('BackwardsFRAM')
#' ## "safe" version of the table; stock_id WILL align with other tables
#' fram_db |> fetch_table_bkchin()
#' }
fetch_table_bkchin <- function(fram_db){
  validate_fram_db(fram_db)

  if(fram_db$fram_db_species != "CHINOOK"){
    cli::cli_abort("`fetch_table_bkchin()` only appropriate for CHINOOK databases, not {fram_db$fram_db_species} database.")
  }

  output_table <- fetch_table_(fram_db, table_name = "BackwardsFRAM",
                              warn = FALSE) |>
    dplyr::rename(bk_stock_id = .data$stock_id) |>
    dplyr::left_join(framrosetta::bk_lookup_chin, by = "bk_stock_id")

  return(output_table)
}

fetch_table_colnames <- function(fram_db, table_name){
  DBI::dbGetQuery(
    fram_db$fram_db_connection,
    glue::glue("SELECT * FROM {table_name} where false;")
  ) |>
    colnames()
}


