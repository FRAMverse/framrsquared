#' Modify FRAM database based on match/replace dataframe
#'
#' Uses a special match/replace dataframe to modify values in a FRAM table.
#'
#' At a high level, modifying a FRAM table requires identify which rows to change, and then replacing the values of one or more of the columns of those row with new values. We often want to make multiple changes at once, and `modify_table` is written around using a dataframe to define the matching and replacing, so that it is relatively easy to check all of the changes being made. This dataframe (hereafter the "match/replace dataframe") should have column names starting with "match_" and "replace_", and ending with the exact match of column names in the FRAM table identified with argument `table_name`. For each row of argument `df`, `modify_table()` will use columns starting with "match_" as conditions to identify rows in the FRAM database to modify, and then for those rows will replace the values of columns identified with "replace_" with the corresponding values in the `df` columns.
#'
#' As a simple example, imagine we want to see how modifying the size limits for Area 7 Sport (chinook fishery id 36) affect our ERs. We would probably start by using copy_run to create multiple duplicate runs, and then we can use `modify_table` to change just the `MinimumSize` values of the "SizeLimits" table for just those rows for which fishery id was 36. If our run ids were 100, 101, and 102, and we wanted to look at minimum sizes of 450, 550, and 650, our `df` argument might look like `data.frame(match_RunID = c(100, 101, 102), match_FisheryID = c(36, 36, 36), replace_MinimumSize = c(450, 550, 650))`. Notably, we might create `df` programmatically to combine different run ids with multiple changes at once or to apply some kind of randomized parameter sampling scheme. Or we could even use an excel sheet to write out the experiment in a `df` format and then read in the sheet and feed it into `modify_table`.
#'
#' @param fram_db FRAM database
#' @param table_name Name of FRAM table
#' @param df The match/replace dataframe or tibble with specially named columns. Columns must start with either "match_" or "replace_", and should otherwise match the names of columns in `table`. For example, modifications to the Cohort table might be achieved with columns "match_RunID", "match_StockID", "match_age", "match_TimeStep", "replace_StartCohort". See Details.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df_total = tibble(match_Age = c(3,4,5))
#' df_total$match_StockID = 100
#' df_total$match_RunID = 396
#' df_total$replace_RecruitScaleFactor = 1:3
#' df_total$replace_RecruitCohortSize = 100:102
#' fram_db |> modify_db(table_name = "StockRecruit", df = df_total)
#' }
modify_table <- function(fram_db, table_name, df){

  ## check format of names:
  if(length(grep("^match_.*|^replace_.*", names(df), invert = TRUE)) > 0){
    cli::cli_abort("`df` must have named columns starting with 'match_' or 'replace_'")
  }

  ## get column names
  table_columns =  DBI::dbGetQuery(fram_db$fram_db_connection,
                                   glue::glue('SELECT * FROM {table_name} where false;')) |>
    colnames()

  match_names = grep("^match_", names(df), value = TRUE)
  match_names = gsub("^match_", "", match_names)

  replace_names = grep("^replace_", names(df), value = TRUE)
  replace_names = gsub("^replace_", "", replace_names)

  ## error checking:
  ##  match_names and replace_names should have no overlap
  if(length(intersect(match_names, replace_names)) > 1){
    cli::cli_abort("'replace_' and 'match_' column types in `df` must be unique. One or more variable is assigned to both matching and replacing!")
  }

  if(length(match_names)==0 | length(replace_names) ==0){
    cli::cli_abort("`df` must have both 'match_' and 'replace_' columns!")
  }

  if(any(! c(match_names, replace_names) %in% table_columns)){
    cli::cli_abort("`df` points to columns that are not in in `table_name`!")
  }

  glue_conditions = glue::glue("{match_names} = {{match_{match_names}}}")
  glue_conditions = paste0(glue_conditions, collapse = " AND ")

  glue_changes = glue::glue("{replace_names} = {{replace_{replace_names}}}")
  glue_changes = paste0(glue_changes, collapse = ", ")

  glue_statement = glue::glue("UPDATE {{table_name}}
                            Set {glue_changes}
                            Where {glue_conditions};
                            ")
  results = df |>
    dplyr::rowwise() |>
    dplyr::mutate(db_call = glue::glue(glue_statement))

  for (i in 1:nrow(results)){
    DBI::dbExecute(fram_db$fram_db_connection,
                   statement = results$db_call[i])
  }
}
