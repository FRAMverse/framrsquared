#'  `r lifecycle::badge("experimental")`
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
#' df_total <- tibble(match_Age = c(3, 4, 5))
#' df_total$match_StockID <- 100
#' df_total$match_RunID <- 396
#' df_total$replace_RecruitScaleFactor <- 1:3
#' df_total$replace_RecruitCohortSize <- 100:102
#' fram_db |> modify_db(table_name = "StockRecruit", df = df_total)
#' }
modify_table <- function(fram_db, table_name, df) {
  ## check format of names:
  if (length(grep("^match_.*|^replace_.*", names(df), invert = TRUE)) > 0) {
    cli::cli_abort("`df` must have named columns starting with 'match_' or 'replace_'")
  }

  ## get column names
  table_columns = fetch_table_colnames(fram_db, table_name)

  match_names <- grep("^match_", names(df), value = TRUE)
  match_names <- gsub("^match_", "", match_names)

  replace_names <- grep("^replace_", names(df), value = TRUE)
  replace_names <- gsub("^replace_", "", replace_names)

  ## error checking:
  ##  match_names and replace_names should have no overlap
  if (length(intersect(match_names, replace_names)) > 1) {
    cli::cli_abort("'replace_' and 'match_' column types in `df` must be unique. One or more variable is assigned to both matching and replacing!")
  }

  if (length(match_names) == 0 | length(replace_names) == 0) {
    cli::cli_abort("`df` must have both 'match_' and 'replace_' columns!")
  }

  if (any(!c(match_names, replace_names) %in% table_columns)) {
    cli::cli_abort("`df` points to columns that are not in in `table_name`!")
  }

  glue_conditions <- glue::glue("{match_names} = {{match_{match_names}}}")
  glue_conditions <- paste0(glue_conditions, collapse = " AND ")

  glue_changes <- glue::glue("{replace_names} = {{replace_{replace_names}}}")
  glue_changes <- paste0(glue_changes, collapse = ", ")

  glue_statement <- glue::glue("UPDATE {{table_name}}
                            Set {glue_changes}
                            Where {glue_conditions};
                            ")
  results <- df |>
    dplyr::rowwise() |>
    dplyr::mutate(db_call = glue::glue(glue_statement)) |>
    dplyr::mutate(rows_affected = DBI::dbExecute(fram_db$fram_db_connection,
                                                 statement = .data$db_call
    )) |>
    dplyr::mutate(db_call = as.list(.data$db_call))
  return(results)
}


#'  `r lifecycle::badge("experimental")`
#' Calculate match/replace df based on scaling
#'
#' Uses a match/replace-style table like in `modify_table()`, but allows user to specify
#' scaling factors for individual columns rather than absolute values, and returns
#' the corresponding match/replace df to be used in `modify_table()`. This is intended to support
#' sensitivity analyses structured as "carry out 100 runs, with stock recruit scalers for stock X
#' running from 5% to 500% of the current value" (`calc_fram_scaling()` is only one part
#' of the pipeline for this). See `modify_table()` for details of setting up a match/replace dataframe;
#' the only difference here is that the columns to be scaled should start with "scale_" instead
#' of "replace_", and should contain the scalers.
#'
#' Note: In the StockRecruit table, RecruitScaleFactor and RecruitCohortSize should have a fixed relationship (scale factor = cohort size / base period size). For this reason, if applying scaling to only one of those columns, `calc_fram_scaling` will automatically apply the same scaling to the other. If the scaling for both is provided and they do not match, `calc_fram_scaling` will error out.
#'
#' @param fram_db FRAM database
#' @param table_name name of FRAM table
#' @param df As the match/replace dataframe of `modify_table`, but with "scale_" columns instead of "replace_" columns. Columns must start with either "match_" or "scale_", and should otherwise match the names of columns in `table`. Columns starting with "scale_" define the scaling factor to be applied to values in that column (for rows matched with the "match_" columns). For example, scaling the StartCohort values to 50% in the Cohort table might be achieved with columns "match_RunID", "match_StockID", "match_age", "match_TimeStep", "scale_StartCohort", with values of 0.5 in scale_Startcohort.
#'
#' @return A match/replace df for use in `modify_table()`, with "replace_" values
#' generated by scaling the corresponding values in the FRAM database. Includes additional "match_"
#' columns for all columns except "PrimaryKey"
#' @export
#'
#' @examples
#' \dontrun{
#' ## in run 31, decrease stock 1's recruit numbers by 50% and double 2's recruit numbers
#' library(here)
#' fram_db <- connect_fram_db(here("example_fram_db.mdb"))
#'
#' df <- data.frame(match_RunID = c(31, 31), match_StockID = 1:2, scale_RecruitScaleFactor = c(.5, 2))
#'
#' df_scaled <- calc_fram_scaling(fram_db, "StockRecruit", df)
#' ## here's what the values become:
#' df_scaled
#'
#' ## we can then modify the database
#' modified <- modify_table(fram_db, "StockRecruit", df_scaled)
#'
#' disconnect_fram_db(fram_db)
#' }
calc_fram_scaling <- function(fram_db, table_name, df) {
  ## Complication: input and output

  tab <- fram_db |>
    fetch_table(table_name)
  db_names <- DBI::dbGetQuery(
    fram_db$fram_db_connection,
    glue::glue("SELECT * FROM {table_name} where false;")
  ) |>
    colnames()
  names(tab) <- db_names

  ## finding scale and match column names
  scale_names <- names(df) |>
    stringr::str_subset("^scale_") |>
    stringr::str_remove("^scale_")

  match_names <- names(df) |>
    stringr::str_subset("^match_") |>
    stringr::str_remove("^match_")

  ## safety check specifically for StockRecruit
  terms_included <- intersect(
    c("RecruitScaleFactor", "RecruitCohortSize"),
    scale_names
  )
  terms_excluded <- setdiff(c("RecruitScaleFactor", "RecruitCohortSize"), scale_names)

  if (length(terms_included) == 1) {
    cli::cli_alert_warning("Adding scaling for {terms_excluded} equal to {terms_included}.")
    df <- df |>
      dplyr::mutate("scale_{terms_excluded}" := .data[[glue::glue("scale_{terms_included}")]])
    scale_names <- c(scale_names, terms_excluded)
  }

  if (length(terms_included == 1)) {
    if (!all(df$scale_RecruitCohortSize ==
             df$scale_RecruitScaleFactor)) {
      cli::cli_abort("scale_RecruitCohortSize and scale_RecruitScaleFactor must match!")
    }
  }

  ## creating mergeable version of df
  df_merge <- df
  ind.match <- grep("^match_", names(df))
  names(df_merge)[ind.match] <- match_names
  ind.scale <- grep("^scale_", names(df))
  names(df_merge)[ind.scale] <- glue::glue(".{scale_names}")

  ## creating modified version of df; this will be our final result
  df_mod <- dplyr::left_join(df_merge, tab, by = match_names) |>
    dplyr::select(-.data$PrimaryKey)

  for (cur_scale in scale_names) {
    df_mod[[cur_scale]] <- df_mod[[cur_scale]] *
      df_mod[[paste0(".", cur_scale)]]
  }

  df_mod <- df_mod |>
    dplyr::select(-dplyr::starts_with("."))

  df_mod <- df_mod |>
    dplyr::rename_with(~ paste0("match_", .),
                       .cols = !dplyr::any_of(scale_names)
    ) |>
    dplyr::rename_with(~ paste0("replace_", .),
                       .cols = dplyr::any_of(scale_names)
    )

  ## Shoudn't end up with NAs, but if something goes wrong with joins, want it to be obvious
  na_check <- df_mod |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), is.na))

  if (nrow(na_check) > 0) {
    cli::cli_warn("{nrow(na_check)} rows of results contain NAs. This is unexpected! Examine carefully.")
  }

  return(df_mod)
}

#'  `r lifecycle::badge("experimental")`
#' Zero out fishery catch for selected fisheries
#'
#' Nukes a fishery for one or more timesteps; useful to streamline scenario modeling during the NoF process. Leaves FisheryFlag alone, and sets scaler, quota, msf scaler, and msf quota to 0. If using `zero_fishery()` on a database that is loaded into FRAM, remember to reload the database before doing more modeling!
#'
#' @param fram_db Fram database connected
#' @param run_id One or more run IDs
#' @param fishery_id One or more fishery ids
#' @param time_step One or more timesteps. Default is NULL; in the case all ages are used.
#'
#' @export
#'
#' @examples
#' \dontrun{fram_db <-  connect_fram_db(here::here("testChinookFram.mdb"))
#'fram_db |>
#'  zero_fishery(run_id = 66, fishery_id = 12, time_step = 2:3)
#'disconnect_fram_db(fram_db)
#' }
zero_fishery <- function(fram_db, run_id, fishery_id, time_step = NULL){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)

  if (!is.null(time_step)) {
    if (!is.numeric(time_step)) {
      cli::cli_abort("`time_step` must be numeric")
    }
  }

  if(is.null(time_step)){
    if(fram_db$fram_db_species == "CHINOOK"){
      time_step <- 1:4
    }else{
      time_step <-  1:5
    }
  }

    df  <- tidyr::expand_grid(match_RunID = run_id,
                            match_FisheryID = fishery_id,
                            replace_FisheryScaleFactor = 0,
                            replace_Quota = 0,
                            replace_MSFFisheryScaleFactor = 0,
                            replace_MSFQuota = 0)
    if(!is.null(time_step)){
      df <- tidyr::expand_grid(df,
                             match_TimeStep = time_step)
    }
    out <- fram_db |> modify_table("FisheryScalers",
                            df = df)
    cli::cli_alert_success("{sum(out$rows_affected)} rows of FisheryScalers successfully zeroed out.")
    cli::cli_alert_warning("If FRAM is currently open, you will need to reload this database!")

}

