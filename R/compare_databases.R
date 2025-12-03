#'  `r lifecycle::badge("experimental")`
#' Compare tables in two equivalent FRAM databases
#'
#' Function supports QAQC practices by comparing the tables of two FRAM databases and identifying (and quantifying) differences.
#'
#' @param file1 Character string. FRAM database that contains the results from running baseline FRAM runs (e.g., our "production" version).
#' @param file2 Character string. FRAM database that contains the results from running modified FRAM runs (e.g., running a forked version of FRAM or using modified input values)
#' @param runid_use Numeric vector of the run_ids to compare. Optional. If not provided, compare all run ids in the databases.
#' @param tables_use Vector of character strings. Optional. If provided, only compare the listed tables.
#' @param slim Logical. Optional, defaults to TRUE. If TRUE, do not include `$tabs_file1` and `$tabs_file2` in output list.
#' @param quiet Logical, defaults to TRUE. When TRUE, suppress messages showing individual steps.
#'
#' @return List of lists and tibbles containing comparison information:
#' * `$ratios` tibble comparing every entry of every relevant column of every table. See "Details" for column descriptions.
#' * `$ratios_detailed` list of tibbles showing the contents of `$ratios` broken into tables, with additional non-compared columns present (e.g., `stock_name` in `$ratios_detailed$Stock`). Not shown if `slim` is TRUE.
#' * `$nrow_tracker` dataframe providing the number of rows in each table of file1 (`$nrow_original`), file2 (`$nrow_new`), and the joined comparison (`$nrow_comparison`). Useful to track down cause of many-to-many join warnings that can result from duplicated table entries; unless there are duplicate entries, `$nrow_comparison` should be less than or equal to the minimum of `$nrow_original` and `$nrow_new`.
#' * `$tabs_file1` List containing the original fetched tables from `file1`. Not returned if argument `slim` is TRUE
#' * `$tabs_file2` List containing the original fetched tables from `file2`. Not returned if argument `slim` is TRUE.
#'
#' @details
#' The key output is the `$ratios` tibble, which contains every comparison of relevant
#' table entries in long-form. These comparisons are implemented by first aligning
#' corresponding table rows using appropriate key columns (e.g. run_id, fishery_id,
#' stock_id, age, time_step, etc).
#'
#'
#' In `$ratios`, the `table` and `variable` columns
#' specify the table column being compared, respectively. `prop_err`, `abs_err`,
#' and `scale_err` provide measures of the changes between the "original" value
#' (from `file1`) and the "comparison" value (from `file2`). More on those below.
#' The `original` and `new` columns give the actual values being compared.
#' `run_id` through `time_step` specify the rows being compared. `bkfram_off_by_fish`
#'  and `bkfram_off_by_prop` provide the context for the comparison (more on that below).
#'
#'  **Quantifying error**
#'
#'  Because FRAM involves numerical solvers, we expect some small differences in table entries
#'  even when comparing two effectively equivalent databases. `compare_databases()` provides three metrics for these changes.
#'  In each case, it is assumed that `file1` is the reference file; the "error" measures all show how much the  value
#'  in `file2` changed relative to the corresponding value in `file1`.

#'  The simplest measure of error is the `abs_err`. This is the absolute value of the difference between
#'  the two values. If we're looking at an entry with table = "Mortality" and variable = "landed_catch",
#'  then an abs.err of 5 means that the `file2` entry was five fish more or less than the `file1` entry. You can confirm this
#'  by looking at the `original` and `new` columns. While `abs_err` is the most easily interpreted,
#'  it is often not very meaningful when looking across tables and variables. After all, an `abs_err` value of 5 could mean a
#'  a relatively meaningless change of five fish for a landed catch entry that was originally thousands of fish,
#'  but the same value of 5 would be a huge change in fishing effort if it were for a fishery scaler entry.
#'
#'  One way to make error comparable across tables and variables is to calculate the proportional error.
#'  If an entry changed by 0.01%, that's not meaningful, while if it changed by 10%, that is. `$prop_err` provides
#'  this proportional error, where -0.5 means the entry in file2 was 50% less than the corresponding value in file1,
#'  and a value of 2 means the entry in file2 was 200% more than the corresponding value in file2. '
#'  This gives error in context of the original value, and is often a good a way to look for problems. However,
#'  we sometimes find very large `$prop_err` values for changes that aren't concerning. For example,
#'  we may have an entry for landed catch in the mortality table that was 0.00001 fish in file1,
#'  and 0 fish in file2. In all practicality these two values are identical, and the
#'  0.00001 fish difference is likely one of random jitter in the numerical solver or rounding differences.
#'  However, our `$prop_err` value for this cell is `-1`, the most extreme negative change we can get.
#'  We can jointly look at `$abs_err` and `$prop_err` to address the potential for
#'  misleadingly large errors `$prop_err`, but it would be nice to have a single error metric that
#'  provides error in context without being sensitive to very small entries in file1.
#'
#'  `scale_err` is an elaboration on `$prop_err` that provides broader context. `$prop_err`
#'  takes the absolute error and scales by the original value in file1. `$scale_err` generalizes this idea,
#'  first calculating the average error for each table-variable combination, and then scaling the
#'  absolute error by the corresponding table-variable average. That is, if an entry for landed_catch in the
#'  Mortality table was 0.001 in file1, and was then 0.002 in file2, and the average of all landed_catch
#'  entries in file 1 was 1000, then the `prop_err` would be `1` (since file2 had double the value of file1, or `(0.002-0.001)/0.001`),
#'  and the `scale.err` would be `0.000001` (`(0.002-0.001)/1000`). This better captures our intuition
#'  that a difference of 0.001 fish in the landed catch isn't a big deal, since those values are typically huge.
#'  `scale_err` is thus a measure of error that is comparable across variables and tables, essentially answer the question
#'  "Has this entry changed a lot for this kind of variable and table?".
#'
#'  While `scale_err` is frequently the most useful error metric, `compare_databases()` provides all three.
#'  There may be contexts in which it's important to focus on the proportional error. For example,
#'  large proportional errors landed catch for the catch rare stocks can be important, but
#'  the much larger catch from other stock could water down the `scale_err` metric.
#'
#'  **Addressing the backwardsFRAM wiggle**
#'
#'  For post-season runs, the backwards FRAM algorithm is employed; its solver stops when
#'  the estimated fish numbers are within 1 fish of the target size. This means that there is
#'  the potential for substantial "wiggle" in bkFRAm values when comparing two databases.
#'  This wiggle can propagate to other tables, especially for stock-age-timesteps in which
#'  the target values were quite small (so a wiggle of +/- 1 fish would be a proportionally large
#'  amount). For this reason, it can be useful to see how our measures of error correspond to the errors in the
#'  corresponding bk fram table. For every table entry for which this makes sense (e.g., has a stock id, age, and timestep),
#'  `$bkfram_off_by_fish` gives the absolute error in the corresponding row of the BackwardsFram table, and
#'  `bkfram_off_by_prop` give the relative error (as a proportion)
#'  in the corresponding row of the BackwardsFram table. If
#'  this bkfram wiggle were the cause of observed errors, we would expect the
#'  largest errors to correspond to the largest
#'  `$bkfram_off_by_fish` or `$bkfram_off_by_prop` values.
#'
#' **Suggestions**
#'
#' For simple plotting to see if the original and new values fall on the 1:1
#' line, `ggplot2::geom_point()` can be used, with `$ratios$original` and `$ratios$new`
#'  for x and y, and a facet_wrap by `table` (and perhaps `variable`) to
#' make plots readable. For identifying meaningful change, scale.err is likely the best measure of error.
#' It can be helpful to plot scale.err against bkfram.off.by.fish or bkfram.off.by.prop to
#' see if the table entries with the largest error correspond to the stock-fishery-age-timestep in which
#' there's the largest wiggle in the backwards fram solutions.
#'
#' When digging into individual tables, it can sometimes be helpful to look at the comparisons in `$ratios_detailed`,
#' which contains additional columns which did not fit into the standardized formatting of `$ratios`.
#'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file1 = "Valid2022_Round_7_1_1_11142023_REFERENCE_fixed - fork rebuild.mdb"
#' file2 = "Valid2022_Round_7.1.1_11142023 - green river split.mdb"
#' out = tables_compare(file1, file2)
#' }
compare_databases <-  function(file1,
                          file2,
                          runid_use = NULL,
                          tables_use = NULL,
                          slim = TRUE,
                          quiet = TRUE) {

  if (length(file1) != 1 || !is.character(file1)) cli::cli_abort("`file1` must be a single character string")
  if (length(file2) != 1 || !is.character(file2)) cli::cli_abort("`file2` must be a single character string")
  if (!is.null(runid_use) && !all(is.numeric(runid_use))) cli::cli_abort("`runid_use` must be NULL or numeric vector")
  if (!is.null(tables_use) && !all(is.character(tables_use))) cli::cli_abort("`tables_use` must be NULL or character vector")
  if (!is.logical(slim) || length(slim) != 1) cli::cli_abort("`slim` must be a single logical value")
  if (!is.logical(quiet) || length(quiet) != 1) cli::cli_abort("`quiet` must be a single logical value")

  ## columns to NOT compare, and instead use as keys for for merging.
  labs_template  <-  c(
    "base_period_id",
    "stock_id",
    "run_id",
    "age",
    "time_step",
    "fishery_id",
    "stock_original",
    "bk_stock_id",
    "species",
    "management_unit_number",
    "stock_name",
    "production_region_number",
    "version_number",
    "time_step_id",
    "stock_long_name",
    "psc_stock_id",
    "report_number"
  )

  ## template for populating missing columns with NAs when creating ratio_comp, below.
  cols <- c(
    run_id = NA_real_,
    fishery_id = NA_real_,
    stock_id = NA_real_,
    time_step = NA_real_,
    age = NA_real_
  )

  con_prod <- connect_fram_db(file1, quiet = quiet)
  con_fork <- connect_fram_db(file2, quiet = quiet)

  ## identify meaningful tables (dbListTables also returns Queries and internal Access tables)
  tables_use_prod <- intersect(DBI::dbListTables(con_prod$fram_db_connection),
                              provide_table_names())
  tables_use_fork <- intersect(DBI::dbListTables(con_fork$fram_db_connection),
                              provide_table_names())
  ## identify if databases have mismatching tables, give warning.
  tables_setdiff <- setdiff(tables_use_prod,
                           tables_use_fork)
  if (length(tables_setdiff) > 0) {
    cli::cli_alert_warning(
      "The provided database files contain different tables. Check that they are intended for comparison. Tables present in only one database: {tables_setdiff}"
    )
  }

  ## identify tables to compare
  tables_names <- intersect(tables_use_prod, tables_use_fork) |>
    ## remove a few unnecessarily/useless tables
    purrr::discard(
      \(x) x %in% c(
        "TAAETRSList",
        "ReportDriver",
        "ChinookBaseSizeLimit_Old",
        "StockRecruitExample",
        "SLRatioVerification"
      )
    )
  if(!is.null(tables_use)){
    tables_names <- intersect(tables_names, tables_use)
  }
  species_cur <- con_prod$fram_db_species

  tabs_prod <- tabs_fork <-  list()

  ## for the chinook case, figure out what the maximum number of stock is for backwards_fram id mapping
  stock_max <- max(c(fetch_table(con_prod, "BaseID")$num_stocks, fetch_table(con_fork, "BaseID")$num_stocks))

  if(!quiet){cli::cli_alert_info("Fetching tables")}
  for (cur_table in tables_names) {
    tabs_prod[[cur_table]] <- fetch_table(con_prod, cur_table, warn = FALSE, label = FALSE) |>
      dplyr::distinct()
    if (!is.null(runid_use) &
        "run_id" %in% names(tabs_prod[[cur_table]])) {
      tabs_prod[[cur_table]] <- tabs_prod[[cur_table]] |>
        dplyr::filter(.data$run_id %in% runid_use)
    }
    tabs_fork[[cur_table]] <- fetch_table(con_fork, cur_table, warn = FALSE, label = FALSE)
    if (!is.null(runid_use) &
        "run_id" %in% names(tabs_fork[[cur_table]])) {
      tabs_fork[[cur_table]] <- tabs_fork[[cur_table]] |>
        dplyr::filter(.data$run_id %in% runid_use)
    }
    ## SCREW Chinook bkFRAM having a different stock_id numbering system. Relabeling here because no thanks.
    ## Also removing the "joint" bkfram stocks, as their inclusion makes it messy to look at anything else.
    if (cur_table == "BackwardsFRAM" & species_cur == "CHINOOK") {
      tabs_prod[[cur_table]] <- tabs_prod[[cur_table]] |>
        dplyr::rename(bk_stock_id = .data$stock_id) |>
        dplyr::left_join(framrosetta::bk_lookupfun_chin(stock_max) |> dplyr::select("bk_stock_id", "stock_id"),
                         by = "bk_stock_id") |>
        dplyr::filter(!is.na(.data$stock_id))
      tabs_fork[[cur_table]] <- tabs_fork[[cur_table]] |>
        dplyr::rename(bk_stock_id = .data$stock_id) |>
        dplyr::left_join(framrosetta::bk_lookupfun_chin(stock_max) |> dplyr::select("bk_stock_id", "stock_id"),
                         by = "bk_stock_id") |>
        dplyr::filter(!is.na(.data$stock_id))
    }
  }
  disconnect_fram_db(con_prod)
  disconnect_fram_db(con_fork)
  if(!quiet){cli::cli_alert_success("Tables fetched")}


  tabs_comp <- list()
  nrow_tracker <- NULL ## for tracking dimensions
  ratio_comp <- NULL ## for storing ratios of new to old
  ratio_list <- list()

  if(!quiet){cli::cli_alert_info("Handling off-by-fish bkFRAM calculations")}

  ## If backwardsFRAM is among the tables, identify differences before looking at other tables,
  ## so that those differences can be linked to other tables.
  if ("BackwardsFRAM" %in% names(tabs_prod)) {
    cur_table <- "BackwardsFRAM"
    labs_used <- intersect(names(tabs_prod[[cur_table]]),
                          labs_template)

    df_orig <- tabs_prod[[cur_table]] |>
      dplyr::select(!dplyr::any_of(c("primary_key"))) |>
      dplyr::filter(!is.na(.data$stock_id)) |>
      tidyr::pivot_longer(dplyr::starts_with("target_esc_age")) |>
      dplyr::mutate(age = as.numeric(gsub("target_esc_age", "", .data$name))) |>
      dplyr::rename(target_esc = .data$value) |>
      dplyr::select(-dplyr::any_of(c("name", "comment", "bk_stock_id", "target_flag")))


    df_fork <- tabs_fork[[cur_table]] |>
      dplyr::select(!dplyr::any_of(c("primary_key"))) |>
      dplyr::filter(!is.na(.data$stock_id)) |>
      tidyr::pivot_longer(dplyr::starts_with("target_esc_age")) |>
      dplyr::mutate(age = as.numeric(gsub("target_esc_age", "",.data$name))) |>
      dplyr::rename(target_esc = .data$value) |>
      dplyr::select(-dplyr::any_of(c("name", "comment", "bk_stock_id", "target_flag")))

    df_comp <- dplyr::full_join(
      df_orig,
      df_fork,
      by = c("run_id", "stock_id", "age"),
      suffix = c('_original', '_new')
    )
    ## resort names
    vec_comp <- c(grep("_original", names(df_comp)), grep("_new", names(df_comp)))
    df_comp <- df_comp[, c(names(df_comp)[-vec_comp], sort(names(df_comp)[vec_comp]))]
    df_comp <- df_comp |>
      dplyr::mutate(
        bkfram_off_by_fish = .data$target_esc_original - .data$target_esc_new,
        bkfram_off_by_prop = .data$bkfram_off_by_fish / .data$target_esc_original
      )
    bkfram_context <- df_comp
  } else{
    ## Make NA version so that later stuff works well
    bkfram_context <- NULL
  }

  cli::cli_progress_bar("Comparing tables", total = length(tabs_prod))
  for (cur_table in names(tabs_prod)) {
    if(!quiet){cli::cli_alert_info("Diffing {cur_table}.")}
    labs_used <- intersect(names(tabs_prod[[cur_table]]),
                          labs_template)

    df_comp <- tabs_prod[[cur_table]] |>
      dplyr::select(!dplyr::any_of(c("primary_key"))) |>
      dplyr::inner_join(
        tabs_fork[[cur_table]] |>
          dplyr::select(!dplyr::any_of(c("primary_key"))),
        by = c(labs_used),
        suffix = c('_original', '_new')
      )
    ## resort names
    vec_comp <- c(grep("_original", names(df_comp)), grep("_new", names(df_comp)))
    df_comp <- df_comp[, c(names(df_comp)[-vec_comp], sort(names(df_comp)[vec_comp]))]

    ##store, but only if table isn't empty.
    if (nrow(df_comp) > 0) {
      tabs_comp[[cur_table]] <- df_comp
      res_cur <- NULL ## stores df of ratios for this table, goes into ratio.list.
      ## figure out which variables have been compared
      vars_comp <- grep("_original$", names(df_comp), value = TRUE)
      vars_comp <- gsub("_original$", "", vars_comp)
      for (var_cur in vars_comp) {
        temp <- df_comp |>
          dplyr::select(dplyr::starts_with(var_cur), dplyr::any_of(labs_template))
        names(temp)[1:2] <- gsub(paste0(var_cur, "_"), "", names(temp)[1:2])
        if (nrow(temp > 0) & is.numeric(temp$new)) {
          temp <- temp |>
            dplyr::mutate(
              prop_err = .data$new / .data$original - 1,
              abs_err = abs(.data$new - .data$original),
              scale_err = .data$abs_err / mean(.data$original),
              variable = var_cur,
              table = cur_table
            )
          if (all(c("stock_id", "run_id", "age") %in% names(temp)) & !is.null(bkfram_context)) {
            temp <- temp |>
              dplyr::left_join(bkfram_context |> dplyr::select(-dplyr::starts_with("target_esc")),
                               by = c("stock_id", "run_id", "age"))
          } else{
            temp <- temp |>
              dplyr::mutate(bkfram_off_by_fish = NA,
                            bkfram_off_by_prop = NA)
          }
          ##adding in columns with NAs if needed, supporting flat df w/info for `ratios`. See https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
          temp <- tibble::add_column(temp, !!!cols[setdiff(names(cols), names(temp))])


          ratio_comp <- rbind(
            ratio_comp,
            temp |> dplyr::select(
              "table",
              "variable",
              "prop_err",
              "abs_err",
              "scale_err",
              "original",
              "new",
              "run_id",
              "fishery_id",
              "stock_id",
              "age",
              "time_step",
              "bkfram_off_by_fish",
              "bkfram_off_by_prop"
            )
          )
          res_cur <- rbind(res_cur, temp)
        }
      }
      ratio_list[[cur_table]] <- res_cur
    }
    nrow_tracker <- rbind(
      nrow_tracker,
      data.frame(
        table = cur_table,
        nrow_original = nrow(tabs_prod[[cur_table]]),
        nrow_new = nrow(tabs_fork[[cur_table]]),
        nrow_comparison = nrow(df_comp)
      )
    )
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  res <- list(
    ratios = ratio_comp,
    nrow_tracker = nrow_tracker
  )
  if(!slim){
    res$ratios_detailed = ratio_list
    res$tabs_file1 = tabs_prod
    res$tabs_file2 = tabs_fork
  }
  return(res)

}
