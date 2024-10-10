
## add in input checks.
## check coho

#' Compare tables in two equivalent FRAM databases
#'
#' Function supports QAQC practices by comparing the tables of two FRAM databases and identifying (and quantifying) differences.
#'
#' @param file1 Character string. FRAM database that contains the results from running baseline FRAM runs (e.g., our "production" version).
#' @param file2 Character string. FRAM database that contains the results from running modified FRAM runs (e.g., running a forked version of FRAM or using modified input values)
#' @param runid_use Numeric vector of the run_ids to compare. Optional. If not provided, compare all run ids in the databases.
#' @param tables_use Vector of character strings. Optional. If provided, only compare the listed tables.
#' @param slim Logical. Optional, defaults to FALSE. If TRUE, do not include `$tabs_file1` and `$tabs_file2` in output list.
#' @param verbose Logical, defaults to TRUE. When TRUE, print updates to console as function is run.
#'
#' @return List of lists and tibbles containing comparison information:
#' * `$ratios` tibble comparing every entry of every relevant column of every table. See "Details" for column descriptions.
#' * `$ratios_detailed` list of tibbles showing the contents of `$ratios` broken into tables, with additional non-compared columns present (e.g., `stock_name` in `$ratios_detailed$Stock`)
#' * `$nrow_tracker` dataframe providing the number of rows in each table of file1 (`$nrow.prod`), file2 (`$nrow.fork`), and the joined comparison (`$nrow.comp`). Useful to track down cause of many-to-many join warnings that can result from duplicated table entries; unless there are duplicate entries, `$nrow.comp` should be less than or equal to the minimum of `$nrow.prod` and `$nrow.fork`.
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
#' specify the table column being compared, respectively. `prop.err`, `abs.err`,
#' and `scale.err` provide measures of the changes between the "original" value
#' (from `file1`) and the "comparison" value (from `file2`). More on those below.
#' The `original` and `comparison` columns give the actual values being compared.
#' `run_id` through `time_step` specify the rows being compared. `bkfram.off.by.fish`
#'  and `bkfram.off.by.prop` provide the context for the comparison (more on that below).
#'
#'  **Quantifying error**
#'
#'  Because FRAM involves numerical solvers, we expect some small differences in table entries
#'  even when comparing two effectively equivalent databases. `compare_databases()` provides three metrics for these changes.
#'  In each case, it is assumed that `file1` is the reference file; the "error" measures all show how much the  value
#'  in `file2` changed relative to the corresponding value in `file1`.

#'  The simplest measure of error is the `abs.err`. This is the absolute value of the difference between
#'  the two values. If we're looking at an entry with table = "Mortality" and variable = "landed_catch",
#'  then an abs.err of 5 means that the `file2` entry was five fish more or less than the `file1` entry. You can confirm this
#'  by looking at the `original` and `comparison` columns. While `abs.err` is the most easily interpreted,
#'  it is often not very meaningful when looking across tables and variables. After all, an `abs.err` value of 5 could mean a
#'  a relatively meaningless change of five fish for a landed catch entry that was originally thousands of fish,
#'  but the same value of 5 would be a huge change in fishing effort if it were for a fishery scaler entry.
#'
#'  One way to make error comparable across tables and variables is to calculate the proportional error.
#'  If an entry changed by 0.01%, that's not meaningful, while if it changed by 10%, that is. `$prop.err` provides
#'  this proportional error, where -0.5 means the entry in file2 was 50% less than the corresponding value in file1,
#'  and a value of 2 means the entry in file2 was 200% more than the corresponding value in file2. '
#'  This gives error in context of the original value, and is often a good a way to look for problems. However,
#'  we sometimes find very large `$prop.err` values for changes that aren't concerning. For example,
#'  we may have an entry for landed catch in the mortality table that was 0.00001 fish in file1,
#'  and 0 fish in file2. In all practicality these two values are identical, and the
#'  0.00001 fish difference is likely one of random jitter in the numerical solver or rounding differences.
#'  However, our `$prop.err` value for this cell is `-1`, the most extreme negative change we can get.
#'  We can jointly look at `$abs.err` and `$prop.err` to address the potential for
#'  misleadingly large errors `$prop.err`, but it would be nice to have a single error metric that
#'  provides error in context without being sensitive to very small entries in file1.
#'
#'  `scale.err` is an elaboration on `$prop.err` that provides broader context. `$prop.err`
#'  takes the absolute error and scales by the original value in file1. `$scale.err` generalizes this idea,
#'  first calculating the average error for each table-variable combination, and then scaling the
#'  absolute error by the corresponding table-variable average. That is, if an entry for landed_catch in the
#'  Mortality table was 0.001 in file1, and was then 0.002 in file2, and the average of all landed_catch
#'  entries in file 1 was 1000, then the `prop.err` would be `1` (since file2 had double the value of file1, or `(0.002-0.001)/0.001`),
#'  and the `scale.err` would be `0.000001` (`(0.002-0.001)/1000`). This better captures our intuition
#'  that a difference of 0.001 fish in the landed catch isn't a big deal, since those values are typically huge.
#'  `scale.err` is thus a measure of error that is comparable across variables and tables, essentially answer the question
#'  "Has this entry changed a lot for this kind of variable and table?".
#'
#'  While `scale.err` is frequently the most useful error metric, `compare_databases()` provides all three.
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
#'  `$bkfram.off.by.fish` gives the absolute error in the corresponding row of the BackwardsFram table, and
#'  `bkfram.off.by.prop` give the relative error (as a proportion)
#'  in the corresponding row of the BackwardsFram table. If
#'  this bkfram wiggle were the cause of observed errors, we would expect the
#'  largest errors to correspond to the largest
#'  `$bkfram.off.by.fish` or `$bkfram.off.by.prop` values.
#'
#' **Suggestions**
#'
#' For simple plotting to see if the original and comparison values fall on the 1:1
#' line, `ggplot2::geom_point()` can be used, with `$ratios$original` and `$ratios$comparison`
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
                          slim = FALSE,
                          verbose = TRUE) {
  ## columns to NOT compare, and instead use as keys for for merging.
  labs.template  <-  c(
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

  ## template for populating missing columns with NAs when creating ratio.comp, below.
  cols <- c(
    run_id = NA_real_,
    fishery_id = NA_real_,
    stock_id = NA_real_,
    time_step = NA_real_,
    age = NA_real_
  )

  con.prod <- connect_fram_db(file1)
  con.fork <- connect_fram_db(file2)

  ## identify meaningful tables (dbListTables also returns Queries and internal Access tables)
  tables.use.prod <- intersect(DBI::dbListTables(con.prod$fram_db_connection),
                              provide_table_names())
  tables.use.fork <- intersect(DBI::dbListTables(con.fork$fram_db_connection),
                              provide_table_names())
  ## identify if databases have mismatching tables, give warning.
  tables.setdiff <- setdiff(tables.use.prod,
                           tables.use.fork)
  if (length(tables.setdiff) > 0) {
    cli::cli_alert_warning(
      "The provided database files contain different tables. Check that they are intended for comparison. Tables present in only one database: {tables.setdiff}"
    )
  }

  ## identify tables to compare
  tables.names <- intersect(tables.use.prod, tables.use.fork) |>
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
    tables.names <- intersect(tables.names, tables_use)
  }
  species.cur <- con.prod$fram_db_species

  tabs.prod <- tabs.fork <-  list()

  ## for the chinook case, figure out what the maximum number of stock is for backwards_fram id mapping
  stock.max <- max(c(fetch_table(con.prod, "BaseID")$num_stocks, fetch_table(con.fork, "BaseID")$num_stocks))

  if(verbose){cli::cli_alert_info("Fetching tables")}
  for (cur.table in tables.names) {
    tabs.prod[[cur.table]] <- fetch_table(con.prod, cur.table) |>
      dplyr::distinct()
    if (!is.null(runid_use) &
        "run_id" %in% names(tabs.prod[[cur.table]])) {
      tabs.prod[[cur.table]] <- tabs.prod[[cur.table]] |>
        dplyr::filter(.data$run_id %in% runid_use)
    }
    tabs.fork[[cur.table]] <- fetch_table(con.fork, cur.table)
    if (!is.null(runid_use) &
        "run_id" %in% names(tabs.fork[[cur.table]])) {
      tabs.fork[[cur.table]] <- tabs.fork[[cur.table]] |>
        dplyr::filter(.data$run_id %in% runid_use)
    }
    ## SCREW Chinook bkFRAM having a different stock_id numbering system. Relabeling here because no thanks.
    ## Also removing the "joint" bkfram stocks, as their inclusion makes it messy to look at anything else.
    if (cur.table == "BackwardsFRAM" & species.cur == "CHINOOK") {
      tabs.prod[[cur.table]] <- tabs.prod[[cur.table]] |>
        dplyr::rename(bk_stock_id = .data$stock_id) |>
        dplyr::left_join(framrosetta::bk_lookupfun_chin(stock.max) |> dplyr::select("bk_stock_id", "stock_id"),
                         by = "bk_stock_id") |>
        dplyr::filter(!is.na(.data$stock_id))
      tabs.fork[[cur.table]] <- tabs.fork[[cur.table]] |>
        dplyr::rename(bk_stock_id = .data$stock_id) |>
        dplyr::left_join(framrosetta::bk_lookupfun_chin(stock.max) |> dplyr::select("bk_stock_id", "stock_id"),
                         by = "bk_stock_id") |>
        dplyr::filter(!is.na(.data$stock_id))
    }
  }
  disconnect_fram_db(con.prod)
  disconnect_fram_db(con.fork)
  if(verbose){cli::cli_alert_success("Tables fetched")}


  tabs.comp <- list()
  nrow_tracker <- NULL ## for tracking dimensions
  ratio.comp <- NULL ## for storing ratios of new to old
  ratio.list <- list()

  if(verbose){cli::cli_alert_info("Handling off-by-fish bkFRAM calculations")}

  ## If backwardsFRAM is among the tables, identify differences before looking at other tables,
  ## so that those differences can be linked to other tables.
  if ("BackwardsFRAM" %in% names(tabs.prod)) {
    cur.table <- "BackwardsFRAM"
    labs.used <- intersect(names(tabs.prod[[cur.table]]),
                          labs.template)

    df.orig <- tabs.prod[[cur.table]] |>
      dplyr::select(!dplyr::any_of(c("primary_key"))) |>
      dplyr::filter(!is.na(.data$stock_id)) |>
      tidyr::pivot_longer(dplyr::starts_with("target_esc_age")) |>
      dplyr::mutate(age = as.numeric(gsub("target_esc_age", "", .data$name))) |>
      dplyr::rename(target.esc = .data$value) |>
      dplyr::select(-dplyr::any_of(c("name", "comment", "bk_stock_id", "target_flag")))


    df.fork <- tabs.fork[[cur.table]] |>
      dplyr::select(!dplyr::any_of(c("primary_key"))) |>
      dplyr::filter(!is.na(.data$stock_id)) |>
      tidyr::pivot_longer(dplyr::starts_with("target_esc_age")) |>
      dplyr::mutate(age = as.numeric(gsub("target_esc_age", "",.data$name))) |>
      dplyr::rename(target.esc = .data$value) |>
      dplyr::select(-dplyr::any_of(c("name", "comment", "bk_stock_id", "target_flag")))

    df.comp <- dplyr::full_join(
      df.orig,
      df.fork,
      by = c("run_id", "stock_id", "age"),
      suffix = c('_original', '_comparison')
    )
    ## resort names
    vec.comp <- c(grep("_original", names(df.comp)), grep("_comparison", names(df.comp)))
    df.comp <- df.comp[, c(names(df.comp)[-vec.comp], sort(names(df.comp)[vec.comp]))]
    df.comp <- df.comp |>
      dplyr::mutate(
        bkfram.off.by.fish = .data$target.esc_original - .data$target.esc_comparison,
        bkfram.off.by.prop = .data$bkfram.off.by.fish / .data$target.esc_original
      )
    bkfram.context <- df.comp
  } else{
    ## Make NA version so that later stuff works well
    bkfram.context <- NULL
  }

  for (cur.table in names(tabs.prod)) {
    if(verbose){cli::cli_alert_info("Diffing {cur.table}.")}
    labs.used <- intersect(names(tabs.prod[[cur.table]]),
                          labs.template)

    df.comp <- tabs.prod[[cur.table]] |>
      dplyr::select(!dplyr::any_of(c("primary_key"))) |>
      dplyr::inner_join(
        tabs.fork[[cur.table]] |>
          dplyr::select(!dplyr::any_of(c("primary_key"))),
        by = c(labs.used),
        suffix = c('_original', '_comparison')
      )
    ## resort names
    vec.comp <- c(grep("_original", names(df.comp)), grep("_comparison", names(df.comp)))
    df.comp <- df.comp[, c(names(df.comp)[-vec.comp], sort(names(df.comp)[vec.comp]))]

    ##store, but only if table isn't empty.
    if (nrow(df.comp) > 0) {
      tabs.comp[[cur.table]] <- df.comp
      res.cur <- NULL ## stores df of ratios for this table, goes into ratio.list.
      ## figure out which variables have been compared
      vars.comp <- grep("_original$", names(df.comp), value = TRUE)
      vars.comp <- gsub("_original$", "", vars.comp)
      for (var.cur in vars.comp) {
        temp <- df.comp |>
          dplyr::select(dplyr::starts_with(var.cur), dplyr::any_of(labs.template))
        names(temp)[1:2] <- gsub(paste0(var.cur, "_"), "", names(temp)[1:2])
        if (nrow(temp > 0) & is.numeric(temp$comparison)) {
          temp <- temp |>
            dplyr::mutate(
              prop.err = .data$comparison / .data$original - 1,
              abs.err = abs(.data$comparison - .data$original),
              scale.err = .data$abs.err / mean(.data$original),
              variable = var.cur,
              table = cur.table
            )
          if (all(c("stock_id", "run_id", "age") %in% names(temp)) & !is.null(bkfram.context)) {
            temp <- temp |>
              dplyr::left_join(bkfram.context |> dplyr::select(-dplyr::starts_with("target.esc")))
          } else{
            temp <- temp |>
              dplyr::mutate(bkfram.off.by.fish = NA,
                            bkfram.off.by.prop = NA)
          }
          ##adding in columns with NAs if needed, supporting flat df w/info for `ratios`. See https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
          temp <- tibble::add_column(temp, !!!cols[setdiff(names(cols), names(temp))])


          ratio.comp <- rbind(
            ratio.comp,
            temp |> dplyr::select(
              "table",
              "variable",
              "prop.err",
              "abs.err",
              "scale.err",
              "original",
              "comparison",
              "run_id",
              "fishery_id",
              "stock_id",
              "age",
              "time_step",
              "bkfram.off.by.fish",
              "bkfram.off.by.prop"
            )
          )
          res.cur <- rbind(res.cur, temp)
        }
      }
      ratio.list[[cur.table]] <- res.cur
    }
    nrow_tracker <- rbind(
      nrow_tracker,
      data.frame(
        table = cur.table,
        nrow.prod = nrow(tabs.prod[[cur.table]]),
        nrow.fork = nrow(tabs.fork[[cur.table]]),
        nrow.comp = nrow(df.comp)
      )
    )



  }
  res <- list(
    ratios = ratio.comp,
    ratios_detailed = ratio.list,
    nrow_tracker = nrow_tracker
  )
  if(!slim){
    res$tabs_file1 = tabs.prod
    res$tabs_file2 = tabs.fork
  }
  return(res)

}
