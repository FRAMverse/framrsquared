
#' Check FRAM database after adding new stock
#'
#' Either provides the step by step process of adding new stock to a FRAM database, or walks through fram database run
#' and checks the tables for potential errors associated with adding new stock.
#'
#' @param file_name filepath to database. If `NULL`, provide summary of process instead. Default = `NULL`.
#' @param run_id RunID associated with the new stock in the FRAM database. IF left `NULL`, provide summary of process instead. Default = `NULL`.
#' @param old_stockcount The number of stocks previously present to treat as the "baseline" -- several checking steps will focus solely on newly added stocks. Defaults to 78.
#' @param override_db_checks Ignore species, database type. When `FALSE`, function will stop if the database is not Chinook or if it's a transfer file. Defaults to FALSE.
#'
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#' ## review process
#' addstock_check()
#' ## check database for additional stock
#' addstock_check("2024 Pre-Season Chinook DB - first test.mdb",
#' run_id = 138)
#' }
# CBE test location: C:/Repos/fram_dev_material/FRAM QA QC/comparing/2024 Pre-Season Chinook DB - first test.mdb
addstock_check <-
  function(file_name = NULL,
           run_id = NULL,
           old_stockcount = 78,
           override_db_checks = FALSE) {


    # print("Add check for FRAM db, not transfer, is chinook. Unclear what would need to be different for Coho. Add override_db_checks toggle.")

    if (is.null(file_name) | is.null(run_id)) {
      #Print instructions
      tab_list = c(
        "*" = "BaseID: increase NumStk argument.",
        "*" = "Stock: add info for new stock.",
        "*" = "StockRecruit: add info for new stock x age, including `RecruitScale`. `RecruitCoho` will get updated automatically.",
        "*" = "BaseCohort: add new entries for stock x age.",
        "*" = "AEQ: I think this might get calculated for us?",
        "*" = "BackwardsFRAM: complicated. See below.",
        "*" = "BaseExploitationRate: add new entries for stock x age x fisheryies. Easiest to manipulate in Excel.",
        "*" = "Growth: add new entries for stock.",
        "*" = "MaturationRate: add new entries for stock x age x timestep. Easiest to manipulate in Excel."
      )
      cli::cli_alert(
        "`file_name` or `run_id` not detected. Providing overview of steps to add stock instead."
      )
      cli::cli_alert(
        "When adding new Chinook stock, the following tables must be updated with the new stock:"
      )
      cli::cli_ol(tab_list)
      cli::cli_alert(
        "\nRemember that we make two versions of each stock: unmarked (odd stockID) and marked (even stockID)."
      )
      cli::cli_alert(
        "When updating BackwardsFRAM table, need to add THREE rows for every unmarked+marked stock pair:"
      )
      ulid = cli::cli_ul()
      cli::cli_li(
        "The first row is the combined entry, with the each of the `TargetEsc*` columns containing the sum of column values for the unmarked and marked stock. The TargetFlag for this row should be 0."
      )
      cli::cli_li("Then the next two rows are the unmarked and marked stock, with TargetFlag = 3.")
      cli::cli_end(ulid)

    } else{


      error_count = 0

      cli::cli_alert("Checking for additional stock (beyond stockID = 78)...")

      con = connect_fram_db(file_name)

      ## checking database qualities
      if(con$fram_db_species != "CHINOOK"){
        if(override_db_checks){
          cli::cli_alert_warning("`addstock_check()` is currently designed for Chinook, but this is a {con$fram_db_species} database. `override_db_checks` set to `TRUE`, so overriding this concern...")
          cli::cli_text()

        }else{
          cli::cli_abort("`addstock_check()` is currently designed for Chinook, but this is a {con$fram_db_species} database. To override this error, set `override_db_checks` to `TRUE`.")
        }
      }

      if(con$fram_db_type != "full"){
        if(override_db_checks){
          cli::cli_alert_warning("`addstock_check()` is designed for full FRAM databases, but this is a {con$fram_db_type} database. `override_db_checks` set to `TRUE`, so overriding this concern, but interpret results with caution...")
          cli::cli_text()

        }else{
          cli::cli_abort("`addstock_check()` is designed for full FRAM databases, but this is a {con$fram_db_type} database. To override this error, set `override_db_checks` to `TRUE`.")
        }
      }


      run_info = fetch_table(con, "RunID")
      if (!run_id %in% run_info$run_id) {
        cli::cli_abort(
          paste0(
            "`run_id` value must be present in database. Available run_ids: ",
            paste0(run_info$run_id, collapse = ", "),
            "."
          )
        )
      }
      ## get the base period ID.
      bp_id = run_info |> dplyr::filter(.data$run_id == .env$run_id) |> dplyr::pull(.data$base_period_id)

      ## BaseID -- get NumStk
      cli::cli_text()
      cli::cli_alert("Checking BaseID table...")
      baseid_df = fetch_table(con, "BaseID") |>
        dplyr::filter(.data$base_period_id == .env$bp_id)
      NumStk = baseid_df |> dplyr::pull(.data$num_stocks)
      cli::cli_alert(paste0("  NumStk = ", NumStk))
      if (NumStk %% 2 != 0) {
        cli::cli_alert_danger(
          "NumStk ({NumStk}) should be an even number, as we add stocks as a pair of unmarked and marked stock. FRAM relies on this pattern."
        )
        error_count = error_count + 1
      }

      ## Stock table
      cli::cli_text()
      cli::cli_alert("Checking Stock table...")
      stock_df = fetch_table(con, "Stock") |>
        dplyr::arrange(dplyr::desc(.data$stock_id)) #|>
      #dplyr::mutate(stock_id = dplyr::if_else(stock_id == 80, 81, stock_id)) # for testing the error messages.
      ## dimension check
      ## check for presence of stock, stock numbering
      error_count = error_count + stock_check_helper(
        "Stock",
        NumStk = NumStk,
        stock_vec = stock_df$stock_id,
        uniques_only = TRUE
      )

      if (!(all(grep("^M-", stock_df$stock_name) %% 2 == 1) &
            all(grep("^U-", stock_df$stock_name) %% 2 == 0))) {
        #marked and unmarked don't follow the right pattern
        cli::cli_alert_danger(
          "Entries of Stock table should be Unmarked (odd number) and marked (even number), and `StockName` column should correspondingly start with \"U-\" and \"M-\"."
        )
        error_count = error_count + 1
      } else{
        cli::cli_alert_success("  Stock naming convention followed.")
      }

      ## StockRecruit table
      cli::cli_text()
      cli::cli_alert("Checking StockRecruit table...")
      stock_recruit_df = fetch_table(con, "StockRecruit") |>
        dplyr::filter(.data$run_id == .env$run_id)
      ## check for presence of stock, stock numbering

      error_count = error_count + stock_id_comp("StockRecruit", stock_recruit_df, stock_ref = stock_df$stock_id)
      ## checking stock_age combinations
      error_count = error_count + stock_age_checker(
        "StockRecruit",
        NumStk = NumStk,
        old_stockcount = old_stockcount,
        df = stock_recruit_df,
        min_age = baseid_df$min_age,
        max_age = baseid_df$max_age
      )

      ## BaseCohort
      cli::cli_text()
      cli::cli_alert("Checking BaseCohort table...")
      base_cohort_df = fetch_table(con, "BaseCohort") |>
        dplyr::filter(.data$base_period_id == .env$bp_id)
      ## check that stock IDs make sense
      error_count = error_count + stock_id_comp("BaseCohort", base_cohort_df, stock_ref = stock_df$stock_id)
      ## checking stock_age combinations
      error_count = error_count + stock_age_checker(
        "BaseCohort",
        NumStk = NumStk,
        old_stockcount = old_stockcount,
        df = base_cohort_df,
        min_age = baseid_df$min_age,
        max_age = baseid_df$max_age
      )

      ## AEQ
      cli::cli_text()
      cli::cli_alert("Checking AEQ table...")
      cli::cli_alert_warning("(Not yet implemented. I think we don't need to?)")

      ## BackwardsFRAM
      cli::cli_text()
      cli::cli_alert("Checking BackwardsFRAM table...")
      cli::cli_alert_info("  Remember, StockID here is different from everywhere else, unfortunately.")
      bkfram_df = fetch_table(con, "BackwardsFRAM") |>
        dplyr::filter(.data$stock_id > old_stockcount * 3 / 2 - 1) |>  ## filter to recently added stock. Formula is annoying, but so is bkfram stock_id
        dplyr::mutate("stock_pop_id" = floor(.data$stock_id / 3))
      vals_expect = (old_stockcount * 3 / 2):(NumStk * 3 / 2 - 1)
      if (all(vals_expect %in% bkfram_df$stock_id) &
          all(bkfram_df$stock_id %in% vals_expect)) {
        cli::cli_alert_success("  Appropriate `StockID` values present in BackwardsFRAM table.")
      } else{
        cli::cli_alert_danger(
          "  Expected StockID {sort(vals_expect)} in BackwardsFRAM table, but had StockID {sort(bkfram_df$stock_id)}"
        )
        error_count = error_count + 1
      }
      ## check that our flags are right
      if (all(bkfram_df$target_flag[bkfram_df$stock_id %% 3 == 0] == 0)) {
        cli::cli_alert_success("  TargetFlag set to 0 when appropriate")
      } else {
        cli::cli_alert_danger(
          "  The first row associated with each new stock-pair must be the sum of marked and unmarked, and must have TargetFlag value of 3."
        )
        error_count = error_count + 1
      }
      if (all(bkfram_df$target_flag[bkfram_df$stock_id %% 3 != 0] == 3)) {
        cli::cli_alert_success("  TargetFlag set to 3 when appropriate")
      } else {
        cli::cli_alert_danger(
          "  The second and third row associated with each new stock-pair must be the marked and unmarked targets, and must have TargetFlag values of 3."
        )
        error_count = error_count + 1
      }
      ## check summation
      bk_sum = bkfram_df |>
        dplyr::filter(.data$target_flag == 3) |>
        dplyr::group_by(.data$stock_pop_id) |>
        dplyr::summarize(
          target_esc_age3 = sum(.data$target_esc_age3),
          target_esc_age4 = sum(.data$target_esc_age4),
          target_esc_age5 = sum(.data$target_esc_age5)
        )
      sum_dif = bk_sum - (
        bkfram_df |>
          dplyr::filter(.data$target_flag == 0) |>
          dplyr::select(
            "stock_pop_id",
            "target_esc_age3",
            "target_esc_age4",
            "target_esc_age5"
          )
      )
      sum_dif$stock_pop_id = (bkfram_df |>
                                dplyr::filter(.data$target_flag == 0) |> dplyr::pull(.data$stock_pop_id))
      if (sum(sum_dif |> dplyr::select(-"stock_pop_id")) == 0) {
        cli::cli_alert_success("  Unmarked and marked stock targets are summing to the combined target.")
      } else{
        cli::cli_alert_danger("  Unmarked and marked stock targets are not summing to the combined target.")
        cli::cli_alert_danger(
          "  The following shows differences between expected sum and actual sum by stock (non-zeroes are problems)"
        )
        print(sum_dif)
        error_count = error_count + 1
      }

      ## BaseExploitationRate
      cli::cli_text()
      cli::cli_alert("Checking BaseExploitationRate table...")
      bper_df = fetch_table(con, "BaseExploitationRate") |>
        dplyr::filter(.data$base_period_id == .env$bp_id)
      error_count = error_count + stock_id_comp("BaseExploitationRate", bper_df, stock_ref = stock_df$stock_id)
      cli::cli_alert("  Cannot easily check that no stock_age-fishery-timestep values are missing.")
      bper_sum = bper_df |>
        dplyr::filter(.data$stock_id > old_stockcount) |>
        dplyr::group_by(.data$stock_id) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::mutate("lab" = paste0("Stock ", .data$stock_id, ": ", .data$n, " records"))
      cli::cli_text(
        "  Most common number of records across stock: {tail(table(bper_df$stock_id),1)[[1]]}"
      )
      cli::cli_text("  Records for new stock:")
      cli::cli_bullets(stats::setNames(bper_sum$lab, rep("*", nrow(bper_sum))))


      ## Growth
      cli::cli_text()
      cli::cli_alert("Checking Growth table...")
      growth_df = fetch_table(con, "Growth")
      error_count = error_count + stock_check_helper(
        "Growth",
        NumStk = NumStk,
        stock_vec = growth_df$stock_id,
        uniques_only = TRUE
      )
      error_count = error_count + stock_id_comp("Growth", growth_df, stock_ref = stock_df$stock_id)

      ## MaturationRate
      cli::cli_text()
      cli::cli_alert("Checking MaturationRate table...")
      mat_df = fetch_table(con, "MaturationRate")
      stock_id_comp("MaturationRate", mat_df, stock_ref = stock_df$stock_id)
      mat_df = mat_df |> dplyr::filter(.data$stock_id > old_stockcount)
      ## Checking for stock_age-timestep combinations
      ts_df = fetch_table(con, "TimeStep")
      mat_df_comp = paste0("StockID: ",
                           mat_df$stock_id,
                           " Age: ",
                           mat_df$age,
                           " TimeStep: ",
                           mat_df$time_step)
      comp_df = tidyr::expand_grid(
        stock_id = (old_stockcount + 1):NumStk,
        age = baseid_df$min_age:baseid_df$max_age,
        time_step = ts_df$time_step_id
      )
      comp_df_comp = paste0(
        "StockID: ",
        comp_df$stock_id,
        " Age: ",
        comp_df$age,
        " TimeStep: ",
        comp_df$time_step
      )
      if (all(mat_df_comp %in% comp_df_comp) &
          all(comp_df_comp %in% mat_df_comp)) {
        cli::cli_alert_success(
          "  All appropriate stock_age-timestep combinations represented in MaturationRate table."
        )
      } else{
        cli::cli_alert_danger(
          c(
            "  Missing stock_age-timestep combinations:",
            stats::setNames(setdiff(comp_df_comp, mat_df_comp), rep("*", length(
              setdiff(comp_df_comp, mat_df_comp)
            ))),
            "Unexpected stock_age combinations:",
            ifelse(length(
              setdiff(mat_df_comp, comp_df_comp)
            ) > 0,
            stats::setNames(
              setdiff(mat_df_comp, comp_df_comp), rep("*", length(
                setdiff(mat_df_comp, comp_df_comp)
              ))
            ),
            " (none)")
          )
        )
        error_count = error_count + 1
      }
      disconnect_fram_db(con)
      cli::cli_text()
      cli::cli_rule()
      cli::cli_text()
      cli::cli_div(theme = list(span.strong = list(color = "blue")))
      cli::cli_text("{.strong Evaluation complete:}")
      if (error_count == 0) {
        cli::cli_text()
        cli::cli_alert_success("No issues detected! Great work!")
        cli::cli_text()
      } else {
        cli::cli_alert_danger("{error_count} issues detected")
      }
  }
    }

#' Helper function to check that stock id exist in the Stock database
#'
#' Intended for internal use, makes some assumptions about inputs.
#'
#' @param table_name Character of table name, for informative messages
#' @param df Dataframe
#' @param stock_ref numeric vector of all stock IDs. Should be stock_df$stock_id.
#'
#' @return numeric; 0 if no warning, 1 if warning.
stock_id_comp = function(table_name, df, stock_ref) {
  #helper function to check that stock id exist in the Stock database
  if (all(unique(df$stock_id) %in% stock_ref)) {
    cli::cli_alert_success("  Stock IDs match those of the Stock table.")
    return(0)
  } else{
    cli::cli_alert_danger(
      "  Unexpected StockID entry(s) in {table_name} table: {setd(unique(df$stock_id), stock_ref)}. This ID is not present in the Stock table."
    )
    return(1)
  }
}

#' Helper function to check that all stock x age combinations are present
#'
#'#' Intended for internal use, makes some assumptions about inputs.
#'
#' @param table_name Character of table name, for informative messages.
#' @param NumStk Maximum number of stock, pulled from BaseID table
#' @param old_stockcount Number of stock in previous FRAM baseperiod. Only looks for problems for StockID > this number.
#' @param df Dataframe to check. Must have columns "stock_id" and "age" (which are the names for relevant columns of framrsquared::fetch_table).
#' @param min_age Minimum age modeled. Should be the min_age from the baseid_df.
#' @param max_age Maximum age modeled. Should be  the max_age from the baseid_df.
#'
#' @return numeri; 0 if no warning, 1 if warning.
#'
stock_age_checker = function(table_name,
                             NumStk,
                             old_stockcount,
                             df,
                             min_age,
                             max_age) {
  #helper function to check that each relevant stock_age combination is present in the newly added stock.
  ## internal use only. Note that it generates the complete combo of stock x age combinatoins based on the min_age and max_age from the base_period table.
  df = df |>
    dplyr::filter(.data$stock_id > old_stockcount) |>
    tidyr::unite("stock_age", "stock_id":"age", sep = " age ")
  df_comp = tidyr::expand_grid(stock_id = (old_stockcount + 1):NumStk,
                               age = min_age:max_age) |>
    tidyr::unite("stock_age", "stock_id":"age", sep = " age ")
  if (all(df$stock_age %in% df_comp$stock_age) &
      all(df_comp$stock_age %in% df$stock_age)) {
    ## not missing anything
    cli::cli_alert_success("  All appropriate stock_age combinations represented in {table_name} table.")
    return(0)
  } else{
    cli::cli_alert_danger(
      "  Missing stock_age combinations {setd(df_comp$stock_age, df$stock_age)}. Unexpected stock_age combinations: {setd(df$stock_age, df_comp$stock_age)}"
    )
    return(1)
  }
}

#' Helper function to check that stock id make sense
#'
#' More thorough checking than stock_id_comp. Checks that the number of stock IDs makes sense given `NumStk`,
#' that Stock IDs are sequential (in the sense that if NumStk = n, every integer up to n is represented).
#' Optionally, can check that each stock ID is unique.
#'
#' @param table_name Character of table name, for informative messages.
#' @param NumStk Maximum number of stock, pulled from BaseID table
#' @param stock_vec vector of stock ids to check. Presumably column of fetched table.
#' @param uniques_only Do we want warnings if there are duplicats of StockIDs? Useful for tables like Stock and Growth that should have only one entry per stock. Logical, default = `FALSE`.
#'
#' @return Numeric, returning number of warnings detected.
#'
stock_check_helper = function(table_name,
                              NumStk,
                              stock_vec,
                              uniques_only = FALSE) {
  cur_err = 0
  if (uniques_only) {
    if (length(stock_vec) == NumStk) {
      cli::cli_alert_success("  One entry per stock.")
    } else {
      cli::cli_alert_danger(
        "  Duplicate StockID detected! Look for duplicate StockID(s) {paste0(unique(stock_vec[duplicated(stock_vec)]), collapse = ', ')}"
      )
      cur_err = cur_err + 1
    }
  }

  if (length(unique(stock_vec)) == NumStk) {
    cli::cli_alert_success("  Number of stock in {table_name} table matches NumStk ({NumStk})")
  } else{
    cli::cli_alert_danger(
      "  Number of stock in {table_name} table ({length(unique(stock_vec))}) must match NumStk ({NumStk}). Did you update the BaseID table `NumStock` column? Did you add the new stock to the {table_name} table?"
    )
    cur_err = cur_err + 1
  }
  ## checking that stock numbers are sequential
  if (all(unique(stock_vec) %in% 1:NumStk)) {
    cli::cli_alert_success("  StockID appears to be sequential (1:{NumStk})")
  } else{
    cli::cli_alert_danger(
      "  StockID in {table_name} table is not sequential -- missing StockID {paste0(setdiff(1:NumStk, unique(stock_vec)), collapse = ', ')}, have unexpected StockID {paste0(setdiff(unique(stock_vec), 1:NumStk), collapse = ', ')}"
    )
    cur_err = cur_err + 1
  }
  return(cur_err)
}

setd = function(a, b) {
  ## minor helper function, largely superfluous. Turns out cli:: functions that support glue
  ## will do the collapsing automatically. The above could be refactored to avoid this function.
  paste0(setdiff(unique(a), b), collapse = ', ')
}
