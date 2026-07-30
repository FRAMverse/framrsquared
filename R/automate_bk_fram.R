
## helper function: pause and wait for user input
stop_for_step <- function(msg){

  yes_flag = FALSE
  has_run = FALSE
  while(!yes_flag){
    if(has_run){
      cli::cli_alert_info("(that was not `done`)")
    }
    purrr::walk(msg, cli::cli_alert)
    cli::cli_alert("Type {.val done} when done")
    inp <- readline("")
    if(inp %in% c("done")){
      yes_flag = TRUE
    }
    has_run = TRUE
  }
}

bkfram_coastal_helper <- function(excel_path,
         fram_path,
         run_id){

  rlang::check_installed("readxl")
  rlang::check_installed("openxlsx2")


  ## run 183 is a copy of run 182 to compare to.

  cli::cli_h1("Preparing backwards run")

  raw <- readxl::read_excel(excel_filepath,
                    sheet = "Terminal ETRS",
                    .name_repair = "unique_quiet")
  names(raw)[1] <- "stock"

  first_empty = min(which(is.na(raw$stock)))

  raw <- raw[1:(first_empty-1),]

  dat <- raw |>
    ## clean up manual character NAs, convert to numeric as needed
    ## Any warnings are REAL, not relics of "NA" -> `NA`, and should be examined carefully
    dplyr::mutate(across("Escapement":"ETRS",
                  \(x){as.numeric(dplyr::if_else(x == "NA",
                                                 NA,
                                                 x))})) |>
    dplyr::pivot_longer(cols = -c("stock", "FLAG"),
                 names_to = "fishery") |>
    dplyr::mutate(FLAG = dplyr::if_else(.data$fishery %in% c("Escapement", "ETRS"),
                          .data$FLAG,
                          NA_integer_))


  ## map stocks + fisheries to ids

  fram_db <- connect_fram_db(fram_path, quiet = TRUE)

  existing_runs = get_run_ids(fram_db)

  stock_lut <- fetch_table(fram_db, "Stock") |>
    select(stock = "stock_long_name", "stock_id")

  fishery_lut <- fetch_table(fram_db, "Fishery") |>
    select(fishery = "fishery_title", "fishery_id")

  dat <- left_join(dat, stock_lut, by = "stock") |>
    left_join(fishery_lut, by = "fishery")

  ## check for lut issues / label mismatches

  bad_fishery <- dat |>
    dplyr::filter(is.na(fishery_id)) |>
    dplyr::pull(fishery) |>
    unique() |>
    setdiff(c("Escapement", "ETRS"))

  if(length(bad_fishery) > 0 ){
    cli::cli_abort(c(
      "The following fishery names in the excel file don't have corresponding `fishery_title` matches in the Fishery table of the FRAM database:",
      stats::setNames(bad_fishery, rep("*", length(bad_fishery)))
    ))
  }

  bad_stock <- dat |>
    dplyr::filter(is.na(stock_id)) |>
    dplyr::pull(stock) |>
    unique() |>
    setdiff(c("Total Catch"))

  if(length(bad_stock) > 0 ){
    cli::cli_abort(c(
      "The following stock names in the excel file don't have corresponding `stock_long_name` matches in the Stock table of the FRAM database:",
      stats::setNames(bad_stock, rep("*", length(bad_stock)))
    ))
  }

  ## store values for later

  target_escapements_to_save <- fram_db |>
    fetch_table("BackwardsFRAM") |>
    dplyr::filter(.data$run_id == .env$run_id,
           .data$stock_id %in% c(127:134, 139:142)
    )

  fishery_save_lut <-
    tibble::tribble(~fishery_id, ~time_step,
            68, 4,
            68, 5,
            69, 5,
            65, 5,
            71, 4,
            71, 5,
            72, 5,
            70, 5,
            23, 4)

  ## double checking coverage of lut:
  #  left_join(dat |> select(fishery, fishery_id) |> distinct(), fishery_save_lut, by = "fishery_id")
  # anti_join(fishery_save_lut, dat |> select(fishery, fishery_id) |> distinct(), by = "fishery_id")
  # Note: buoy 10 (id 23) is not in the excel file, so should show up in our anti_join call

  fishery_scalers_to_save <- fram_db |>
    fetch_table("FisheryScalers", label = TRUE) |>
    dplyr::filter(.data$run_id %in% .env$run_id) |>
    dplyr::inner_join(fishery_save_lut, by = c("fishery_id", "time_step"))


  cli::cli_alert("Saving original escapements and fishery scalers to excel file...")


  wb <- openxlsx2::wb_load(excel_filepath)

  ## deal with storage sheets if they exist -- need to create new ones

  current_sheets <- openxlsx2::wb_get_sheet_names(wb)

  sheets_to_remove <- intersect(current_sheets,
                                c("Original Target Escapements", "Original Fishery Scalers", "BKFram Progress"))

  if("BKFram Progress" %in% sheets_to_remove){
    status_info <- openxlsx2::wb_to_df(wb,
                            sheet = "BKFram Progress", col_names = FALSE)
    if(!is.null(status_info) &&
       !grepl("^Process complete", status_info)[1]){
      cli::cli_abort(c("'BKFram Progress' sheet shows the last call of this function was incomplete! That may have left the wrong values in the FRAM database for this run!\n",
                       "To proceed, delete the 'BKFRAM Progress' sheet and re-run this function."))
    }
  }

  if(length(sheets_to_remove) > 0){
    for(cur_sheet in sheets_to_remove){
      wb <- wb |>
        openxlsx2::wb_remove_worksheet(cur_sheet)
    }
  }


  wb <- wb |>
    openxlsx2::wb_add_worksheet("Original Target Escapements") |>
    openxlsx2::wb_add_data("Original Target Escapements",
                target_escapements_to_save) |>
    openxlsx2::wb_add_worksheet("Original Fishery Scalers") |>
    openxlsx2::wb_add_data("Original Fishery Scalers",
                fishery_scalers_to_save) |>
    openxlsx2::wb_add_worksheet("BKFram Progress") |>
    openxlsx2::wb_add_data("BKFram Progress",
                data.frame(glue::glue("Storing initial data for run {run_id}")),
                col_names = FALSE)

  openxlsx2::wb_save(wb,
          excel_filepath, overwrite = TRUE)


  fishery_replacement_df <- fishery_save_lut |>
    dplyr::rename(match_FisheryID = "fishery_id",
           match_TimeStep = "time_step") |>
    dplyr::mutate(
      match_RunID = .env$run_id,
      replace_FisheryScaleFactor = 0,
      replace_Quota = 0,
      replace_MSFFisheryScaleFactor = 0,
      replace_MSFQuota = 0,
      ## if buoy 10, flag = 8, otherwise flag = 2
      replace_FisheryFlag = dplyr::if_else(.data$match_FisheryID == 23,
                                    8,
                                    2))
  target_escapement_replacement_df <- dat |>
    dplyr::filter(.data$fishery == "ETRS",
           .data$stock!= "Total Catch",
           .data$FLAG != 0) |>
    dplyr::arrange(.data$stock_id) |>
    dplyr::select(match_StockID = "stock_id",
           replace_TargetEscAge3 = "value") |>
    dplyr::mutate(match_RunID = .env$run_id)


  fishery_ids_coastal <- fishery_replacement_df$match_FisheryID |>
    setdiff(23) |>
    unique() |>
    sort()

  stocks_touched <- target_escapement_replacement_df$match_StockID |>
    unique() |>
    sort()


  cli::cli_alert("For fisheries {fishery_ids_coastal}, setting FisheryScalers catch values to 0 and flags to 2 for appropriate timesteps...")
  cli::cli_alert("For fishery 23, setting FisheryScalers catch values to 0 and flag to 8 for appropriate timesteps...")
  cli::cli_alert("For Stocks {stocks_touched}, changing target escapement to match `ETRS` of excel file...")


  if(nrow(fishery_replacement_df) != nrow(fishery_scalers_to_save)){
    cli::cli_abort("Must have the same number of replacement fishery scalers as fishery scalers to replace!")
  }

  if(nrow(target_escapement_replacement_df) != nrow(target_escapements_to_save)){
    cli::cli_abort("Must have the same number of replacement escapements as escapements to replace!")
  }

  fisheries_replaced <- modify_table(fram_db,
                                     table_name = "FisheryScalers",
                                     df = fishery_replacement_df)

  if(!all(fisheries_replaced$rows_affected == 1)){
    cli::cli_alert_warning("One or more fishery scalers for this run either replaced multiple rows in FRAM database or replaced none!")
  }

  target_escapements_replaced <- modify_table(fram_db,
                                              table_name = "BackwardsFRAM",
                                              df = target_escapement_replacement_df)

  res <- list(initial_fishery_replacement = fisheries_replaced,
              initial_target_escapement_replacement = target_escapements_replaced)

  if(!all(target_escapements_replaced$rows_affected == 1)){
    cli::cli_alert_warning("One or more target escapements for this run either replaced multiple rows in FRAM database or replaced none!")
  }

  ## updating status
  wb <- openxslx2::wb_load(excel_filepath)
  openxslx2::wb_add_data(wb,
              "BKFram Progress",
              data.frame(glue::glue("Fishery and escapment values changed for {run_id} in preparation for backwards run.")),
              col_names = FALSE)
  openxslx2::wb_save(wb,
          excel_filepath, overwrite = TRUE)

  cli::cli_h1("Running backwards run")

  stop_for_step("Reload FRAM database")
  stop_for_step("Run post-season BKrun.\nClick 'Save BKFRAM Targets and new recruit Scalars' and save as new model, suggested form of `bc-BKCoho20XX_A_2_a`.")
  stop_for_step("Review model run results. If you didn't have the option to save your run as a new run, do so now.")
  new_run_id <- setdiff(get_run_ids(fram_db), existing_runs)

  if(length(new_run_id) != 1){
    cli::cli_abort(c("Detecting {length(new_run_id)} new run IDs! There should be exactly 1 -- the new bkFRAM run.",
                     "Canceling remaining function. Original target escapement and fishery values can be found in the excel file."
    ))
  }

  stop_for_step("It appears that the new BkFRAM run had id {new_run_id}. Confirm this is true or cancel the function with [ESC].")



  cli::cli_h1("Returning original values to Fishery Scalers and Target Escapement to new run {new_run_id}")

  target_escapement_returnment_df <- target_escapements_to_save |>
    dplyr::select(match_StockID = "stock_id",
           replace_TargetEscAge3  = "target_esc_age3") |>
    dplyr::mutate(match_RunID = new_run_id)

  fishery_returnment_df <- fishery_scalers_to_save |>
    dplyr::select(match_FisheryID = "fishery_id",
           match_TimeStep = "time_step",
           replace_FisheryFlag = "fishery_flag",
           replace_FisheryScaleFactor = "fishery_scale_factor",
           replace_Quota = "quota",
           replace_MSFFisheryScaleFactor = "msf_fishery_scale_factor",
           replace_MSFQuota = "msf_quota",
    ) |>
    dplyr::mutate(match_RunID = "new_run_id")


  fisheries_replaced <- modify_table(fram_db,
                                     table_name = "FisheryScalers",
                                     df = fishery_returnment_df)

  if(!all(fisheries_replaced$rows_affected == 1)){
    cli::cli_alert_warning("One or more fishery scalers for this run either replaced multiple rows in FRAM database or replaced none!")
  }

  target_escapements_replaced <- modify_table(fram_db,
                                              table_name = "BackwardsFRAM",
                                              df = target_escapement_returnment_df)

  if(!all(target_escapements_replaced$rows_affected == 1)){
    cli::cli_alert_warning("One or more target escapements for this run either replaced multiple rows in FRAM database or replaced none!")
  }

  res$final_fishery_replacement = fisheries_replaced
  res$final_target_escapement_replacement = target_escapements_replaced

  wb <- openxlsx2::wb_load(excel_filepath)
  openxlsx2::wb_add_data(wb,
              "BKFram Progress",
              data.frame(glue::glue("Run values replaced in new run, id {new_run_id}.")),
              col_names = FALSE)
  openxlsx2::wb_save(wb,
          excel_filepath, overwrite = TRUE)

  cli::cli_h1("Forward Run")

  stop_for_step("Reload FRAM database")
  stop_for_step("Run forward using BKTAMM file, WITHOUT WA coastal iterations. Save as new model, suggested form of `bc-BKCoho20XX_A_2_b`")


  openxlsx2::wb <- wb_load(excel_filepath)
  openxlsx2::wb_add_data(wb,
              "BKFram Progress",
              data.frame(glue::glue("Process complete! bkrun = {run_id}, forward run = {new_run_id}")),
              col_names = FALSE)
  openxlsx2::wb_save(wb,
          excel_filepath, overwrite = TRUE)

  cli::cli_alert_success("Process complete! bkrun = {run_id}, forward run = {new_run_id}")

  ## return list of the modifications that were made to
  return(invisible(res))
}
