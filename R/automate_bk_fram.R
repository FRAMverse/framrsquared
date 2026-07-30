#################################
## BkFRAM Coastal back-and-forth helper
#################################
##
## This code is designed to help the Coho bkfram process in which we need to
##  (a) store some coastal target escapement and fishery values (also buoy 10. Does that count as coastal?)
##  (b) set target escapement values to match an external source
##  (c) set specific fishery values to 0
##  (d) Run fram backwards
##  (e) return the fishery and target escapement values to what they were
##  (f) run FRAM forwards
##
##  The code does the FRAM DB changing, and prompts users for the steps in which they need to run FRAM.
##
##  The codea relies on an excel file that contains the escapement information in a specific structure.
##  The code will add sheets to the file to (a) store the original fishery and target escapement numbers
##  that get replaced and then returned. The code also adds a sheet to track progress, in case it aborts partway through.
##
##  Steph Thurner and Collin both have copies of the excel template file in question, and make_bk_helper_template_excel() creates
##  an empty version of the file with the minimum necessary tabs.
##
##  The only function users should need is `bkfram_coastal_helper()`, which takes as arguments:
##  - `excel_path` => filepath for the excel file with escapement information. Character atomic
##  - `fram_path` => filepath for the fram database. Character atomic.
##  - `run_id` => the run id of the backwards run in question. Numeric.
##
##  Example code:
##
## bkfram_coastal_helper(excel_path = "C:/Users/person/Documents/bkfram automation/TemplateBKFRAMAutomationDraft2.xlsx",
##                       fram_path = ""C:/Users/person/Documents/bkfram automation/PSC_CoTC_PostSeason_CohoFRAMDB_2010thru2024.mdb",
##                       run_id = 70)
##
##
##  The function invisibly returns a list of the changes made to the database, so you can review/debug with
##
##  output <- bkfram_coastal_helper(...
##
##  and then look at `output` afterwards
##
## ---------------------------------------------------



## this function makes an empty version of the excel template file. It should *not* be needed unless
## we lose track of the excel file Steph and Collin have, but it's good to be able to recreate
make_bk_helper_template_excel <- function(
    file, # filepath for new file, including filetype .xlsx.
    overwrite = TRUE
) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop(
      "The openxlsx2 package is required. ",
      "Install it with install.packages('openxlsx2').",
      call. = FALSE
    )
  }

  sheet <- "Terminal ETRS"

  # Colors resolved from the theme colors and tints used by
  # the original workbook.
  gray_fill   <- "#D1D1D1"
  green_fill  <- "#A3C4A7"
  orange_fill <- "#F2AA84"

  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet(
      sheet = sheet,
      grid_lines = TRUE,
      zoom = 85
    )

  # Set the workbook's default font to match the original.
  wb <- openxlsx2::wb_set_base_font(
    wb,
    font_name = "Aptos Narrow",
    font_size = 11
  )

  # ------------------------------------------------------------------
  # Cell contents
  # ------------------------------------------------------------------

  # Column headings across row 1.
  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = c(
      "Escapement",
      "FLAG",
      "Queets R Net",
      "Queets R Sport",
      "Queets R C&S ",
      "Quillayute R Net",
      "Quillayute R Sport",
      "Quillayute R C&S",
      "ETRS"
    ),
    dims = "B1:J1",
    col_names = FALSE
  )

  # Stock labels in column A.
  stock_labels <- c(
    "Queets River Fall Natural UnMarked",
    "Queets River Fall Hatchery UnMarked",
    "Queets River Fall Hatchery Marked",
    "Quillayute River Summer Natural UnMarked",
    "Quillayute River Summer Hatchery UnMarked",
    "Quillayute River Summer Hatchery Marked",
    "Quillayute River Fall Natural UnMarked",
    "Quillayute River Fall Hatchery UnMarked",
    "Quillayute River Fall Hatchery Marked"
  )

  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = stock_labels,
    dims = "A2:A10",
    col_names = FALSE
  )

  # Additional labels.
  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = "Total Catch",
    dims = "A11",
    col_names = FALSE
  )

  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = "Green = Forward Run",
    dims = "A16",
    col_names = FALSE
  )

  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = "Orange = Backward Run",
    dims = "A17",
    col_names = FALSE
  )

  # ------------------------------------------------------------------
  # Cell fills
  # ------------------------------------------------------------------

  # Green forward-run input cells.
  green_ranges <- c(
    "B2:B10"
  )

  for (cell_range in green_ranges) {
    wb <- openxlsx2::wb_add_fill(
      wb,
      sheet = sheet,
      dims = cell_range,
      color = openxlsx2::wb_color(hex = green_fill),
      pattern = "solid"
    )
  }

  # Orange backward-run cells.
  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet,
    dims = "J2:J10",
    color = openxlsx2::wb_color(hex = orange_fill),
    pattern = "solid"
  )

  # Gray cells in the total-catch row.
  gray_ranges <- c(
    "A11:C11",
    "J11"
  )

  for (cell_range in gray_ranges) {
    wb <- openxlsx2::wb_add_fill(
      wb,
      sheet = sheet,
      dims = cell_range,
      color = openxlsx2::wb_color(hex = gray_fill),
      pattern = "solid"
    )
  }

  # ------------------------------------------------------------------
  # Column dimensions
  # ------------------------------------------------------------------

  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = 1,
    widths = 33
  )

  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = 2,
    widths = 10.90625
  )

  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = 3,
    widths = 10.90625
  )

  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = 4:9,
    widths = 18.7265625
  )

  # Column J uses Excel's standard default width, 8.43.
  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = 10,
    widths = 8.43
  )

  # ------------------------------------------------------------------
  # Page margins
  # ------------------------------------------------------------------

  wb <- openxlsx2::wb_set_page_setup(
    wb,
    sheet = sheet,
    left = 0.7,
    right = 0.7,
    top = 0.75,
    bottom = 0.75,
    header = 0.3,
    footer = 0.3
  )

  # Save and return the workbook invisibly so it can also be modified
  # further by the calling code.
  openxlsx2::wb_save(
    wb,
    file = file,
    overwrite = overwrite
  )

  invisible(wb)
}

## helper function: pause and wait for user input. Users should not need to call this itself.
stop_for_step <- function(msg, doneword = "done", .envir = parent.frame()){

  yes_flag = FALSE
  has_run = FALSE
  while(!yes_flag){
    if(has_run){
      cli::cli_alert_info("(that was not `{doneword}`)")
    }

    purrr::walk(msg, cli::cli_alert, .envir = .envir)

    cli::cat_line()

    cli::cli_alert("Type {.val {doneword}} when done")
    inp <- readline("")

    if(inp %in% c(doneword)){
      yes_flag = TRUE
    }
    has_run = TRUE
  }
}


## helper function: pause and wait for user input. Users should not need to call this itself.
confirm_continue <- function(msg, .envir = parent.frame()){

  yes_flag = FALSE
  has_run = FALSE
  while(!yes_flag){
    if(has_run){
      cli::cli_alert_danger("(that was not `yes` or [ESC])")
    }

    purrr::walk(msg, cli::cli_alert_warning, .envir = .envir)
    cli::cat_line()

    cli::cli_alert("Type {.val yes} to continue or hit [ESC] to cancel.")
    inp <- readline("")

    if(inp %in% c("yes", "Yes", "YES")){
      yes_flag = TRUE
    }
    has_run = TRUE
  }
}

## helper function: pause and wait for user input. Users should not need to call this itself.
get_user_run_id <- function(fram_db){
  valid_input = FALSE
  while(!valid_input){
    cli::cat_line()
    cli::cli_alert("Manually provide new run id number in the console (or hit [Esc] to cancel function):")
    inp <- readline("")


    ## check if it can conform to an integer
    inp_converted <- suppressWarnings(as.numeric(inp))
    if(is.na(inp_converted)){
      cli::cli_alert_warning("Input must be a valid integer")
    } else {
      if(!(inp_converted %in% get_run_ids(fram_db))){
        cli::cli_alert_warning("{inp_converted} is not a run_id in this database!")
        cli::cli_alert_info("Available runs: {get_run_ids(fram_db)}")
      } else {
        valid_input = TRUE
      }
    }

  }
  return(inp_converted)
}

## Main function
bkfram_coastal_helper <- function(excel_path,
                                  fram_path,
                                  run_id){

  rlang::check_installed("readxl")
  rlang::check_installed("openxlsx2")



  cli::cli_h1("Preparing backwards run")

  raw <- readxl::read_excel(excel_path,
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
    tidyr::pivot_longer(cols = -c("stock", "FLAG"),
                        names_to = "fishery") |>
    dplyr::mutate(FLAG = dplyr::if_else(.data$fishery %in% c("Escapement", "ETRS"),
                                        .data$FLAG,
                                        NA_integer_))


  ## map stocks + fisheries to ids

  fram_db <- connect_fram_db(fram_path, quiet = TRUE)

  existing_runs = get_run_ids(fram_db)

  stock_lut <- fetch_table(fram_db, "Stock") |>
    dplyr::select(stock = "stock_long_name", "stock_id")

  fishery_lut <- fetch_table(fram_db, "Fishery") |>
    dplyr::select(fishery = "fishery_title", "fishery_id")

  dat <- dplyr::left_join(dat, stock_lut, by = "stock") |>
    dplyr::left_join(fishery_lut, by = "fishery")

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
  # Results look right
  # Note: buoy 10 (id 23) is not in the excel file, so should show up in our anti_join call

  fishery_scalers_to_save <- fram_db |>
    fetch_table("FisheryScalers", label = TRUE) |>
    dplyr::filter(.data$run_id %in% .env$run_id) |>
    dplyr::inner_join(fishery_save_lut, by = c("fishery_id", "time_step"))


  cli::cli_alert("Saving original escapements and fishery scalers to excel file...")


  wb <- openxlsx2::wb_load(excel_path)

  ## deal with storage sheets if they exist -- need to create new ones

  current_sheets <- openxlsx2::wb_get_sheet_names(wb)

  sheets_to_remove <- intersect(current_sheets,
                                c("Original Target Escapements", "Original Fishery Scalers", "BKFram Progress"))

  if("BKFram Progress" %in% sheets_to_remove){
    status_info <- openxlsx2::wb_to_df(wb,
                                       sheet = "BKFram Progress", col_names = FALSE)
    if(!is.null(status_info) &&
       !grepl("^Process complete", status_info)[1]){

      cli::cli_div(theme = list(span.emph = list(color = "blue")))
      status_info[1]
      confirm_continue(c("It appears that the last use of this excel template file terminated before completing.",
                         "Last status logged: {.emph {status_info[1]}}",
                         "If a previous attempt changed the fishery scalers and escapement targets of this run, continuing may treat the temporary scalers and escapements as if they were original values.",
                         "Continue despite this?\n"

      ))
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
                     excel_path, overwrite = TRUE)


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
  wb <- openxlsx2::wb_load(excel_path)
  openxlsx2::wb_add_data(wb,
                         "BKFram Progress",
                         data.frame(glue::glue("Fishery and escapment values changed for {run_id} in preparation for backwards run.")),
                         col_names = FALSE)
  openxlsx2::wb_save(wb,
                     excel_path, overwrite = TRUE)

  cli::cli_h1("Running backwards run")

  stop_for_step("Reload FRAM database, selecting run {run_id}")
  stop_for_step("Run post-season BKrun.\nClick 'Save BKFRAM Targets and new recruit Scalars' and save as new model, suggested form of `bc-BKCoho20XX_A_2_a`.")
  stop_for_step("Review model run results. If you didn't have the option to save your run as a new run, do so now.")
  updated_existing_runs = get_run_ids(fram_db)
  new_run_id <- setdiff(updated_existing_runs, existing_runs)


  ## update the following section with warning / not warning and option to input run id.
  if(length(new_run_id) != 1){
    cli::cli_alert_warning("Attempting to automatically detect the new BkFRAM run id has misbehaved. Detected {length(new_run_id)} new run IDs: {new_run_id}.")
    new_run_id <- get_user_run_id(fram_db)

  } else{
    ## could automatically read it
    {
      cli::cli_alert_success("It appears that the new BkFRAM run had id {new_run_id}. Use this (\"yes\") or manually provide run id (\"no\")?")

      valid_input = FALSE
      has_run = FALSE
      while(!valid_input){
        if(has_run){
          cli::cli_alert_info("(that was not \"Yes\" or \"No\")")
        }

        cli::cat_line()

        cli::cli_alert("Type \"Yes\" (use current run id) or \"No\" (manually provide run id)")
        inp <- readline("")

        if(inp %in% c("Yes", "yes", "YES", "NO", "no", "No")){
          valid_input = TRUE
        }
        has_run = TRUE
      }

      if(inp %in% c("no", "No", "NO")){
        new_run_id <- get_user_run_id(fram_db)
      }
    }

  }

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
    dplyr::mutate(match_RunID = new_run_id)


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

  wb <- openxlsx2::wb_load(excel_path)
  openxlsx2::wb_add_data(wb,
                         "BKFram Progress",
                         data.frame(glue::glue("Run values replaced in new run, id {new_run_id}.")),
                         col_names = FALSE)
  openxlsx2::wb_save(wb,
                     excel_path, overwrite = TRUE)

  cli::cli_h1("Forward Run")

  stop_for_step("Reload FRAM database, selecting run {new_run_id}")
  stop_for_step("Run forward using BKTAMM file, WITHOUT WA coastal iterations. Save as new model, suggested form of `bc-BKCoho20XX_A_2_b`")


  wb <- openxlsx2::wb_load(excel_path)
  openxlsx2::wb_add_data(wb,
                         "BKFram Progress",
                         data.frame(glue::glue("Process complete! bkrun = {run_id}, forward run = {new_run_id}")),
                         col_names = FALSE)
  openxlsx2::wb_save(wb,
                     excel_path, overwrite = TRUE)

  cli::cli_alert_success("Process complete! Backwards run id = {run_id}, forward run id = {new_run_id}")

  ## return list of the modifications that were made to
  return(invisible(res))
}

##
make_bk_helper_template_excel("C:/Repos/test_template.xlsx")
