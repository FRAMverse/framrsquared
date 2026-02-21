## find the stock names, used for validation purposes
present_stocks <- function(fram_db,
                           cur_run_id,
                           cur_stock_id,
                           stock_group_label,
                           verbose) {
  ## confirm stocks:
  ## ## base period number
  bp <- fram_db |>
    fetch_table_("RunID") |>
    dplyr::filter(.data$run_id == cur_run_id) |>
    dplyr::pull("base_period_id")

  ## stock version number
  stock_vers <- fram_db |>
    fetch_table_("BaseID") |>
    dplyr::filter(.data$base_period_id == bp) |>
    dplyr::pull("stock_version")

  ## stock names based on id
  stock_names <- fram_db |>
    fetch_table_("Stock") |>
    dplyr::filter(
      .data$stock_version == stock_vers,
      .data$stock_id %in% cur_stock_id
    ) |>
    dplyr::pull("stock_long_name")

  if (verbose) {
    cli::cli_alert("Currently working on '{stock_group_label}' stocks:")
    cli::cli_ul()
    for (s in stock_names) {
      cli::cli_li("{.field {s}}")
    }
    cli::cli_end()
  }
  return(invisible(stock_names))
}

## calculate the total er
calculate_soncc_er <- function(fram_db,
                               cur_run_id,
                               cur_stock_id) {
  terminal_fishery_ids <- c(1, 9)

  all_mort <- fram_db |>
    fetch_table_("Mortality") |>
    dplyr::filter(
      .data$run_id %in% cur_run_id,
      .data$stock_id %in% cur_stock_id
    ) |>
    add_total_mortality() |>
    dplyr::summarize(total_mortality = sum(.data$total_mortality)) |>
    dplyr::pull("total_mortality")

  non_terminal_mort <- fram_db |>
    fetch_table_("Mortality") |>
    dplyr::filter(
      .data$run_id %in% cur_run_id,
      .data$stock_id %in% cur_stock_id
    ) |>
    dplyr::filter(!.data$fishery_id %in% terminal_fishery_ids) |>
    add_total_mortality() |>
    dplyr::summarize(total_mortality = sum(.data$total_mortality)) |>
    dplyr::pull("total_mortality")

  esc <- fram_db |>
    fetch_table_("Escapement") |>
    dplyr::filter(
      .data$run_id %in% cur_run_id,
      .data$stock_id %in% cur_stock_id
    ) |>
    dplyr::pull("escapement") |>
    sum()

  er <- non_terminal_mort / (all_mort + esc)

  return(er)
}


##  calculate the er breakdowns
calculate_soncc_er_breakdown <- function(fram_db,
                                         cur_run_id,
                                         cur_stock_id) {
  terminal_fishery_ids <- c(1, 9)

  all_mort <- fram_db |>
    fetch_table_("Mortality") |>
    dplyr::filter(
      .data$run_id %in% cur_run_id,
      .data$stock_id %in% cur_stock_id
    ) |>
    add_total_mortality() |>
    dplyr::summarize(total_mortality = sum(.data$total_mortality)) |>
    dplyr::pull("total_mortality")

  mort_by_category <- fram_db |>
    fetch_table_("Mortality") |>
    dplyr::filter(
      .data$run_id %in% cur_run_id,
      .data$stock_id %in% cur_stock_id
    ) |>
    dplyr::full_join(fishery_coho_soncc,
      by = "fishery_id"
    ) |>
    dplyr::mutate(stt_label = paste(.data$region, "|", .data$gear, "|", .data$area)) |>
    dplyr::filter(!.data$fishery_id %in% terminal_fishery_ids) |>
    add_total_mortality() |>
    dplyr::mutate(total_mortality = dplyr::coalesce(.data$total_mortality, 0)) |>
    dplyr::group_by(.data$stt_label, .data$factor) |>
    dplyr::summarize(total_mortality = sum(.data$total_mortality)) |>
    dplyr::ungroup() |>
    dplyr::arrange("factor")

  esc <- fram_db |>
    fetch_table_("Escapement") |>
    dplyr::filter(
      .data$run_id %in% cur_run_id,
      .data$stock_id %in% cur_stock_id
    ) |>
    dplyr::pull("escapement") |>
    sum()

  er_breakdown <- mort_by_category |>
    dplyr::mutate(er = .data$total_mortality / (all_mort + esc))

  return(er_breakdown)
}

#' Calculate SONCC ERs
#'
#' Calculate mortality information for STT SONCC calculations. Does so for both the currently used unmarked hatchery stocks, and the analogous wild stocks (two separate sets of results). Note that the two terminal fisheries of Oregon and California (fishery_ids of 1 and 9) are excluded from the numerator of ERs calculated in this function.
#'
#' @param fram_db Fram database connection
#' @param run_id FRAM run id
#' @param verbose Include extra messages about stocks? Probably excessive, defaults to FALSE.
#'
#' @return List of lists: `$hatchery` and `$wild` respectively represent SONCC calculations for the hatchery stocks (193 and 197) that are currently used, and the analogous wild stocks (195 and 197). Each of those themselves is a list containing:
#' \describe{
#'   \item{`$total_er`}{Total ER for this stock group}
#'   \item{`$er_breakdown`}{ER for this stock group broken into the fishery categories used in the SONCC calculator}
#'   \item{`$stock_id`}{FRAM stock ids used for this stock group}
#'   \item{`$stock_name`}{FRAM stock names used for this stock group}
#' }
#'
#' @seealso [format_soncc_pasteable()], [create_soncc_pasteable()]
#' @export
#'
calculate_soncc <- function(fram_db,
                            run_id,
                            verbose = FALSE) {

  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)
  validate_flag(verbose)

  cur_run_title <- fram_db |>
    fetch_table("RunID") |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull("run_title")

  cli::cli_alert("Calculating SONCC numbers from {.field {cur_run_title}} run")

  ## hatchery stocks
  hatch_stock_id <- c(193, 197)
  hatch_stock_names <- present_stocks(fram_db,
    cur_run_id = run_id,
    cur_stock_id = hatch_stock_id,
    stock_group_label = "Hatchery",
    verbose = verbose
  )
  hatch_er <- calculate_soncc_er(fram_db,
    cur_run_id = run_id,
    cur_stock_id = hatch_stock_id
  )
  hatch_er_breakdown <- calculate_soncc_er_breakdown(fram_db,
    cur_run_id = run_id,
    cur_stock_id = hatch_stock_id
  )

  ## wild stocks
  wild_stock_id <- c(195, 199)
  wild_stock_names <- present_stocks(fram_db,
    cur_run_id = run_id,
    cur_stock_id = wild_stock_id,
    stock_group_label = "Wild",
    verbose = verbose
  )
  wild_er <- calculate_soncc_er(fram_db,
    cur_run_id = run_id,
    cur_stock_id = wild_stock_id
  )
  wild_er_breakdown <- calculate_soncc_er_breakdown(fram_db,
    cur_run_id = run_id,
    cur_stock_id = wild_stock_id
  )

  res <- list(
    hatchery =
      list(
        total_er = hatch_er,
        er_breakdown = hatch_er_breakdown,
        stock_id = hatch_stock_id,
        stock_name = hatch_stock_names
      ),
    wild = list(
      total_er = wild_er,
      er_breakdown = wild_er_breakdown,
      stock_id = wild_stock_id,
      stock_name = wild_stock_names
    )
  )

  return(res)
}



#' helper function to add dummy NA rows to a dataframe
#'
#' based on original row number. Used to make row spacing of dataframe
#' match row spacing of target excel file.
#'
#' @param df dataframe
#' @param after_rows vector of row numbers that need a space after them. Repeat a row number to add more than one space after it
#' @param dummy_row 1-row version of df with `NA` for all values.
#'
#' @return dataframe with additional NA rows
#'
add_dummy_rows <- function(df, after_rows, dummy_row) {
  after_rows <- sort(after_rows)
  df_temp <- df
  for (i in 1:length(after_rows)) {
    df_temp <- df_temp |>
      dplyr::add_row(dummy_row,
        .after = (i - 1 + after_rows[i])
      )
  }
  return(df_temp)
}


## takes an er breakdown dataframe and formats it for copy-pasting. Saves an
## excel file (argument `filename` should end in ".xlsx")
## right now we're using the hatchery version, so $hatchery$er_breakdown of
## the output of `calculate_soncc()`.
#' Save sonc ER breakdown in formatted excel workbook
#'
#' Takes the `$er_breakdown` output of [calculate_soncc()], formats into an appropriate shape (adding blank spaces, removing extraneous columns) and saves as an excel workbook. Formatting
#' is designed for seamless copy-pasting into the SONCC calculator workbook.
#'
#' @param soncc_er_breakdown `$er_breakdown` output of [calculate_soncc()]
#' @param filename Filename to save excel workbook to. Must end in `.xlsx`
#'
#' @return nothing
#' @export
#'
#' @seealso [calculate_soncc()], [create_soncc_pasteable()]
#'
format_soncc_pasteable <- function(soncc_er_breakdown,
                                  filename) {

  validate_data_frame(soncc_er_breakdown)
  validate_character(filename, n = 1)

  if (!grepl("\\.xlsx$", filename)) {
    cli::cli_abort("Argument {.arg filename} must end with {.val .xlsx}")
  }


  if (!rlang::is_installed("openxlsx2")) {
    cli::cli_abort("Package {.pkg openxlsx2} is required for this functionality.
                  Install it with {.code install.packages('openxlsx2')}.")
  }

  dummy_row <- data.frame(stt_label = NA, er = NA)
  soncc_er_breakdown <- soncc_er_breakdown |>
    dplyr::arrange(factor) |>
    dplyr::select(.data$stt_label, .data$er)

  res <- add_dummy_rows(soncc_er_breakdown,
    after_rows = c(3, 3, 6, 6, 6, 11, 16),
    dummy_row = dummy_row
  ) |>
    dplyr::rename(
      "fishery group" = "stt_label",
      "paste in" = "er"
    )

  class(res$`paste in`) <- c(class(res$`paste in`), "percentage")

  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("soncc to copy") |>
    openxlsx2::wb_add_data(
      sheet = "soncc to copy",
      x = res,
      na = ""
    ) |>
    openxlsx2::wb_add_fill(dims = "A1:B1", color = openxlsx2::wb_color("#90D5FF")) |>
    openxlsx2::wb_add_font(dims = "A1:B1", bold = TRUE, size = 11) |>
    openxlsx2::wb_set_col_widths(cols = 1:2, widths = c(50, 10))

  openxlsx2::wb_save(wb, file = filename)
}

#' Calculate SONCC ER breakdown for STT
#'
#' Creates a copy-pasteable excel file with the ER breakdown needed for the SONCC calculator.
#'
#' @param fram_db FRAM database connection
#' @param run_id Run id of run to calculate SONCC for
#' @param filename filename (including filepath) to save copy-paste ready SONCC breakdowns. Should end in `.xlsx`
#'
#' @return nothing
#' @export
#' @seealso [calculate_soncc()], [format_soncc_pasteable()]
#'
#' @examples
#' \dontrun{
#' library(here)
#' fram_db <- connect_fram_db(here("2026NOF_CohoFRAMdatabase_DRAFT.mdb"))
#' create_soncc_pasteable(fram_db, run_id = 152, filename = here("soncc_copy_ready.xlsx"))
#' }
create_soncc_pasteable <- function(fram_db, run_id, filename){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)
  validate_character(filename, n = 1)

  out <- calculate_soncc(fram_db, run_id)
  format_soncc_pasteable(out$hatchery$er_breakdown, filename)
}
