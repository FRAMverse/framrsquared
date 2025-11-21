#' Check that non-retention entries match flags
#'
#' Non-retention flags determine which values of the non-retention database are use. It is possible to accidentally forget to change flag or place values in the wrong locations.
#'
#'
#' This function checks for the following issues:
#' \itemize{
#' \item{Non-zero entries in cells that are IGNORED due to the flag. These seem like a big problem!}
#' \item{Zero entries in cells that are USED due to the flag. These may be correct, especially if the flag is 3 ("Legal/Sublegal Enc") as there might have been 0 sublegal or legal . Additionally, sometimes all entries are zero (e.g., presumably nonretention was intended to be zeroed out) but the flag was left as non-zero.}
#' }
#'
#'
#' @inheritParams fetch_table
#' @param run_id FRAM run id
#' @param wa_only Look only at WA non-retention? Defaults to FALSE.
#'
#' @return Invisibly returns a modified version of NonRetention table, with new columns
#' \describe{
#' \item{`$exist_ignored_vals`}{indicator for one or more non-zero values being ignored due to the flag}
#' \item{`$exist_unexpected_zeroes`}{indicator for one or more zero values being used due to flag}
#' \item{`$zeroes_exist_class`}{classification of entries with `$exist_unexpected_zeroes = TRUE`}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fram_db |> check_nonretention_flagging(48, wa_only = TRUE)
#' }
check_nonretention_flagging <- function(fram_db,
                                        run_id,
                                        wa_only = FALSE) {
  nr_df <- fram_db |>
    fetch_table("NonRetention") |>
    dplyr::filter(.data$run_id %in% run_id)

  if (wa_only) {
    nr_df <- nr_df |>
      filter_wa()
  }

  nr_df <- nr_df |>
    dplyr::mutate(exist_ignored_vals = dplyr::case_match(
      .data$non_retention_flag,
      1 ~ .data$cnr_input1 + .data$cnr_input2 > 0,
      2 ~ FALSE,
      3 ~ .data$cnr_input3 + .data$cnr_input4 > 0,
      4 ~ .data$cnr_input2 + .data$cnr_input3 + .data$cnr_input4 > 0
    )) |>
    dplyr::mutate(exist_unexpected_zeroes = dplyr::case_match(
      .data$non_retention_flag,
      1 ~ (.data$cnr_input1 == 0) | (.data$cnr_input2 == 0),
      2 ~ (.data$cnr_input1 == 0) | (.data$cnr_input2 == 0) |
        (.data$cnr_input3 == 0) | (.data$cnr_input4 == 0),
      3 ~ (.data$cnr_input1 == 0) | (.data$cnr_input2 == 0),
      4 ~ .data$cnr_input1 == 0,
    )) |>
    dplyr::mutate(zeroes_exist_class = dplyr::if_else(.data$exist_unexpected_zeroes,
      "Other warning!",
      NA_character_
    )) |>
    dplyr::mutate(zeroes_exist_class = dplyr::if_else(
      .data$exist_unexpected_zeroes & .data$non_retention_flag == 3,
      "flag 3: could be legit zeroes",
      .data$zeroes_exist_class
    )) |>
    dplyr::mutate(zeroes_exist_class = dplyr::if_else(
      .data$exist_unexpected_zeroes &
        (.data$cnr_input1 + .data$cnr_input2 + .data$cnr_input3 + .data$cnr_input4 == 0),
      "Zeroed but has flag",
      .data$zeroes_exist_class
    ))

  if (wa_only) {
    cli::cli_h1("Comparing flags to non-retention entries, WA ONLY")
  } else {
    cli::cli_h1("Comparing flags to non-retention entries, ALL FISHERIES")
  }
  cli::cli_text("{.emph For dataframe with all non-retention rows with potential problems, assign this function to an object}")
  cli::cli_div(theme = list(span.strong = list(color = "coral")))

  if (sum(nr_df$exist_ignored_vals) == 0) {
    cli::cli_alert_success("{.strong {sum(nr_df$exist_ignored_vals)}} non-zero entries are ignored due to flags")
  } else {
    cli::cli_alert_danger("{.strong {sum(nr_df$exist_ignored_vals)}} non-zero entries are ignored due to flags. These are probably a problem:")
    problem_fisheries <- nr_df |>
      dplyr::filter(.data$exist_ignored_vals) |>
      dplyr::pull(.data$fishery_id) |>
      unique()
    cli::cli_alert("Fisheries: {problem_fisheries}")
  }

  cli::cli_alert("{.strong {sum(nr_df$zeroes_exist_class == 'flag 3: could be legit zeroes',na.rm = T)}} entries with flag of 3 have a relevant non-retention zero value. \nThese might be legit.")
  cli::cli_alert("{.strong {sum(nr_df$zeroes_exist_class == 'Zeroed but has flag',na.rm = T)}} entries with non-zero flag but all zero non-retention values. \nThese are probably fine, but could reflect missing values.")

  if (sum(nr_df$zeroes_exist_class == "Other warning!", na.rm = T) == 0) {
    cli::cli_alert_success("{.strong {sum(nr_df$zeroes_exist_class == 'Other warning!', na.rm = T)}} other cases with zeroes for relevant non-retention values.")
  } else {
    cli::cli_alert_danger("{.strong {sum(nr_df$zeroes_exist_class == 'Other warning!', na.rm = T)}} other cases with zeroes for relevant non-retention values. These are probably a problem:")
    problem_fisheries <- nr_df |>
      dplyr::filter(.data$zeroes_exist_class == "Other warning!") |>
      dplyr::pull(.data$fishery_id) |>
      unique()
    cli::cli_alert("Fisheries: {problem_fisheries}")
  }

  return(invisible(nr_df |>
    dplyr::filter(.data$exist_ignored_vals | .data$exist_unexpected_zeroes)))
}
