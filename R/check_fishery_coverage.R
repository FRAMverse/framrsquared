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
#' @param wa_only Check only Washington fisheries? Logical, defaults to `TRUE`
#' @param run_id FRAM run id
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

  if(wa_only){
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




#' Check each fishery x timestep for non-retention
#'
#' For one or more run ids, uses NonRetention table of FRAM database to identify which
#' fishery x timesteps have non-retention applied. Uses flagging rules to identify *actual*
#' non-retention used; to check that there are no mismatches between nonretention flags
#' and nonretention values, use [check_nonretention_flagging()]. `check_nonretention_coverage()` assumes
#' any non-modeled fishery has no non-retnention. Consequently, fishery x timesteps
#' that are not included in the base period will show up as `FALSE` for nonretention.
#'
#' @param fram_db fram database connection
#' @param run_id FRAM run id(s)
#'
#' @return dataframe with fishery, timestep, run, and `$has_nr`, which is TRUE if there
#' is non-zero non-retention modeled, FALSE otherwise.
#' @export
#'
#' @seealso [check_nonretention_flagging()], [plot_nonretention_coverage()]
#'
#' @examples
#' \dontrun{
#' fram_db |> check_nonretention_coverage(run_id = 47)
#' }
check_nonretention_coverage = function(fram_db, run_id){

  nr_df <- fram_db |>
    fetch_table("NonRetention") |>
    dplyr::filter(.data$run_id %in% run_id)

  nr_df <- nr_df |>
    dplyr::mutate(has_nr = dplyr::case_match(
      .data$non_retention_flag,
      0 ~ FALSE,
      1 ~ (.data$cnr_input1 != 0) | (.data$cnr_input2 != 0),
      2 ~ (.data$cnr_input1 != 0) | (.data$cnr_input2 != 0) |
        (.data$cnr_input3 != 0) | (.data$cnr_input4 != 0),
      3 ~ (.data$cnr_input1 != 0) | (.data$cnr_input2 != 0),
      4 ~ .data$cnr_input1 != 0
    ))

  all_fisheries <- fram_db |>
    fetch_table("Fishery")
  all_timesteps <- fram_db |>
    fetch_table("TimeStep")
  run_info <- fram_db |>
    fetch_table("RunID") |>
    dplyr::select("run_id", "run_title", "run_name")

  fishery_nr <- tidyr::expand_grid(fishery_id = all_fisheries$fishery_id,
                                   time_step = all_timesteps$time_step_id,
                                   run_id = run_id)
  res <- dplyr::left_join(fishery_nr, nr_df,
            by = c("fishery_id", "time_step", "run_id")) |>
    dplyr::mutate(non_retention_flag = dplyr::coalesce(.data$non_retention_flag, 0),
                  has_nr = dplyr::coalesce(.data$has_nr, FALSE)) |>
    dplyr::select("run_id", "fishery_id", "time_step", "non_retention_flag", "has_nr") |>
    dplyr::left_join(run_info, by = c("run_id"))
  attr(res, "species") <- fram_db$fram_db_species
  return(res)
}



#' Plot non-retention coverage
#'
#' Plots geom_tile of fisheries x timesteps, identifying when non-retention is being modeled.
#' See [check_nonretention_coverage()] for details. By default, plot shows only
#' Washington sport fisheries. This can be changed with optional arguments `wa_only` and `sport_only`.
#'
#' @param fram_db FRAM database
#' @param run_id One or more run ids. If multiple are provided, plot will use facet wrapping.
#' @param wa_only Plot only for Washington fisheries? Logical, defaults to `TRUE`
#' @param sport_only Plot only sport fisheries? Logical, defaults to `TRUE`
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' fram_db |> plot_nonretention_coverage(run_id = 47)
#' }
plot_nonretention_coverage <- function(fram_db, run_id, wa_only = TRUE, sport_only = TRUE){
  df <- check_nonretention_coverage(fram_db, run_id) |>
    framrosetta::label_fisheries()
  if(wa_only){
    df <- df |>
      filter_wa()
  }
  if(sport_only){
    df <- df |>
      filter_sport()
  }

  gp <- df |>
    ggplot2::ggplot(ggplot2::aes(x = .data$time_step, y = .data$fishery_label, fill = .data$has_nr))+
    ggplot2::geom_tile()+
    # scale_fill_discrete()
    ggplot2::theme_minimal(base_size = 13)+
    ggplot2::theme(legend.position = "top")+
    ggplot2::labs(x = "Timestep", y = "", fill = "Nonretention modeled")
  if(length(unique(df$run_id)) == 1){
    gp <- gp +
      ggplot2::labs(title = glue::glue("Run {df$run_id[1]}: {df$run_name[1]}"))
  } else {
    gp <- gp +
      ggplot2::facet_wrap(. ~ .data$run_name)
  }
  return(gp)
}
