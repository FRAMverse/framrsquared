#' Check the FisheryScalers and NonRetention have valid values
#'
#' Compares FisheryScalers and NonRetention entries against base period to ensure
#' there are no inputs that can't be represented in FRAM.
#'
#' @param fram_db FRAM database connection
#' @param run_id FRAM run id
#'
#' @returns Invisibly returns a list of tables identifying the fishery x timesteps that are not in base period
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fram_db |> check_fishery_coverage(run_id = 156)
#' }
check_bp_coverage <- function(fram_db, run_id){

  base_period_id <- fram_db |>
    fetch_table("RunID") |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(base_period_id)

  in_bp_er <- fram_db |>
    fetch_table_("BaseExploitationRate") |>
    dplyr::filter(.data$exploitation_rate > 0) |>
    dplyr::filter(.data$base_period_id == .env$base_period_id) |>
    dplyr::select("fishery_id", "time_step") |>
    dplyr::distinct() |>
    dplyr::mutate(has_bp_er = TRUE)


  in_fishery_scalers <- fram_db |>
    fetch_table_("FisheryScalers") |>
    dplyr::filter(.data$run_id %in% .env$run_id) |>
    na_scalers_from_flag() |>
    dplyr::mutate(dplyr::across(.data$fishery_scale_factor:.data$msf_quota, ~ dplyr::coalesce(.x, 0))) |>
    dplyr::filter(dplyr::if_any(.data$fishery_scale_factor:.data$msf_quota, ~ .x != 0)) |>
    dplyr::select("fishery_id", "time_step")


  no_bp_er <- in_fishery_scalers |>
    dplyr::left_join(in_bp_er, by = c("fishery_id", "time_step")) |>
    dplyr::filter(is.na(.data$has_bp_er))|>
    dplyr::mutate(has_bp_er = dplyr::coalesce(.data$has_bp_er, FALSE))

  if(nrow(no_bp_er)>0){
    cli::cli_alert("Issues detected! The following are represented in 'FisheryScalers' but not in the base period:")
    problem_children <- no_bp_er |>
      dplyr::summarize(ts_collapse = paste0(.data$time_step, collapse = ", "),
                .by = .data$fishery_id) |>
      dplyr::mutate(msg = glue::glue("  fishery_id {fishery_id}, time_step(s) {ts_collapse}")) |>
      dplyr::pull(.data$msg)
    purrr::walk(problem_children, cli::cli_alert_danger)
  } else {
    cli::cli_alert_success("All modeled fisheries are represented in bp!")
  }

  in_cnr <- fram_db |>
    fetch_table_("NonRetention") |>
    dplyr::filter(.data$run_id %in% .env$run_id) |>
    na_non_retention_from_flag() |>
    dplyr::mutate(dplyr::across(.data$cnr_input1:.data$cnr_input4, ~ dplyr::coalesce(.x, 0))) |>
    dplyr::filter(dplyr::if_any(.data$cnr_input1:.data$cnr_input4, ~ .x != 0)) |>
    dplyr::filter(.data$non_retention_flag != 0) |>
    dplyr::select("fishery_id", "time_step")

  no_bp_cnr <- in_cnr |>
    dplyr::left_join(in_bp_er, by = c("fishery_id", "time_step")) |>
    dplyr::filter(is.na(.data$has_bp_er)) |>
    dplyr::mutate(has_bp_er = dplyr::coalesce(.data$has_bp_er, FALSE))

  if(nrow(no_bp_cnr)>0){
    cli::cli_alert("Issues detected! The following are represented in 'NonRetention' but not in the base period:")
    problem_children <- no_bp_cnr |>
      dplyr::summarize(ts_collapse = paste0(.data$time_step, collapse = ", "),
                .by = .data$fishery_id) |>
      dplyr::mutate(msg = glue::glue("  fishery_id {fishery_id}, time_step(s) {ts_collapse}")) |>
      dplyr::pull(.data$msg)
    purrr::walk(problem_children, cli::cli_alert_danger)
  } else {
    cli::cli_alert_success("All modeled CNR is represented in bp!")
  }

  return(invisible(list(scalers_problem = no_bp_er,
                        cnr_problems = no_bp_cnr)))
}

