#' Calculate stock proportions
#'
#' @inheritParams
#' @param run_use
#' @param fishery_use
#'
#' @return
#' @export
#'
#' @examples
calc_stock_proportions <- function(fram_db, ## fram database connection
                                   run_use, ## run id number
                                   fishery_use ## fishery id number
){
  fram_db |>
    fetch_table("Mortality") |>
    dplyr::filter(run_id == .data$run_use,
                  fishery_id == .data$fishery_use)  |>
    dplyr::group_by(.data$time_step, .data$run_id) |>
    dplyr::mutate(prop.timestep = .data$encounter/.data$sum(encounter)) |>
    dplyr::ungroup()
}
