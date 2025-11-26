#' Plot stock composition
#'
#' Produces a stock composition chart, low frequency stocks are
#' grouped into geographic area.
#' @export
#' @param fram_db Fram database object
#' @param run_id numeric, RunID
#' @param fishery_id numeric, Fishery ID
#' @param time_step numeric, Time Step
#' @param group_threshold numeric, Stock percentages below this
#' number will be grouped. Default is 1%, setting to zero will turn grouping off
#' @examples
#' \dontrun{
#' fram_db |> stock_comp(run_id = 132)
#' }

plot_stock_comp <- function(fram_db, run_id, fishery_id, time_step, group_threshold = .01) {

  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)
  validate_fishery_ids(fram_db, fishery_id)
  validate_numeric(time_step)
  if(! time_step %in% 1:5){
    cli::cli_abort("`time_step` must be a valid timestep (1-4 for Chinook, 1-5 for Coho)")
  }
  validate_numeric(group_threshold, 1)


  if(!rlang::is_installed("forcats")) {
    cli::cli_abort('Please install the {.pkg forcats} package to use this funciton.')
  }
  # pull data
  mort <- fram_db |>
    fetch_table('Mortality') |> dplyr::filter(.data$run_id == .env$run_id,
                                       .data$fishery_id == .env$fishery_id,
                                       .data$time_step == .env$time_step)

  fishery_name <- fram_db |>
    fetch_table('Fishery') |>
    dplyr::filter(.data$fishery_id == .env$fishery_id) |>
    dplyr::pull(.data$fishery_name)

  stock <- fram_db |> fetch_table('Stock') |> dplyr::select(.data$stock_id, .data$stock_long_name)


  # sum mortality
  mortality <- mort |>
    add_total_mortality() |>
    dplyr::select(.data$run_id, .data$stock_id, .data$age, .data$fishery_id, .data$time_step, total_mort = .data$total_mortality) |>
    dplyr::inner_join(stock, by = 'stock_id')

  # preak out into percentages
  morts <- mortality |>
    dplyr::mutate(
      ts = .data$total_mort / sum(.data$total_mort),
      mark = dplyr::if_else(.data$stock_id %% 2 == 0, 'Marked', 'Unmarked')
    ) |>
    dplyr::arrange(-.data$ts) |>
    dplyr::inner_join(coho_stock_comp_lut, by = 'stock_id') |> # coho_lut_stock_comp coming from package data
    dplyr::mutate(
      stock_long_name = dplyr::if_else(.data$ts < .env$group_threshold, .data$stock_group, .data$stock_long_name)
    ) |>
    dplyr::group_by(.data$run_id, .data$age, .data$fishery_id, .data$time_step, .data$stock_long_name, .data$mark) |>
    dplyr::summarize(
      dplyr::across(c(.data$total_mort, .data$ts), sum), .groups = 'drop'
    ) |>
    dplyr::group_by(.data$run_id, .data$age, .data$fishery_id, .data$time_step, .data$stock_long_name) |>
    dplyr::mutate(total = sum(.data$ts)) |>
    dplyr::ungroup()

  # plot
  morts |>
    ggplot2::ggplot(ggplot2::aes(.data$ts, forcats::fct_reorder(.data$stock_long_name, .data$total), fill = .data$mark)) +
    ggplot2::geom_col(alpha = .7) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::labs(
      x = NULL,
      y = 'Stock',
      subtitle = glue::glue("{fishery_name} {stringr::str_to_title(fram_db$fram_db_species)} Stock Composition Time-Step {time_step}")
    ) +
    ggplot2::theme(legend.title = ggplot2::element_blank())

}
