#' Plot stock composition
#'
#' Produces a stock composition chart. Low frequency stocks are
#' grouped into geographic area.
#'
#' @param fram_db Fram database object
#' @param run_id numeric, RunID
#' @param fishery_id numeric, Fishery ID
#' @param time_step numeric, Time Step
#' @param group_threshold numeric, Stock percentages below this
#' number will be grouped. Default is 1%, setting to zero will turn grouping off
#'
#' @returns ggplot object
#'
#' @export
#'
#' @seealso [calculate_stock_comp()]
#'
#' @examples
#' \dontrun{
#' fram_db |> stock_comp(run_id = 132)
#' }

plot_stock_comp <- function(fram_db, run_id, fishery_id, time_step, group_threshold = .01) {
  # Get fishery name
  base_version_number <- fetch_table_(fram_db = fram_db,
                                      table_name = "RunID") |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull("base_period_id")

  fishery_version <- fetch_table_(fram_db = fram_db,
                                  table_name = "BaseID") |>
    dplyr::filter(.data$base_period_id == .env$base_version_number) |>
    dplyr::pull("fishery_version")

  fishery_name <- fetch_table_(fram_db = fram_db,
                               table_name = "Fishery") |>
    dplyr::filter(.data$version_number == .env$fishery_version,
                  .data$fishery_id == .env$fishery_id) |>
    dplyr::pull("fishery_title")



  # plot
  calculate_stock_comp(fram_db = fram_db,
                       run_id = run_id, fishery_id = fishery_id,
                       time_step = time_step,
                       group_threshold = group_threshold) |>
    ggplot2::ggplot(ggplot2::aes(.data$ts,
                                 stats::reorder(.data$stock_long_name, .data$total),
                                 fill = .data$mark)) +
    ggplot2::geom_col(alpha = .7) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::labs(
      x = NULL,
      y = 'Stock',
      subtitle = glue::glue("{fishery_name} {stringr::str_to_title(fram_db$fram_db_species)} Stock Composition Time-Step {time_step}")
    ) +
    ggplot2::theme(legend.title = ggplot2::element_blank())

}

#' Plot stock composition
#'
#' Produces a dataframe of stock composition for a given timestep and fishery. Low frequency stocks are
#' grouped into geographic area. For chinook, ages are combined.
#'
#' @param fram_db Fram database object
#' @param run_id numeric, RunID
#' @param fishery_id numeric, Fishery ID
#' @param time_step numeric, Time Step
#' @param group_threshold numeric, Stock percentages below this
#' number will be grouped. Default is 1%, setting to zero will turn grouping off
#'
#' @returns Tibble identify run, age, fishery, timestep, stock, and marks tatus. Provides calculated total mortality `$total_mort`, the proportion of all mortality in this fishery associated with that row (`ts`), and the sum `ts` for marked and unmarked fish of a given stock (`$total`) which can be used for sorting purposes.
#'
#' @export
#'
#' @seealso [plot_stock_comp()]
#'
#' @examples
#' \dontrun{
#' fram_db |> stock_comp(run_id = 132)
#' }
calculate_stock_comp <- function(fram_db, run_id, fishery_id, time_step, group_threshold = .01){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)
  validate_fishery_ids(fram_db, fishery_id, n = 1)
  validate_numeric(time_step)
  if(! time_step %in% 1:5){
    fram_abort("`time_step` must be a valid timestep (1-4 for Chinook, 1-5 for Coho)")
  }
  validate_numeric(group_threshold, 1)

  # pull data
  mort <- fram_db |>
    fetch_table_('Mortality') |> dplyr::filter(.data$run_id == .env$run_id,
                                               .data$fishery_id == .env$fishery_id,
                                               .data$time_step == .env$time_step)

  fishery_name <- fram_db |>
    fetch_table_('Fishery') |>
    dplyr::filter(.data$fishery_id == .env$fishery_id) |>
    dplyr::pull(.data$fishery_name)

  stock <- fram_db |> fetch_table_('Stock') |> dplyr::select("stock_id", "stock_long_name")


  # sum mortality
  mortality <- mort |>
    add_total_mortality() |>
    dplyr::select("run_id", "stock_id", "age", "fishery_id", "time_step", total_mort = "total_mortality") |>
    dplyr::inner_join(stock, by = 'stock_id')

  # break out into percentages
  mortality |>
    dplyr::mutate(
      ts = .data$total_mort / sum(.data$total_mort),
      mark = dplyr::if_else(.data$stock_id %% 2 == 0, 'Marked', 'Unmarked')
    ) |>
    dplyr::arrange(-.data$ts) |>
    dplyr::inner_join(coho_stock_comp_lut, by = 'stock_id') |>
    dplyr::group_by(.data$run_id, .data$fishery_id, .data$time_step,
                    .data$stock_long_name, .data$mark,
                    .data$stock_group) |>
    dplyr::summarize(
      dplyr::across(c("total_mort", "ts"), sum), .groups = 'drop'
    ) |>
    dplyr::group_by() |>
    dplyr::mutate(total = sum(.data$ts),
                  .by = c("run_id", "fishery_id", "time_step", "stock_long_name", "stock_group")) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      stock_long_name = dplyr::if_else(.data$total < .env$group_threshold, .data$stock_group, .data$stock_long_name)
    ) |>
    ## recalc with stock groups as needed.
    dplyr::summarize(dplyr::across(c("total_mort", "ts"), sum),
                     .by = c("run_id", "fishery_id", "time_step", "stock_long_name", "mark")) |>
    dplyr::mutate(total = sum(.data$ts),
                  .by = c("run_id", "fishery_id", "time_step", "stock_long_name"))
}
