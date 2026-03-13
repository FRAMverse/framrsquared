#' Make plots to show the amount of landed catch_per_impact
#'
#' Identify how much reduction in landed catch at each fishery that would be needed
#' to reduce the impacts on a focal stock by 1 fish.
#'
#' @param fram_db fram database connection
#' @param run_id run_id of interest
#' @param stock_id stock_id of interest
#' @param filters_list list of framrsquared filter functions to apply before plotting. Defaults to `list(filter_wa, filter_sport)`, which filters to WA sport fisheries.
#' @param msp  Use Model Stock Proportion? Logical, defaults to TRUE.
#' @return ggplot object
#' @export
#'
#' @seealso [plot_stock_mortality()]
#'
#' @examples
#' \dontrun{
#' path = "FRAM compilations - readonly/2024-Pre-Season-Chinook-DB/2024 Pre-Season Chinook DB.mdb"
#' run_id = 132
#' stock_id = 3
#' plot_impacts_per_catch_heatmap(path,
#'                                run_id = 132,
#'                                stock_id = 5)
#' }
plot_impacts_per_catch_heatmap <- function(fram_db,
                                           run_id,
                                           stock_id,
                                           filters_list = list(filter_wa, filter_sport),
                                           msp = TRUE,
                                           digits_round = 1,
                                           outer_text_size = 18,
                                           cell_text_size = 5,
                                           short_title = FALSE,
                                           per_thousand_catch = FALSE) {
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)
  validate_stock_ids(fram_db, stock_id)
  validate_numeric(digits_round, n = 1)
  validate_numeric(outer_text_size, n = 1)
  validate_numeric(cell_text_size, n = 1)
  validate_flag(short_title)
  validate_flag(per_thousand_catch)
#
#   if(is.null(digits_round)){
#     if(per_thousand_catch){
#       digits_round = 5
#     } else {
#       digits_round = 1
#     }
#   }

  validate_stock_ids(fram_db, stock_id)
  if(length(stock_id)>1){
    cli::cli_abort("`stock_id` must be single stock.")
  }

  validate_flag(msp)

  run_info <- fetch_table_(fram_db, "RunID") |>
    dplyr::filter(run_id == .env$run_id)
  print(dim(run_info))
  cli::cli_alert(
    glue::glue(
      "Generating plot for run '{run_info$run_title}', a {fram_db$fram_db_species} FRAM run from {as.Date(run_info$run_time_date)}"
    )
  )

  stock_table <- fetch_table_(fram_db, "Stock")
  stock_name <- stock_table |>
    dplyr::filter(stock_id == .env$stock_id) |>
    dplyr::pull(.data$stock_name)
  stock_title <- stock_table |>
    dplyr::filter(stock_id == .env$stock_id) |>
    dplyr::pull(.data$stock_long_name)

  if (length(stock_name) == 0) {
    cli::cli_abort(
      glue::glue(
        "`stock_id` of {stock_id} not found in database. Available ids range from {min(stock_table$stock_id)} to {max(stock_table$stock_id)}"
      )
    )
  }


  fishery_landed <- fishery_mortality(fram_db, run_id = run_id) |>
    dplyr::group_by(.data$fishery_id, .data$time_step) |>
    dplyr::summarize(landed_catch = sum(.data$landed_catch))


  ## for chinook

  if (fram_db$fram_db_species == "CHINOOK") {
    stock_mort = aeq_mortality_(fram_db, run_id = run_id, msp = msp) |>
      dplyr::filter(stock_id == .env$stock_id) |>
      dplyr::mutate(total_mortality = .data$landed_catch + .data$shaker + .data$drop_off +
                      .data$msf_landed_catch + .data$msf_shaker + .data$msf_drop_off) |>
      dplyr::group_by(.data$fishery_id, .data$time_step) |>
      dplyr::summarize(mort = sum(.data$total_mortality)) |>
      dplyr::ungroup()
  } else{
    stock_mort = stock_mortality(fram_db, run_id = run_id) |>
      ## stock mortality combines msf and NS values.
      dplyr::mutate(total_mortality = .data$landed_catch + .data$shaker + .data$drop_off) |>
      dplyr::filter(stock_id == .env$stock_id) |>
      dplyr::group_by(.data$fishery_id, .data$time_step) |>
      dplyr::summarize(mort = sum(.data$total_mortality)) |>
      dplyr::ungroup()
  }

  time_step_lut <- fram_db |>
    fetch_table_(table_name = "TimeStep") |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select("time_step_id", "time_step_name") |>
    dplyr::rename(time_step = "time_step_id")

  dat_plot <- dplyr::full_join(fishery_landed, stock_mort, by = c("fishery_id", "time_step")) |>
    dplyr::mutate(catch_per_impact = .data$landed_catch / .data$mort) |>
    dplyr::mutate(catch_per_impact = dplyr::if_else(
      is.infinite(.data$catch_per_impact),
      NA,
      .data$catch_per_impact)) |>
    tibble::as_tibble() |>
    dplyr::left_join(time_step_lut, by = "time_step") |>
    dplyr::mutate(timestep_label = glue::glue("{time_step}\n({time_step_name})"))
  attr(dat_plot, "species") <- fram_db$fram_db_species


  if(!is.null(filters_list)){
    ## give species for filtering
    for(i in 1:length(filters_list)){
      dat_plot <- dat_plot |>
        filters_list[[i]]()
    }
  }
  dat_plot <- dat_plot |>
    framrosetta::label_fisheries() |>
    dplyr::mutate(fishery_label = glue::glue("{fishery_label} | (id={fishery_id})")) |>
    dplyr::filter(!is.na(.data$catch_per_impact)) |>
    dplyr::filter(.data$catch_per_impact != 0) |>
    tidyr::complete(.data$fishery_label, .data$timestep_label)

  if(per_thousand_catch){
    cli::cli_alert("Plotting in units of impacts per thousand catch.")

    dat_plot$catch_per_impact = 1/dat_plot$catch_per_impact * 1000

    subtitle = "Impacts per 1000 landed catch."
    fill_label = "Impacts / 1k\n"


    color_low = "aquamarine"
    color_high = "goldenrod1"
  } else {
    cli::cli_alert("Plotting in units of landed catch per impact.")

    subtitle = "Landed catch per impact."
    fill_label = "catch per\nimpact"

    color_low = "goldenrod1"
    color_high = "aquamarine"
  }
  #
  # dat_plot <- dat_plot |>
  #   dplyr::mutate(catch_per_impact = round(catch_per_impact, digits_round))

  if(short_title){
    plot_title = glue::glue("{stock_name} (stock_id = {stock_id})")
  } else {
    plot_title = glue::glue("{stock_title} (stock_id = {stock_id})")
  }

  ggplot2::ggplot(
    dat_plot,
    ggplot2::aes(
      x = .data$fishery_label,
      y = .data$timestep_label,
      fill = .data$catch_per_impact,
      label = dplyr::if_else(
        is.na(.data$catch_per_impact),
        "",
        format(round(.data$catch_per_impact, digits_round), big.mark = ",")
      )
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(size = cell_text_size) +
    ggplot2::scale_y_discrete(position = "right") +
    ggplot2::scale_fill_gradient(
      low = color_low,
      high = color_high,
      trans = "log",
      labels = function(x) {
        format(signif(x, 1), big.mark = ",")
      }
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      y = "Timestep",
      title = plot_title,
      subtitle = subtitle,
      fill = fill_label,
      x = ""
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = outer_text_size),
      panel.background = ggplot2::element_blank()
    )
}
