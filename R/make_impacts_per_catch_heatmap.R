#' Make plots to show the amount of landed catch_per_impact
#'
#' Identify how much reduction in landed catch at each fishery that would be needed
#' to reduce the impacts on a focal stock by 1 fish. Does *not* include CNR.
#'
#' @param fram_db fram database connection
#' @param run_id run_id of interest
#' @param stock_id stock_id of interest. Can accept multiple stock_ids, and will plot the impact on the combined stocks.
#' @param filters_list list of framrsquared filter functions to apply before plotting. Defaults to `list(filter_wa, filter_sport)`, which filters to WA sport fisheries.
#' @param msp  Use Model Stock Proportion? Logical, defaults to TRUE. Only relevant for Chinook databases.
#' @param digits_round How many digits should cell values be rounded to? Numeric, defaults to 1.
#' @param outer_text_size Controls size of plot text elements except cell text. Different plot elements scale relative to this value. Numeric defaults to 18.
#' @param cell_text_size Controls size of text size in heatmap cells.  Numeric, defaults to 5. Different units from `outer_text_size`.
#' @param short_title Should the abbreviated stock name (e.g., "M-ssdnph") be used (`TRUE`) or the longer name (e.g., "South Puget SOund Net Pens Marked"). Logical, defaults to `FALSE`; `TRUE` may be useful when plots must be small.
#' @param per_thousand_catch Should plot be presented in units of Impacts per Thousand Landed Catch (TRUE) or landed catch per impact (FALSE). Logical, defaults to FALSE.
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
    cli::cli_alert_warning("Multiple stock IDs provided! Interpret combined impacts with caution!")
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
    dplyr::filter(stock_id %in% .env$stock_id) |>
    dplyr::pull(.data$stock_name)
  stock_title <- stock_table |>
    dplyr::filter(stock_id %in% .env$stock_id) |>
    dplyr::pull(.data$stock_long_name)

  if (length(stock_name) == 0) {
    cli::cli_abort(
      glue::glue(
        "`stock_id` of {stock_id} not found in database. Available ids range from {min(stock_table$stock_id)} to {max(stock_table$stock_id)}"
      )
    )
  }

  if (fram_db$fram_db_species == "CHINOOK") {
    stock_mort = aeq_mortality_(fram_db, run_id = run_id, msp = msp) |>
      dplyr::filter(stock_id %in% .env$stock_id) |>
      add_total_mortality() |>
      dplyr::group_by(.data$fishery_id, .data$time_step) |>
      dplyr::summarize(mort = sum(.data$total_mortality)) |>
      dplyr::ungroup()
  } else{
    stock_mort = fram_db |>
      fetch_table_("Mortality") |>
      dplyr::filter(.data$run_id == .env$run_id,
                    .data$stock_id %in% .env$stock_id) |>
      ## stock mortality combines msf and NS values.
      dplyr::group_by(.data$run_id, .data$time_step, .data$fishery_id) |>
      dplyr::summarize(
        dplyr::across(c(.data$landed_catch:.data$drop_off,
                        .data$msf_landed_catch:.data$msf_drop_off), \(x) sum(x)),
        .groups='drop') |>
      dplyr::mutate(total_mortality =
                      .data$landed_catch +
                      .data$shaker +
                      .data$drop_off +
                      .data$msf_landed_catch +
                      .data$msf_non_retention +
                      .data$msf_shaker +
                      .data$msf_drop_off
      ) |>
      dplyr::group_by(.data$fishery_id, .data$time_step) |>
      dplyr::summarize(mort = sum(.data$total_mortality)) |>
      dplyr::ungroup()
  }
  attr(stock_mort, "species") <- fram_db$fram_db_species

  if(!is.null(filters_list)){
    ## give species for filtering
    for(i in 1:length(filters_list)){
      stock_mort <- stock_mort |>
        filters_list[[i]]()
    }
  }



  fishery_landed <- fishery_mortality(fram_db, run_id = run_id, fishery_id = unique(stock_mort$fishery_id)) |>
    dplyr::group_by(.data$fishery_id, .data$time_step) |>
    dplyr::summarize(landed_catch = sum(.data$landed_catch))


  ## for chinook



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



  dat_plot <- dat_plot |>
    framrosetta::label_fisheries() |>
    dplyr::mutate(fishery_label = glue::glue("{fishery_label} | (id={fishery_id})")) |>
    dplyr::filter(!is.na(.data$catch_per_impact)) |>
    dplyr::filter(.data$catch_per_impact != 0) |>
    tidyr::complete(.data$fishery_label, .data$timestep_label)

  fishery_label_sorted <- dat_plot |>
    dplyr::arrange(.data$fishery_id) |>
    dplyr::pull(.data$fishery_label) |>
    unique() |>
    rev()

  dat_plot <-  dat_plot |>
    dplyr::mutate(fishery_label = factor(.data$fishery_label, levels = fishery_label_sorted))

  if(per_thousand_catch){
    cli::cli_alert("Plotting in units of impacts per thousand catch.")

    dat_plot$catch_per_impact = 1/dat_plot$catch_per_impact * 1000

    subtitle = "Impacts per 1000 landed catch."
    fill_label = "Impacts / 1k\n"
    legend_scale_labels = c("Lowest\nImpact", "Highest\nImpact")


    color_low = "aquamarine"
    color_high = "goldenrod1"


  } else {
    cli::cli_alert("Plotting in units of landed catch per impact.")

    subtitle = "Landed catch per impact."
    fill_label = "catch per\nimpact"
    legend_scale_labels = c("Highest\nImpact", "Lowest\nImpact")

    color_low = "goldenrod1"
    color_high = "aquamarine"
  }
  #
  # dat_plot <- dat_plot |>
  #   dplyr::mutate(catch_per_impact = round(catch_per_impact, digits_round))


  if(length(stock_id)>1)
  {
    plot_title = glue::glue("Combined Stocks {glue::glue_collapse(stock_id, ', ')}: {glue::glue_collapse(stock_name, ', ')}")
  } else {
    if(short_title){
      plot_title = glue::glue("{stock_name} (stock_id = {stock_id})")
    } else {
      plot_title = glue::glue("{stock_title} (stock_id = {stock_id})")
    }
  }

  ## placement of qualitative legend scale labels
  scale_range = range(dat_plot$catch_per_impact, na.rm = T)
  ## positioning equally on log scale
  scale_range = exp(log(scale_range) + c(1, -1) * 0.1 * diff(log(scale_range)))




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
      breaks = scale_range,
      # labels = legend_scale_labels
      labels = c("Lowest", "Highest")
      # labels = function(x) {
      #   format(signif(x, 1), big.mark = ",")
      # }
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
