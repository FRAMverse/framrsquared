#' Make plots to show the amount of landed catch_per_impact
#'
#' Identify how much reduction in landed catch at each fishery that would be needed
#' to reduce the impacts on a focal stock by 1 fish.
#'
#' @param fram_db fram database connection
#' @param run_id run_id of interest
#' @param stock_id stock_id of interest.
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' path = "FRAM compilations - readonly/2024-Pre-Season-Chinook-DB/2024 Pre-Season Chinook DB.mdb"
#' run_id = 132
#' stock_id = 3
#' make_impacts_per_catch_heatmap(path,
#'                                run_id = 132,
#'                                stock_id = 5)
#' }

make_impacts_per_catch_heatmap <- function(fram_db, run_id, stock_id) {
  validate_fram_db(fram_db)
  validate_numeric(stock_id)
  validate_run_id(fram_db, run_id)

  run_info <- framrsquared::fetch_table(fram_db, "RunID") |>
    dplyr::filter(run_id == .env$run_id)
  print(dim(run_info))
  cli::cli_alert(
    glue::glue(
      "Generating plot for run '{run_info$run_title}', a {fram_db$fram_db_species} FRAM run from {as.Date(run_info$run_time_date)}"
    )
  )

  stock_table <- framrsquared::fetch_table(fram_db, "Stock")
  stock_name <- stock_table |>
    dplyr::filter(stock_id == .env$stock_id) |>
    dplyr::pull(.data$stock_name)

  if (length(stock_name) == 0) {
    cli::cli_abort(
      glue::glue(
        "`stock_id` of {stock_id} not found in database. Available ids range from {min(stock_table$stock_id)} to {max(stock_table$stock_id)}"
      )
    )
  }

  framrsquared::fetch_table(fram_db, "Mortality")

  fishery_landed <- framrsquared::fishery_mortality(fram_db, run_id = run_id) |>
    dplyr::group_by(.data$fishery_id, .data$time_step) |>
    dplyr::summarize(landed_catch = sum(.data$landed_catch))


  ## for chinook

  if (fram_db$fram_db_species == "CHINOOK") {
    stock_mort = framrsquared::aeq_mortality(fram_db, run_id = run_id) |>
      dplyr::filter(stock_id == .env$stock_id) |>
      dplyr::mutate(
        mort = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off +
          .data$msf_landed_catch + .data$msf_non_retention + .data$msf_shaker + .data$msf_drop_off
      ) |>
      dplyr::group_by(.data$fishery_id, .data$time_step) |>
      dplyr::summarize(mort = sum(.data$mort)) |>
      dplyr::ungroup()
  } else{
    stock_mort = framrsquared::stock_mortality(fram_db, run_id = run_id) |>
      dplyr::mutate(mort = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off) |>
      dplyr::filter(stock_id == .env$stock_id) |>
      dplyr::group_by(.data$fishery_id, .data$time_step) |>
      dplyr::summarize(mort = sum(.data$mort)) |>
      dplyr::ungroup()
  }

  dat_plot <- dplyr::full_join(fishery_landed, stock_mort, by = c("fishery_id", "time_step")) |>
    dplyr::mutate(catch_per_impact = .data$landed_catch / .data$mort) |>
    tibble::as_tibble()
  attr(dat_plot, "species") <- fram_db$fram_db_species


  if (fram_db$fram_db_species == "CHINOOK") {
    fishery_lut = framrosetta::fishery_chinook_fram
  } else{
    fishery_lut = framrosetta::fishery_coho_fram
  }

  dat_plot <- dplyr::left_join(dat_plot,
                              fishery_lut |>
                                dplyr::select("fishery_id", "fishery_title"),
                              by = "fishery_id")

  dat_plot <- dat_plot |>
    framrsquared::filter_wa() |>
    framrsquared::filter_sport() |>
    dplyr::filter(!is.na(.data$catch_per_impact)) |>
    dplyr::filter(.data$catch_per_impact != 0) |>
    tidyr::complete(.data$fishery_title, .data$time_step)

  ggplot2::ggplot(
    dat_plot,
    ggplot2::aes(
      x = .data$fishery_title,
      y = .data$time_step,
      fill = .data$catch_per_impact,
      label = dplyr::if_else(
        is.na(.data$catch_per_impact),
        "",
        format(round(.data$catch_per_impact, 1), big.mark = ",")
      )
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text() +
    ggplot2::scale_y_continuous(position = "right") +
    ggplot2::scale_fill_gradient(
      low = "goldenrod1",
      high = "aquamarine",
      trans = "log",
      labels = function(x) {
        format(signif(x, 1), big.mark = ",")
      }
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      y = "Timestep",
      title = glue::glue("{stock_name}"),
      subtitle = glue::glue("Landed catch per impact"),
      fill = "catch per\nimpact",
      x = ""
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 18),
      panel.background = ggplot2::element_blank()
    )
}
