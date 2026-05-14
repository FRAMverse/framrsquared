#' Returns a tibble matching the Fishery Mortality screen.
#'
#' @param fram_db FRAM database object
#' @param run_id atomic or vector of run_ids to filter to. Can improve speed. Optional, defaults to
#'   `NULL`.
#' @param fishery_id atomic or vector of fishery_id to filter to. Can improve speed. Optional,
#'   defaults to `NULL`.
#' @param msp Use Model Stock Proportion? Logical, defaults to TRUE. Only relevant for Chinook.
#'
#' @export
#'
#' @returns Tibble identifying, run, fishery, age, timestep, and providing calculations of
#'   mortalities by type, aggregating across NS and MSF.
#'
#' @examples
#' \dontrun{
#' fram_db |> fishery_mortality(run_id = 101)
#' }
fishery_mortality <- function(fram_db, run_id = NULL, fishery_id = NULL, msp = TRUE) {
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id, allow_null = TRUE)
  validate_fishery_ids(fram_db, fishery_id, allow_null = TRUE)
  validate_flag(msp)

  fishery_mort <- fram_db |>
    fetch_table_("Mortality")

  if(!is.null(run_id)){
    fishery_mort <- fishery_mort |>
      dplyr::filter(.data$run_id %in% .env$run_id)
  }
  if(!is.null(fishery_id)){
    fishery_mort <- fishery_mort |>
      dplyr::filter(.data$fishery_id %in% .env$fishery_id)
  }

  fishery_mort <- fishery_mort |>
    dplyr::group_by(
      .data$run_id,
      .data$age,
      .data$fishery_id,
      .data$time_step
    ) |>
    dplyr::summarize(
      dplyr::across(
        c(
          "landed_catch":"drop_off",
          "msf_landed_catch":"msf_drop_off"
        ),
        \(x) sum(x)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer("landed_catch":"msf_drop_off") |>
    dplyr::mutate(name = stringr::str_remove(.data$name, "msf_")) |>
    dplyr::group_by(
      .data$run_id,
      .data$fishery_id,
      .data$age,
      .data$time_step,
      .data$name
    ) |>
    dplyr::summarise(value = sum(.data$value), .groups = "drop") |>
    tidyr::pivot_wider() |>
    dplyr::select(
      "run_id":"time_step", "landed_catch",
      "non_retention", "shaker", "drop_off"
    ) |>
    dplyr::arrange(.data$run_id, .data$fishery_id, .data$age, .data$time_step)


  attr(fishery_mort, 'species') <- fram_db$fram_db_species
  return(fishery_mort)


}



#' Plot total mortalities by fishery
#'
#' Creates an ordered bar chart with the top number of mortalities per fishery.
#'
#' @export
#'
#' @param fram_db fram database object, supplied through connect_fram_db
#' @param run_id numeric, RunID
#' @param stock_id numeric, ID of focal stock. Can accept multiple ids, but this should only be used
#'   when combining FRAM stocks makes sense (e.g., combining marked and unmarked components of the
#'   same stock).
#' @param top_n numeric, Number of fisheries to display
#' @param filters_list list of framrsquared filter functions to apply before plotting.
#' @param msp Use Model Stock Proportion? Logical, defaults to TRUE.
#' @param split_cnr Produce separate panels for CNR and non-CNR mortality? Logical, defaults to
#'   FALSE.
#' @param fishery_title_short Use abbreviated fishery names instead of full names? Useful if
#'   horizontal space is limited. Logical, defaults to FALSE.
#' @param stock_title_short Use abbreviated stock name instead of full name in title? Will always
#'   use abbreviated stock name if multiple stock ids are provided.
#' @param warn Warn if providing multiple stocks?
#'
#' @returns ggplot2 object.
#'
#' @seealso [plot_impacts_per_catch_heatmap()], [plot_stock_mortality_time_step()]
#'
#' @examples
#' \dontrun{
#' fram_db |> plot_stock_mortality(run_id = 132, stock_id = 17)
#' fram_db |> plot_stock_mortality(run_id = 132, stock_id = 17,
#'         filters_list = list(filter_wa, filter_sport))
#' }
#'

plot_stock_mortality <- function(fram_db, run_id, stock_id,
                                 top_n = 10,
                                 filters_list = NULL,
                                 msp = TRUE,
                                 split_cnr = FALSE,
                                 fishery_title_short = FALSE,
                                 stock_title_short = FALSE,
                                 warn = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id, n = 1)
  validate_stock_ids(fram_db, stock_id)
  validate_flag(msp)
  validate_flag(split_cnr)

  # make sure run ids are integers
  if (length(stock_id)>1 & warn) {
    cli::cli_alert_warning("Plot may not be meaningful when combining stock (unless combined FRAM stocks have biological interpretation). Consider providing a single value for stock_id.")
  }

  if(!is.null(filters_list) && !is.list(filters_list)){
    fram_abort("If provided, filters_list must be a list of fishery filter functions.")
  }
  if(!is.null(filters_list) && !all(purrr::map_vec(filters_list, \(x) is.function(x)))){
    fram_abort("If provided, filters_list must be a list of fishery filter functions. One or more list items is not a function.")
  }

  validate_flag(warn)

  # identify species used
  species_used = fetch_table_(fram_db, "RunID") |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(.data$species_name)

  # lut for display of stock name
  stocks <- fram_db |>
    fetch_table_('Stock') |>
    dplyr::filter(.data$species %in% fram_db$fram_db_species) |>
    dplyr::select("stock_id", "stock_name", "stock_long_name")

  # lut for display of fishery
  if(fishery_title_short){
    fisheries <- fram_db |>
      fetch_table_('Fishery') |>
      dplyr::filter(.data$species == fram_db$fram_db_species) |>
      dplyr::select("fishery_id",
                    fishery_label = "fishery_name")
  } else {
    fisheries <- fram_db |>
      fetch_table_('Fishery') |>
      dplyr::filter(.data$species == fram_db$fram_db_species) |>
      dplyr::select("fishery_id",
                    fishery_label = "fishery_title")
  }

  fisheries <- fisheries |>
    dplyr::mutate(fishery_label = glue::glue("{fishery_label} | (id = {fishery_id})"))

  if(species_used == "CHINOOK"){
    mortality <- fram_db |>
      aeq_mortality_(msp = msp) |>
      dplyr::filter(.data$time_step != 1)
  } else {
    mortality <- fram_db |> fetch_table_('Mortality')
  }

  mortality <- mortality |>
    dplyr::filter(.data$run_id == .env$run_id,
                  .data$stock_id %in% .env$stock_id) |>
    dplyr::group_by(.data$run_id, .data$fishery_id) |>
    dplyr::summarize(
      dplyr::across(c("landed_catch":"drop_off",
                      "msf_landed_catch":"msf_drop_off"), \(x) sum(x)),
      .groups='drop')

  if(split_cnr){
    mortality <- mortality |>
      dplyr::mutate(non_cnr = .data$landed_catch +
                      .data$shaker +
                      .data$drop_off +
                      .data$msf_landed_catch +
                      .data$msf_non_retention +
                      .data$msf_shaker +
                      .data$msf_drop_off,
                    cnr = .data$non_retention) |>
      tidyr::pivot_longer(
        cols = c("non_cnr", "cnr"),
        names_to = "mortality_type",
        values_to = "total_mortality"
      )|>
      dplyr::select("run_id", "fishery_id", total_mort = "total_mortality",
                    "mortality_type")
    percent_cnr = NULL ## dummy var to avoid silly error
  } else{
    total_cnr = sum(mortality$non_retention)
    mortality <- mortality |>
      add_total_mortality() |>
      dplyr::select("run_id", "fishery_id", total_mort = "total_mortality")
    percent_cnr = round(total_cnr / sum(mortality$total_mort)*100,
                        1)
  }

  if(!is.null(filters_list)){
    ## give species for filtering
    attr(mortality, "species") <- species_used
    for(i in 1:length(filters_list)){
      mortality <- mortality |>
        filters_list[[i]]()
    }
  }

  run_name <- fram_db |>
    fetch_table_('RunID') |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(.data$run_name)

  if(stock_title_short | length(stock_id) > 1){
    stock_name <- stocks |>
      dplyr::filter(.data$stock_id %in% .env$stock_id) |>
      dplyr::pull(.data$stock_name)
  } else {
    stock_name <- stocks |>
      dplyr::filter(.data$stock_id %in% .env$stock_id) |>
      dplyr::pull(.data$stock_long_name)
  }

  stock_name = glue::glue_collapse(stock_name, sep = ", ")


  if(split_cnr){
    mortality_primary = mortality |>
      dplyr::filter(.data$mortality_type == "non_cnr")
    mortality_cnr = mortality |>
      dplyr::filter(.data$mortality_type == "cnr")
  } else{
    mortality_primary = mortality
  }

  x_max = max(mortality_primary$total_mort)

  gp <- mortality_primary |>
    dplyr::slice_max(.data$total_mort, n = top_n) |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    ggplot2::ggplot(ggplot2::aes(.data$total_mort,
                                 stats::reorder(.data$fishery_label, .data$total_mort))) +
    ggplot2::geom_col() +
    ggplot2::xlim(c(0, x_max))+
    ggplot2::labs(
      title = glue::glue('Top mortality for stock {stock_name} (stock_id = {glue::glue_collapse(stock_id, ", ")}) | {percent_cnr}% CNR'),
      subtitle = glue::glue('{run_name} (run_id = {run_id})'),
      x = ifelse(species_used == 'COHO','Mortalities', 'AEQs (timesteps 2-4)'),
      y = 'Fishery'
    )

  if(split_cnr){

    gp = gp+
      ggplot2::ggtitle(glue::glue('Top non-CNR mortality for stock {stock_name} (stock_id = {glue::glue_collapse(stock_id, ", ")})'))

    gp_cnr = mortality_cnr |>
      dplyr::slice_max(.data$total_mort, n = top_n) |>
      dplyr::inner_join(fisheries, by = 'fishery_id') |>
      ggplot2::ggplot(ggplot2::aes(.data$total_mort,
                                   stats::reorder(.data$fishery_label, .data$total_mort))) +
      ggplot2::geom_col() +
      ## put on same scale, unless CNR is more.
      ggplot2::xlim(c(0, x_max))+
      ggplot2::labs(
        title = glue::glue('Top CNR mortality'),
        x = ifelse(species_used == 'COHO','Mortalities', 'AEQs (timesteps 2-4)'),
        y = 'Fishery'
      )
    gp_final = patchwork::wrap_plots(gp, gp_cnr, ncol = 1) +
      patchwork::plot_layout(axes = "collect")
  } else {
    gp_final = gp
  }

  return(gp_final)
}





#' Plot total mortalities by fishery and timestep
#'
#' Creates an ordered bar chart with the top number of mortalities per fishery and time step.
#'
#' @export
#'
#' @inheritParams plot_stock_mortality
#'
#' @returns ggplot2 object
#'
#' @seealso [plot_stock_mortality()], [plot_impacts_per_catch_heatmap()]
#'
#' @examples
#' \dontrun{
#' fram_db |> stock_mortality_time_step(run_id = 132, stock_id = 17)
#' }
#'

plot_stock_mortality_time_step <- function(fram_db,
                                           run_id,
                                           stock_id,
                                           top_n = 10,
                                           filters_list = NULL,
                                           msp = TRUE,
                                           split_cnr = FALSE,
                                           fishery_title_short = FALSE,
                                           stock_title_short = FALSE,
                                           warn = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)
  validate_stock_ids(fram_db, stock_id)
  validate_flag(msp)


  if (length(run_id)>1) {
    cli::cli_alert_warning("Plot is not meaningful when combining multiple runs. Provide a single run in run_id.")
  }

  # make sure run ids are integers
  if (length(stock_id)>1 & warn) {
    cli::cli_alert_warning("Plot may not be meaningful when combining stock (unless combined FRAM stocks have biological interpretation). Consider providing a single value for stock_id.")
  }

  if(!is.null(filters_list) && !is.list(filters_list)){
    fram_abort("If provided, filters_list must be a list of fishery filter functions.")
  }
  if(!is.null(filters_list) && !all(purrr::map_vec(filters_list, \(x) is.function(x)))){
    fram_abort("If provided, filters_list must be a list of fishery filter functions. One or more list items is not a function.")
  }

  species_used = fetch_table_(fram_db, "RunID") |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(.data$species_name)

  # lut for display of stock name
  stocks <- fram_db |>
    fetch_table_('Stock') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select("stock_id", "stock_name", "stock_long_name")

  # lut for display of fishery
  if(fishery_title_short){
    fisheries <- fram_db |>
      fetch_table_('Fishery') |>
      dplyr::filter(.data$species == fram_db$fram_db_species) |>
      dplyr::select("fishery_id",
                    fishery_label = "fishery_name")
  } else {
    fisheries <- fram_db |>
      fetch_table_('Fishery') |>
      dplyr::filter(.data$species == fram_db$fram_db_species) |>
      dplyr::select("fishery_id",
                    fishery_label = "fishery_title")
  }

  fisheries <- fisheries |>
    dplyr::mutate(fishery_label = glue::glue("{fishery_label} | (id = {fishery_id})"))


  if(fram_db$fram_db_species == "CHINOOK"){
    mortality <- fram_db |> aeq_mortality_(msp = msp)|>
      dplyr::filter(.data$time_step != 1)
  } else {
    mortality <- fram_db |> fetch_table_('Mortality')
  }

  mortality <- mortality |>
    dplyr::filter(.data$run_id == .env$run_id,
                  .data$stock_id %in% .env$stock_id) |>
    dplyr::group_by(.data$run_id, .data$time_step, .data$fishery_id) |>
    dplyr::summarize(
      dplyr::across(c("landed_catch":"drop_off",
                      "msf_landed_catch":"msf_drop_off"), \(x) sum(x)),
      .groups='drop')

  if(split_cnr){
    mortality <- mortality |>
      dplyr::mutate(non_cnr = .data$landed_catch +
                      .data$shaker +
                      .data$drop_off +
                      .data$msf_landed_catch +
                      .data$msf_non_retention +
                      .data$msf_shaker +
                      .data$msf_drop_off,
                    cnr = .data$non_retention) |>
      tidyr::pivot_longer(
        cols = c("non_cnr", "cnr"),
        names_to = "mortality_type",
        values_to = "total_mortality"
      )|>
      dplyr::select("run_id", "fishery_id", total_mort = "total_mortality",
                    "mortality_type",
                    "time_step")
    percent_cnr = NULL ## dummy var to avoid silly error
  } else{
    total_cnr = sum(mortality$non_retention)
    mortality <- mortality |>
      add_total_mortality() |>
      dplyr::select("run_id", "fishery_id", total_mort = "total_mortality",
                    "time_step")
    percent_cnr = round(total_cnr / sum(mortality$total_mort)*100,
                        1)
  }

  if(!is.null(filters_list)){
    ## give species for filtering
    attr(mortality, "species") <- species_used
    for(i in 1:length(filters_list)){
      mortality <- mortality |>
        filters_list[[i]]()
    }
  }

  mortality = mortality |>
    label_timesteps_db(fram_db = fram_db)


  run_name <- fram_db |>
    fetch_table_('RunID') |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(.data$run_name)

  if(stock_title_short | length(stock_id) > 1){
    stock_name <- stocks |>
      dplyr::filter(.data$stock_id %in% .env$stock_id) |>
      dplyr::pull(.data$stock_name)
  } else {
    stock_name <- stocks |>
      dplyr::filter(.data$stock_id %in% .env$stock_id) |>
      dplyr::pull(.data$stock_long_name)
  }

  stock_name <- paste0(stock_name, collapse = ', ')


  if(split_cnr){
    mortality_primary = mortality |>
      dplyr::filter(.data$mortality_type == "non_cnr")
    mortality_cnr = mortality |>
      dplyr::filter(.data$mortality_type == "cnr")
  } else{
    mortality_primary = mortality
  }

  ## primary

  mort_table <- mortality_primary |>
    dplyr::group_by(.data$run_id, .data$fishery_id) |>
    dplyr::summarize(
      dplyr::across("total_mort", \(x) sum(x)),
      .groups='drop') |>
    # dplyr::ungroup()
    dplyr::slice_max(.data$total_mort, n = top_n)

  x_max = max(mort_table$total_mort)

  top_fish <- mortality_primary |>
    dplyr::filter(.data$fishery_id %in% mort_table$fishery_id)


  gp <- top_fish |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    ggplot2::ggplot(ggplot2::aes(
      .data$total_mort,
      stats::reorder(.data$fishery_label, .data$total_mort, function(x) {
        sum(x)
      }),
      fill = .data$time_step_label)
    ) +
    ggplot2::geom_col(alpha = .6) +
    ggplot2::scale_fill_brewer(palette = 'Set1') +
    ggplot2::xlim(c(0, x_max))+
    ggplot2::labs(
      title = glue::glue('Top mortality for stock {stock_name} (stock_id = {glue::glue_collapse(stock_id, ", ")}) | {percent_cnr}% CNR'),
      subtitle = glue::glue('{run_name} (run_id = {run_id})'),
      x = ifelse(species_used == 'COHO','Mortalities', 'AEQs'),
      y = 'Fishery',
      fill = "Timestep"
    ) +
    ggplot2::theme(legend.position = "top")


  if(split_cnr){

    gp = gp+
      ggplot2::ggtitle(glue::glue('Top non-CNR mortality for stock {stock_name} (stock_id = {glue::glue_collapse(stock_id, ", ")})'))

    mort_table <- mortality_cnr |>
      dplyr::group_by(.data$run_id, .data$fishery_id) |>
      dplyr::summarize(
        dplyr::across("total_mort", \(x) sum(x)),
        .groups='drop') |>
      dplyr::slice_max(.data$total_mort, n = top_n) |>
      dplyr::pull(.data$fishery_id)

    top_fish <- mortality_cnr |>
      dplyr::filter(.data$fishery_id %in% mort_table)


    gp_cnr <- top_fish |>
      dplyr::inner_join(fisheries, by = 'fishery_id') |>
      ggplot2::ggplot(ggplot2::aes(
        .data$total_mort,
        stats::reorder(.data$fishery_label, .data$total_mort, function(x) {
          sum(x)
        }),
        fill = .data$time_step_label
      )) +
      ggplot2::geom_col(alpha = .6) +
      ggplot2::scale_fill_brewer(palette = 'Set1') +
      ggplot2::xlim(c(0, x_max))+
      ggplot2::labs(
        title = glue::glue('Top CNR mortality'),
        subtitle = glue::glue('{run_name} (run_id = {run_id})'),
        x = ifelse(species_used == 'COHO','Mortalities', 'AEQs'),
        y = 'Fishery'
      ) +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     legend.position = "none")

    gp_final = patchwork::wrap_plots(gp, gp_cnr, ncol = 1) +
      patchwork::plot_layout(axes = "collect")

  }  else  {
    gp_final = gp
  }
  return(gp_final)
}
