#' Returns a tibble matching the Fishery Mortality screen.
#' @param fram_db FRAM database object
#' @param run_id Run ID
#' @param msp Model Stock Proportion, default TRUE
#' @export
#' @examples
#' \dontrun{
#' fram_db |> fishery_mortality(run_id = 101)
#' }
fishery_mortality <- function(fram_db, run_id = NULL, msp = TRUE) {
  validate_fram_db(fram_db)
  if(!is.numeric(run_id)){validate_run_id(fram_db, run_id)}

  fishery_mort <- fram_db |>
    fetch_table("Mortality") |>
    dplyr::group_by(
      .data$run_id,
      .data$age,
      .data$fishery_id,
      .data$time_step
    ) |>
    dplyr::summarize(
      dplyr::across(
        c(
          .data$landed_catch:.data$drop_off,
          .data$msf_landed_catch:.data$msf_drop_off
        ),
        \(x) sum(x)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(.data$landed_catch:.data$msf_drop_off) |>
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
      .data$run_id:.data$time_step, .data$landed_catch,
      .data$non_retention, .data$shaker, .data$drop_off
    ) |>
    dplyr::arrange(.data$run_id, .data$fishery_id, .data$age, .data$time_step)


  if (is.null(run_id)) {
    fishery_mort |> # returns fishery mortality for all runs in db
      `attr<-`('species', fram_db$fram_db_species)
  } else {
    fishery_mort |>
      dplyr::filter(.data$run_id %in% .env$run_id) |>
      `attr<-`('species', fram_db$fram_db_species)
  }


}



#' Creates an ordered bar chart with
#' the top number of mortalities per
#' fishery.
#'
#' @export
#'
#' @param fram_db fram database object, supplied through connect_fram_db
#' @param run_id numeric, RunID
#' @param stock_id numeric, ID of focal stock
#' @param top_n numeric, Number of fisheries to display
#' @param filters_list list of framrsquared filter functions to apply before plotting.
#' @param msp Use Model Stock Proportion expansion? Default is true
#'
#' @examples
#' \dontrun{
#' fram_db |> plot_stock_mortality(run_id = 132, stock_id = 17)
#' fram_db |> plot_stock_mortality(run_id = 132, stock_id = 17,
#'         filters_list = list(filter_wa, filter_marine))
#' }
#'

plot_stock_mortality <- function(fram_db, run_id, stock_id, top_n = 10, filters_list = NULL, msp = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_id)

  if (length(run_id)>1) {
    cli::cli_abort("Plot is not meaningful when combining multiple runs. Provide a single run in run_id.")
  }

  if (!is.logical(msp)) {
    cli::cli_abort("msp must be TRUE or FALSE.")
  }

  # make sure run ids are integers
  if (!is.numeric(stock_id)) {
    cli::cli_abort("Stock ID must be and integer")
  }

  if (length(stock_id)>1) {
    cli::cli_abort("Plot is not meaningful when combining stock. Provide a single value for stock_id.")
  }

  if(!is.null(filters_list) & !is.list(filters_list)){
    cli::cli_abort("If provided, filters_list must be a list of fishery filter functions.")
  }
  if(!is.null(filters_list) & !all(purrr::map_vec(filters_list, \(x) is.function(x)))){
    cli::cli_abort("If provided, filters_list must be a list of fishery filter functions. One or more list items is not a function.")
  }

  # identify species used
  species_used = fetch_table(fram_db, "RunID") |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(.data$species_name)

  # lut for display of stock name
  stocks <- fram_db |>
    fetch_table('Stock') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$stock_id, .data$stock_name)

  # lut for display of fishery
  fisheries <- fram_db |>
    fetch_table('Fishery') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)

  if(species_used == "CHINOOK"){
    mortality <- fram_db |> aeq_mortality(msp = msp)
  } else {
    mortality <- fram_db |> fetch_table('Mortality')
  }
  mortality <- mortality |>
    dplyr::filter(.data$run_id == .env$run_id,
                  .data$stock_id %in% .env$stock_id) |>
    dplyr::group_by(.data$run_id, .data$stock_id, .data$fishery_id) |>
    dplyr::summarize(
      dplyr::across(c(.data$landed_catch:.data$drop_off,
                      .data$msf_landed_catch:.data$msf_drop_off), \(x) sum(x)),
      .groups='drop') |>
    dplyr::mutate(
      total_mort = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off +
        .data$msf_landed_catch + .data$msf_non_retention + .data$msf_shaker + .data$msf_drop_off
    ) |>
    dplyr::select(.data$run_id, .data$stock_id, .data$fishery_id, .data$total_mort)

  if(!is.null(filters_list)){
    ## give species for filtering
    attr(mortality, "species") <- species_used
    for(i in 1:length(filters_list)){
      mortality <- mortality |>
        filters_list[[i]]()
    }
  }

  run_name <- fram_db |>
    fetch_table('RunID') |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(.data$run_name)

  stock_name <- stocks |>
    dplyr::filter(.data$stock_id == .env$stock_id) |>
    dplyr::pull(.data$stock_name)
  mortality |>
    dplyr::slice_max(.data$total_mort, n = top_n) |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    ggplot2::ggplot(ggplot2::aes(.data$total_mort, stats::reorder(.data$fishery_name, .data$total_mort))) +
    ggplot2::geom_col() +
    ggplot2::labs(
      subtitle = glue::glue('Top mortality for stock {stock_name} ({run_name})'),
      x = ifelse(species_used == 'COHO','Mortalities', 'AEQs'),
      y = 'Fishery'
    )
}

#' Creates an ordered bar chart with
#' the top number of mortalities per
#' fishery and time step.
#'
#' @export
#'
#' @inheritParams plot_stock_mortality
#'
#' @examples
#' \dontrun{
#' fram_db |> stock_mortality_time_step(run_id = 132, stock_id = 17)
#' }
#'

plot_stock_mortality_time_step <- function(fram_db, run_id, stock_id, top_n = 10, filters_list = NULL, msp = TRUE){
  #lifecycle::deprecate_warn('0.3.0','coho_stock_mortality_time_step()', with = 'stock_mortality()')
  validate_fram_db(fram_db, db_type = 'full')
  validate_run_id(fram_db, run_id)

  species_used = fetch_table(fram_db, "RunID") |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(.data$species_name)

  if (length(run_id)>1) {
    cli::cli_abort("Plot is not meaningful when combining multiple runs. Provide a single run in run_id.")
  }

  # make sure run ids are integers
  if (!is.numeric(stock_id)) {
    cli::cli_abort("Stock ID must be and integer")
  }

  # lut for display of stock name
  stocks <- fram_db |>
    fetch_table('Stock') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$stock_id, .data$stock_name)

  # lut for display of fishery
  fisheries <- fram_db |>
    fetch_table('Fishery') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)

  if(fram_db$fram_db_species == "CHINOOK"){
    mortality <- fram_db |> aeq_mortality(msp = msp)
  } else {
    mortality <- fram_db |> fetch_table('Mortality')
  }
  mortality <- mortality |>
    dplyr::filter(.data$run_id == .env$run_id,
                  .data$stock_id %in% .env$stock_id) |>
    dplyr::group_by(.data$run_id, .data$time_step, .data$fishery_id) |>
    dplyr::summarize(
      dplyr::across(c(.data$landed_catch:.data$drop_off,
                      .data$msf_landed_catch:.data$msf_drop_off), \(x) sum(x)),
      .groups='drop') |>
    dplyr::mutate(
      total_mort = .data$landed_catch + .data$non_retention + .data$shaker + .data$drop_off +
        .data$msf_landed_catch + .data$msf_non_retention + .data$msf_shaker + .data$msf_drop_off
    ) |>
    dplyr::select(.data$run_id, .data$fishery_id, .data$total_mort, .data$time_step)

  if(!is.null(filters_list)){
    ## give species for filtering
    attr(mortality, "species") <- species_used
    for(i in 1:length(filters_list)){
      mortality <- mortality |>
        filters_list[[i]]()
    }
  }

  run_name <- fram_db |>
    fetch_table('RunID') |>
    dplyr::filter(.data$run_id == .env$run_id) |>
    dplyr::pull(.data$run_name)

  stock_name <- stocks |>
    dplyr::filter(.data$stock_id %in% .env$stock_id) |>
    dplyr::pull(.data$stock_name)

  stock_name <- paste0(stock_name, collapse = ', ')

  mort_table <- mortality |>
    dplyr::group_by(.data$run_id, .data$fishery_id) |>
    dplyr::summarize(
      dplyr::across(.data$total_mort, \(x) sum(x)),
      .groups='drop') |>
    dplyr::slice_max(.data$total_mort, n = top_n) |>
    dplyr::pull(.data$fishery_id)

  top_fish <- mortality |>
    dplyr::filter(.data$fishery_id %in% mort_table)


  top_fish |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    ggplot2::ggplot(ggplot2::aes(
      .data$total_mort,
      stats::reorder(.data$fishery_name, .data$total_mort, function(x) {
        sum(x)
      }),
      fill = factor(.data$time_step)
    )) +
    ggplot2::geom_col(alpha = .6) +
    ggplot2::scale_fill_brewer(palette = 'Set1') +
    ggplot2::labs(
      subtitle = glue::glue('Top mortality for stock {stock_name} ({run_name})'),
      x = 'Mortalities',
      y = 'Fishery'
    ) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}
