#' Parse BackwardsFramCheck.Txt
#'
#' Parses the "BackwardFramCheck.Txt" output of a backwards Coho FRAM run. This contains useful information on
#' how escapement has (hopefully) converged on target escapement across the backwards FRAM iterations.
#'
#' Fixed width parsing based on Angelica code. Requires `readr` package.
#'
#' @param filepath complete filepath to a `BackFramCheck.Txt` file.
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' data = read_bkfram_check(here::here("BackFramCheck.Txt"))
parse_bkfram_check <- function(filepath){

  validate_character(filepath)

  rlang::check_installed("readr")

  data <- readr::read_fwf(filepath,
                          col_positions = readr::fwf_cols(
                            iteration = c(1,2),
                            stock_id = c(3,6),
                            escapement = c(7,18),
                            escapement_target = c(19,26),
                            esc_target_ratio = c(27, 36),
                            old_scalar = c(37, 49),
                            new_scalar = c(50, 66),
                            starting_cohort = c(61, 71),
                            stock_name = c(74, 82)
                          ),
                          skip=4, ## first 4 lines are metdata and headers
                          col_types = readr::cols(
                            iteration = readr::col_integer(),
                            stock_id = readr::col_integer(),
                            escapement = readr::col_integer(),
                            escapement_target = readr::col_integer(),
                            esc_target_ratio = readr::col_number(),
                            old_scalar = readr::col_number(),
                            new_scalar = readr::col_number(),
                            starting_cohort = readr::col_integer(),
                            stock_name = readr::col_character()
                          ),
                          na = c("", "NA", "-", "*"))

  return(data)

}

#' Title
#'
#' @param data
#'
#' @returns
#'
#' @examples
#' \dontrun{
#' data <- parse_bkfram_check(filepath)
#' data <- aggregate_bkfram_check(data)
#' lut <- out |>
#'  dplyr::select(stock_name, stock_id, stock_aggregate_id, stock_aggregate_name) |>
#'  dplyr::distinct() |>
#'  tidyr::unnest(cols = c("stock_name", "stock_id"))
#' }
aggregate_bkfram_check <- function(data){
  data <- data |>
    dplyr::mutate(stock_aggregate_id = ceiling(.data$stock_id/2),
                  stock_aggregate_name = gsub("M-", "", .data$stock_name),
                  stock_aggregate_name = gsub("U-", "", .data$stock_aggregate_name)
    ) |>
    dplyr::summarize(escapement = sum(.data$escapement),
                     escapement_target = sum(.data$escapement_target),
                     stock_id = list(c(stock_id)),
                     stock_name = list(c(stock_name)),
                     missing_start_cohort = all(is.na(starting_cohort)),
                     .by = c("iteration", "stock_aggregate_name", "stock_aggregate_id")) |>
    dplyr::mutate(esc_target_ratio = .data$escapement_target/.data$escapement)

  return(data = data)

}


process_bkfram_check <- function(filepath, stock_id = NULL, aggregate_stocks = TRUE){
  validate_character(filepath)
  validate_numeric(stock_id, allow_null = TRUE)
  validate_flag(aggregate_stocks)

  data = parse_bkfram_check(filepath)

  if(aggregate_stocks){
    data <- aggregate_bkfram_check(data) |>
      dplyr::rowwise() |>
      dplyr::mutate(stock_label = paste0(.data$stock_aggregate_name, " (", paste(stock_id, collapse = ", "), ")")) |>
      dplyr::ungroup()

    stock_name_lut <- data |>
      dplyr::select("stock_name", "stock_id", "stock_aggregate_id", "stock_aggregate_name", "stock_label") |>
      dplyr::distinct() |>
      tidyr::unnest(cols = c("stock_name", "stock_id"))

  } else{

    data <- data |>
      dplyr::mutate(stock_label = paste0(.data$stock_name, " (", .data$stock_id, ")"))

    stock_name_lut <- data |>
      dplyr::select("stock_id", "stock_name", "stock_label") |>
      dplyr::distinct()

  }


  if(!is.null(stock_id)){
    stock_label = stock_name_lut |>
      dplyr::filter(.data$stock_id %in% .env$stock_id) |>
      dplyr::pull(stock_label) |>
      unique()

    data <- data |>
      dplyr::filter(.data$stock_label %in% .env$stock_label)
  }

  return(data)
}

#' Plot trace of backwards FRAM convergence
#'
#' Backwards FRAM for Coho iterates towards target escapement values. This function helps visualize
#' whether convergence occurred for one or more stocks by plotting the trajectory of escapements across
#' FRAM iterations and overlaying the target escapement. If stocks have a flag of 2 in the `BackwardFRAM`
#' table, backwards FRAM solves for the joint escapement of the marked and unmarked components of stocks.
#' By default, `plot_bkfram_convergence_trace()` aggregates marked and unmarked components to match this behavior.
#' Optional argument `aggregate_stocks` can be set to `FALSE` to instead plot convergence of marked and unmarked
#' components separately.
#'
#' @param filepath  complete filepath to a `BackFramCheck.Txt` file.
#' @param stock_id One or more stock ids. Numeric.
#' @param aggregate_stocks Should marked and unmarked stocks be combined? This better represents convergence for stocks with a flag of 2. Logical, defaults to TRUE.
#'
#' @returns
#' @export
#'
#' @examples
plot_bkfram_convergence_trace <- function(filepath, stock_id, aggregate_stocks = TRUE){

  validate_character(filepath)
  validate_numeric(stock_id)
  validate_flag(aggregate_stocks)

  data = process_bkfram_check(filepath = filepath,
                              stock_id = stock_id,
                              aggregate_stocks = aggregate_stocks)

  stock_label = data$stock_label |> unique()

  if(length(stock_label) > 5){
    plot_title = glue::glue("Convergence of stocks {glue::glue_collapse(stock_id, sep = ', ')}")
  } else {
    plot_title = glue::glue("Convergence of {glue::glue_collapse(stock_label, sep = ', ')}")
  }

  data |>
    dplyr::filter(iteration != 1) |>
    ggplot2::ggplot(ggplot2::aes(.data$iteration, .data$escapement, col = .data$stock_label, group = .data$stock_label))+
    ggplot2::geom_line(ggplot2::aes(linetype = "Modeled")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$escapement_target, linetype = "Target"))+
    # ggplot::scale_x_discrete(breaks = seq(0, 100, 5)) +
    ggplot2::scale_y_continuous(labels = \(x) format(x, big.mark = ","))+
    ggplot2::labs(
      y = "Escapment",
      x = "FRAM Iteration",
      col = "Stock",
      linetype = "",
      title = plot_title
    )+
    ggplot2::theme_bw(base_size = 13)
}

plot_bkfram_convergence_bar <- function(filepath,
                                        iteration = NULL,
                                        plot_ratio = TRUE,
                                        aggregate_stocks = TRUE,
                                        verbose = TRUE,
                                        thresh = 0.01){

  validate_character(filepath)
  validate_numeric(iteration, n = 1, allow_null = TRUE)
  validate_flag(plot_ratio)
  validate_flag(aggregate_stocks)
  validate_flag(verbose)
  validate_numeric(thresh, n = 1)

  data = process_bkfram_check(filepath = filepath,
                              aggregate_stocks = aggregate_stocks)

  operator <- ifelse(plot_ratio, "/", "-")

  if(is.null(iteration)){
    target_iteration = max(data$iteration)
    x_label = glue::glue('Target Escapement {operator} Final Iteration Escapement')
  } else {
    target_iteration = iteration
    x_label = glue::glue('Target Escapement {operator} Model Escapement, Iteration {target_iteration}')
  }

  if(plot_ratio){
    gp <- plot_bkfram_convergence_bar_ratio(data = data,
                                            target_iteration = target_iteration,
                                            verbose = verbose,
                                            thresh = thresh,
                                            x_label = x_label)
  } else {
    gp <- plot_bkfram_convergence_bar_diff(data = data,
                                           target_iteration = target_iteration,
                                           verbose = verbose,
                                           thresh = thresh,
                                           x_label = x_label)
  }
  return(gp)
}


plot_bkfram_convergence_bar_ratio <- function(data,
                                              target_iteration,
                                              verbose,
                                              thresh,
                                              x_label){
  data <- data |>
    dplyr::filter(.data$iteration == target_iteration) |>
    dplyr::filter_out(abs(esc_target_ratio - 1) < thresh) |>
    dplyr::filter(!is.na(esc_target_ratio),
                  !missing_start_cohort)

  if(nrow(data) == 0 & verbose){
    cli::cli_alert_success("No stocks outside of {thresh} of 1:1 ratio by iteration {target_iteration}!")
  }

  ## plotting
  data |>
    ggplot2::ggplot(ggplot2::aes(y = stats::reorder(.data$stock_label, .data$esc_target_ratio),
                                 x = esc_target_ratio))+
    ggplot2::geom_vline(xintercept = 1, linetype = 2)+
    ggplot2::geom_col()+
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(
      y = 'Stock',
      x = x_label,
      title = glue::glue("Imperfect escapement convergence, Iteration {target_iteration}"),
      subtitle = glue::glue("Excluding stock within {thresh} of a perfect ratio")
    )
}

plot_bkfram_convergence_bar_diff <- function(data,
                                             target_iteration,
                                             verbose,
                                             thresh,
                                             x_label){
  data <- data |>
    dplyr::filter(.data$iteration == target_iteration) |>
    dplyr::mutate(escapement_diff = escapement_target - escapement) |>
    dplyr::filter_out(abs(escapement_diff) < thresh) |>
    dplyr::filter(!missing_start_cohort)

  if(nrow(data) == 0 & verbose){
    cli::cli_alert_success("No stocks off by more than {thresh} fish by iteration {target_iteration}!")
  }

  ## plotting
  data |>
    ggplot2::ggplot(ggplot2::aes(y = stats::reorder(.data$stock_label, .data$escapement_diff),
                                 x = escapement_diff))+
    ggplot2::geom_vline(xintercept = 1, linetype = 2)+
    ggplot2::geom_col()+
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(
      y = 'Stock',
      x = x_label,
      title = glue::glue("Imperfect escapement convergence, Iteration {target_iteration}"),
      subtitle = glue::glue("Excluding stock within {thresh} fish of target")
    )
}



