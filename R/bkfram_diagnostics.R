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
              .by = c("iteration", "stock_aggregate_name", "stock_aggregate_id")) |>
    dplyr::mutate(esc_target_ratio = .data$escapement_target/.data$escapement)

  return(data = data)

}


process_bkfram_check <- function(filepath, stock_id, aggregate_stocks = TRUE){
  validate_character(filepath)
  validate_numeric(stock_id)
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
      dplyr::mutate(stock_label = paste0(stock_name, " (", stock_id, ")"))

    stock_name_lut <- data |>
      dplyr::select("stock_id", "stock_name", "stock_label") |>
      dplyr::distinct()

  }

  stock_label = stock_name_lut |>
    dplyr::filter(.data$stock_id %in% .env$stock_id) |>
    dplyr::pull(stock_label) |>
    unique()

  data <- data |>
    dplyr::filter(.data$stock_label %in% .env$stock_label)
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
      dplyr::mutate(stock_label = paste0(stock_name, " (", stock_id, ")"))

    stock_name_lut <- data |>
      dplyr::select("stock_id", "stock_name", "stock_label") |>
      dplyr::distinct()

  }

  stock_label = stock_name_lut |>
    dplyr::filter(.data$stock_id %in% .env$stock_id) |>
    dplyr::pull(stock_label) |>
    unique()

  if(length(stock_label) > 5){
    plot_title = glue::glue("Convergence of stocks {glue::glue_collapse(stock_id, sep = ', ')}")
  } else {
    plot_title = glue::glue("Convergence of {glue::glue_collapse(stock_label, sep = ', ')}")
  }

  data |>
    dplyr::filter(.data$stock_label %in% .env$stock_label,
                  iteration != 1) |>
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


