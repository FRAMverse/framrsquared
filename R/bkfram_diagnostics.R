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

  raw_lines <- readr::read_lines(filepath,
                                 skip = 4)
  parsed <- stringr::str_match(
    raw_lines,
    "^\\s*(\\d+)\\s+" |>
      paste0(
        "(\\d+)\\s+",
        "(\\S+)\\s+",
        "(\\S+)\\s+",
        "(\\S+)\\s+",
        "(\\S+)\\s+",
        "(\\S+)\\s+",
        "(\\S+)\\s+",
        "(.*)$"
      )
  )[, -1]
  dat <- tibble::as_tibble(parsed,
                           .name_repair = ~ c(
                             "iteration",
                             "stock_id",
                             "escapement",
                             "escapement_target",
                             "esc_target_ratio",
                             "old_scalar",
                             "new_scalar",
                             "starting_cohort",
                             "stock_name"
                           )) |>
    dplyr::mutate(dplyr::across("iteration":"starting_cohort",
                                numerify_text))

  return(dat)

}

## tiny helper for converting text file numeric columns to numerics with
## appropriate NAs
numerify_text <- function(x){
  x[x == "*"] <- NA
  x[x == "NaN"] <- NA
  x[x == "-"] <- NA
  as.numeric(x)
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

  if(nrow(data) == 0){
    if(verbose){
      cli::cli_alert_success("No stocks outside of {thresh} of 1:1 ratio by iteration {target_iteration}!")
    }
    title = glue::glue("All escapements converged by Iteration {target_iteration}!")
    subtitle = glue::glue("(for a threshold of {thresh}")
  } else {
    title = glue::glue("Imperfect escapement convergence, Iteration {target_iteration}")
    subtitle = glue::glue("Excluding stock within {thresh} of a perfect ratio")
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
      title = title,
      subtitle = subtitle
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


  if(nrow(data) == 0){
    if(verbose){
      cli::cli_alert_success("No stocks off by more than {thresh} fish by iteration {target_iteration}!")
    }
    title = glue::glue("All escapements converged by Iteration {target_iteration}!")
    subtitle = glue::glue("(for a threshold of {thresh}")
  } else {
    title = glue::glue("Imperfect escapement convergence, Iteration {target_iteration}")
    subtitle = glue::glue("Excluding stock within {thresh} fish of target")
  }

  ## plotting
  data |>
    ggplot2::ggplot(ggplot2::aes(y = stats::reorder(.data$stock_label, .data$escapement_diff),
                                 x = escapement_diff)) +
    ggplot2::geom_vline(xintercept = 1, linetype = 2) +
    ggplot2::geom_col()+
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(
      y = 'Stock',
      x = x_label,
      title = title,
      subtitle = subtitle
    )
}



