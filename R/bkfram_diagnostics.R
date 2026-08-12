#' Parse BackwardsFramCheck.Txt
#'
#' Parses the "BackwardFramCheck.Txt" output of a backwards Coho FRAM run. This contains useful information on
#' how escapement has (hopefully) converged on target escapement across the backwards FRAM iterations.
#' Does not do any additional processing; users should probably be using [process_bkfram_check()]
#' Requires `readr` package.
#'
#' @param filepath complete filepath to a `BackFramCheck.Txt` file.
#'
#' @seealso [process_bkfram_check()]
#' @returns
#'  Tibble form of "BackwardFramCheck.Txt" starting from line 4 (after metadata).
#' @export
#'
#' @examples
#' data = parse_bkfram_check(system.file("BaseFramCheck.Txt", package = "framrsquared.dev"))
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
                                numerify_text)) |>
    dplyr::mutate(stock_name = gsub("- ", "", .data$stock_name))

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

#' Aggregates data from a bkfram check file to the joint marked-unmarked coho stock level.
#'
#' Only works for Coho!
#'
#' @param data Tibble, output of  [parse_bkfram_check()]
#'
#' @returns Tibble with columns based on bkfram check file.
#' Additional columns: `missing_start_cohort`:
#'
#' - `$missing_start_cohort`: logical, TRUE if both marked and unmarked components have NAs for starting cohorts (useful for filtering out stocks that aren't being solved for)
#' - `$stock_aggregate_name`: `stock_name` without the "M-" or "U-" prefix
#' - `$stock_id` and `$stock_name`: list-columns of 2-d vectors with both stock_id and stock_name values for each combined stock. Can be `unnest`'d to make lookup table (see example)
#'
#' @examples
#' \dontrun{
#' data <- parse_bkfram_check(system.file("BackFramCheck.Txt", package = "framrsquared.dev"))
#' data <- aggregate_bkfram_check(data)
#' lut <- out |>
#'  dplyr::select(stock_name, stock_id, stock_aggregate_id, stock_aggregate_name) |>
#'  dplyr::distinct() |>
#'  tidyr::unnest(cols = c("stock_name", "stock_id"))
#'  }
aggregate_bkfram_check <- function(data){
  data <- data |>
    dplyr::mutate(stock_aggregate_id = ceiling(.data$stock_id/2),
                  stock_aggregate_name = gsub("M-", "", .data$stock_name),
                  stock_aggregate_name = gsub("U-", "", .data$stock_aggregate_name)
    ) |>
    dplyr::summarize(escapement = sum(.data$escapement),
                     escapement_target = sum(.data$escapement_target),
                     starting_cohort = sum(.data$starting_cohort),
                     stock_id = list(c(.data$stock_id)),
                     stock_name = list(c(.data$stock_name)),
                     missing_start_cohort = all(is.na(.data$starting_cohort)),
                     .by = c("iteration", "stock_aggregate_name", "stock_aggregate_id")) |>
    dplyr::mutate(esc_target_ratio = .data$escapement_target/.data$escapement)

  return(data = data)

}


#' Read and process backwards FRAM check file
#'
#' Parses and processes the "BackwardFramCheck.Txt" output of a backwards Coho FRAM run.
#' Primarily intended to support plotting functions. Adds "stock_label" based on short stock name
#' and stock id in "BackwardFramCheck.Txt".
#' Optionally aggregates marked and unmarked stocks together (argument `aggregate_stocks`),
#' Optionally filters to one or more stock ids (argument `stock_id`).
#'
#' @param filepath Full filepath for `BackwardFramCheck.Txt` file. Character.
#' @param stock_id One or more Coho stock IDs to filter to. Numeric, optional, defaults to `NULL`.
#' @param aggregate_stocks Should
#'
#' @returns Tibble; columns depend on whether aggregation was performed or not, but always returns these key columns:
#'
#' - `$iteration`: fram iteration
#' - `$escapement`: model escapement for stock or stock aggregrate
#' - `$escapement_target`: corresponding target escamenet
#' - `$esc_target_ratio`: target/model escapement
#' - `$stock_label`: stock name and id(s), depending if aggregated or not.
#'
#' @export
#'
#' @examples
#' process_bkfram_check(system.file("BaseFramCheck.Txt",
#'                       package = "framrsquared.dev"),
#'                       aggregate_stocks = FALSE)
#' process_bkfram_check(system.file("BaseFramCheck.Txt",
#'                      package = "framrsquared.dev"),
#'                      aggregate_stocks = TRUE)
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
#' whether  and at what iterations convergence occurred for one or more stocks by plotting the trajectory of escapements across
#' FRAM iterations and overlaying the target escapement. If stocks have a flag of 2 in the `BackwardFRAM`
#' table, backwards FRAM solves for the joint escapement of the marked and unmarked components of stocks.
#' By default, `plot_bkfram_convergence_trace()` aggregates marked and unmarked components, which will match
#' this behavior. Optional argument `aggregate_stocks` can be set to `FALSE` to instead plot
#' convergence of marked and unmarked components separately; this can lead to misleading behavior in which
#' stocks with a flag of 2 in the FRAM database appear to never converge.
#'
#' @param filepath Complete filepath to a `BackFramCheck.Txt` file.
#' @param stock_id One or more stock ids. Numeric.
#' @param aggregate_stocks Should marked and unmarked stocks be combined?
#' This better represents convergence for stocks with a flag of 2. Logical, defaults to TRUE.
#'
#' @seealso [plot_bkfram_convergence_bar()], [plot_bkfram_convergence_scatter()]
#'
#' @returns ggplot2 object.
#' @export
#'
#' @examples
#'  plot_bkfram_convergence_trace(system.file("BaseFramCheck.Txt",
#'                                package = "framrsquared.dev"),
#'                                stock_id = 19:23)
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
    dplyr::filter(.data$iteration != 1) |>
    ggplot2::ggplot(ggplot2::aes(.data$iteration, .data$escapement,
                                 col = .data$stock_label,
                                 group = .data$stock_label))+
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

#' Barplots of stocks that fail to converge by iteration X
#'
#' Filters to stocks whose model escapement at iteration of interest are not within `thresh` of the target ratio (or target value),
#' and creates a barplot of how much these stocks differ from the target ratio/value.
#'
#' By default, filters and plots based on the target ratio (e.g., `(target escapement)/(model escapement)`, where
#' a threshold of 0.01 means filtering out stocks with ratios between 0.99 and 1.01. Optionally can instead
#' filter and plot based on the difference in fish between target and model escapements.
#'
#'
#' @param filepath Full filepath to "BackFramCheck.Txt"
#' @param iteration What iteration should we plot? If not provided, plot final iteration. Numeric, defaults to `NULL`
#' @param plot_ratio Should the filtering and plotting be based on ratios (`TRUE`) or total fish (`FALSE`). See details. Logical, defaults to `TRUE`.
#' @param aggregate_stocks Should marked and unmarked stocks be combined? Logical, defaults to `TRUE`.
#' @param thresh Threshold. If `plot_ratio == TRUE`, a thresh of 0.01 filters out stocks with ratios target/modeled escapement of 0.99 to 1.01. If `plot_ratio == FALSE`, a thresh of 0.01 filters out stocks with target - modeled escapements of -0.01 to 0.01. Numeric, defaults to 0.01
#' @param verbose Print optional statements to CLI?
#'
#' @seealso [plot_bkfram_convergence_trace()], [plot_bkfram_convergence_scatter()]
#'
#' @returns ggplot2 object.
#' @export
#'
#' @examples
#'  plot_bkfram_convergence_bar(system.file("BaseFramCheck.Txt",
#'                              package = "framrsquared.dev"),
#'                              iteration = 20)
plot_bkfram_convergence_bar <- function(filepath,
                                        iteration = NULL,
                                        plot_ratio = TRUE,
                                        aggregate_stocks = TRUE,
                                        thresh = 0.01,
                                        verbose = TRUE
                                        ){

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

  gp +
    ggplot2::theme_bw(base_size = 13)
}


## helper function that feeds into `plot_bkfram_convergence_bar`
plot_bkfram_convergence_bar_ratio <- function(data,
                                              target_iteration,
                                              verbose,
                                              thresh,
                                              x_label){
  data <- data |>
    dplyr::filter(.data$iteration == .env$target_iteration) |>
    dplyr::filter_out(abs(.data$esc_target_ratio - 1) < .env$thresh) |>
    dplyr::filter(!is.na(.data$esc_target_ratio),
                  !.data$missing_start_cohort)

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
                                 x = .data$esc_target_ratio))+
    ggplot2::geom_vline(xintercept = 1, linetype = 2)+
    ggplot2::geom_col()+
    ggplot2::labs(
      y = 'Stock',
      x = x_label,
      title = title,
      subtitle = subtitle
    )
}

## helper function that feeds into `plot_bkfram_convergence_bar`
plot_bkfram_convergence_bar_diff <- function(data,
                                             target_iteration,
                                             verbose,
                                             thresh,
                                             x_label){
  data <- data |>
    dplyr::filter(.data$iteration == .env$target_iteration) |>
    dplyr::mutate(escapement_diff = .data$escapement_target - .data$escapement) |>
    dplyr::filter_out(abs(.data$escapement_diff) < .env$thresh) |>
    dplyr::filter(!.data$missing_start_cohort)


  if(nrow(data) == 0){
    if(verbose){
      cli::cli_alert_success("No stocks off by more than {thresh} fish by iteration {target_iteration}!")
    }
    title = glue::glue("All escapements converged by Iteration {target_iteration}!")
    subtitle = glue::glue("(for a threshold of {thresh})")
  } else {
    title = glue::glue("Imperfect escapement convergence, Iteration {target_iteration}")
    subtitle = glue::glue("Excluding stock within {thresh} fish of target")
  }

  ## plotting
  data |>
    ggplot2::ggplot(ggplot2::aes(y = stats::reorder(.data$stock_label, .data$escapement_diff),
                                 x = .data$escapement_diff)) +
    ggplot2::geom_vline(xintercept = 1, linetype = 2) +
    ggplot2::geom_col()+
    ggplot2::labs(
      y = 'Stock',
      x = x_label,
      title = title,
      subtitle = subtitle
    )
}


#' Barplots of stocks that fail to converge by iteration X
#'
#' Filters to stocks whose model escapement at iteration of interest are not within `thresh` of the target ratio (or target value),
#' and creates a scatterplot of target vs modeled escapement. If many stocks would be plotted, filters to the most extreme `max_n` of them.
#'
#' @inheritParams plot_bkfram_convergence_bar
#' @param max_n Maximum number of stocks to plot. Numeric, defaults to 10.
#' @param label_size Size of label text; may want to adjust for readability based on plot size. Numeric, defaults to 5.
#'
#' @seealso [plot_bkfram_convergence_trace()], [plot_bkfram_convergence_bar()]
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#'  plot_bkfram_convergence_bar(system.file("BaseFramCheck.Txt",
#'                                          package = "framrsquared.dev"),
#'                                          iteration = 20)
plot_bkfram_convergence_scatter <- function(filepath,
                                            iteration = NULL,
                                            aggregate_stocks = TRUE,
                                            verbose = TRUE,
                                            thresh = 0.01,
                                            max_n = 10,
                                            label_size = 4){

  rlang::check_installed("ggrepel")

  validate_character(filepath)
  validate_numeric(iteration, n = 1, allow_null = TRUE)
  validate_flag(aggregate_stocks)
  validate_flag(verbose)
  validate_numeric(thresh, n = 1)
  validate_numeric(max_n, n = 1)
  validate_numeric(label_size, n = 1)

  data = process_bkfram_check(filepath = filepath,
                              aggregate_stocks = aggregate_stocks)



  if(is.null(iteration)){
    target_iteration = max(data$iteration)
    title = glue::glue("Imperfect convergence, final iteration")
  } else {
    target_iteration = iteration
    title = glue::glue("Imperfect convergence, Iteration {target_iteration}")
  }


  data <- data |>
    dplyr::filter(abs(data$esc_target_ratio-1) > .env$thresh,
                  .data$iteration == .env$target_iteration,
                  !.data$missing_start_cohort)

  if(nrow(data) == 0){
    if(verbose){
      cli::cli_alert_success("No stocks outside of {thresh} of 1:1 ratio by iteration {target_iteration}!")
    }
    title = glue::glue("All escapements converged by Iteration {target_iteration}!")
    subtitle = glue::glue("(for a threshold of {thresh}")
  } else {
    title = glue::glue("Imperfect escapement convergence, Iteration {target_iteration}")
    subtitle = glue::glue("Excluding stock within {thresh} of perfect ratio (dashed line)")
  }

  if(nrow(data) > max_n){
    cli::cli_alert_warning("{nrow(data)} stocks non-converged by iteration {target_iteration}! Plotting most extreme {max_n}.")
    data = data |>
      dplyr::mutate(sort_score = abs(.data$esc_target_ratio -1)) |>
      dplyr::arrange(-.data$sort_score) |>
      utils::head(max_n)
  }


  gp <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$escapement_target,
                                 y = .data$escapement,
                                 label = .data$stock_label)) +
    ggplot2::geom_abline(slope = 1, linetype = 2) +
    ggplot2::geom_point()+
    ggrepel::geom_label_repel(size = label_size) +
    ggplot2::labs(
      y = glue::glue('Model Escapement, Iteration {target_iteration}'),
      x = "Target Escapement",
      title = title,
      subtitle = "Dashed line = 1:1"
    )+
    ggplot2::scale_x_continuous(labels = \(x) format(x, big.mark = ","))+
    ggplot2::scale_y_continuous(labels = \(x) format(x, big.mark = ","))+
    ggplot2::theme_bw(base_size = 13)

  return(gp)
}


