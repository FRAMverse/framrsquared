#' Generates a dataframe that compares two runs identified by run_id's
#' @param fram_db FRAM database object
#' @param run_ids Vector of run_ids (two)
#' @export
#' @examples
#' \dontrun{fram_db |> compare_inputs(c(100,101))}
#'
compare_inputs <- function(fram_db, run_ids){
  # abort if do have two run ids

  if(length(run_ids) != 2){cli::cli_abort('Two valid run ids must be provided')}
  scalers <- fram_db |>
    fetch_table('FisheryScalers') |>
    dplyr::filter(.data$run_id %in% .env$run_ids)

  original <- scalers |>
    input_summary_(run_ids[[1]])

  comparison <- scalers |>
    input_summary_(run_ids[[2]])

  original |>
    dplyr::inner_join(comparison,by=c('fishery_id', 'time_step'), suffix = c('_original', '_comparison')) |>
    dplyr::mutate(
      percent_diff = (.data$total_quota_comparison - .data$total_quota_original) / .data$total_quota_original,
      reg_change = dplyr::case_when(
        .data$regulation_comparison != .data$regulation_original ~ paste0(.data$regulation_original, '->', .data$regulation_comparison),
        .data$total_quota_original == 0 & .data$total_quota_comparison > 0 ~ paste0('NR->', .data$regulation_comparison),
        .data$total_quota_comparison == 0 & .data$total_quota_original > 0 ~ paste0(.data$regulation_original, '->NR')
      )
    )
}


#' Generates a heat map of changed between two run inputs. Can be a very busy
#' chart if not filtered down. Consider using a filter.
#' @param .data Dataframe origination from the compare_inputs() function
#' @export
#' @examples
#' \dontrun{fram_db |> compare_inputs(c(100, 101)) |> compare_inputs_chart()}
compare_inputs_chart <- function(.data){
  .data |>
    dplyr::mutate(
      percent_diff = dplyr::if_else(is.infinite(.data$percent_diff), 1, .data$percent_diff),
      percent_diff = dplyr::if_else(is.na(.data$percent_diff), 0, .data$percent_diff),
      reg_change = dplyr::if_else(is.na(.data$reg_change), '', .data$reg_change)
    ) |>
    #dplyr::filter(!is.na(.data$percent_diff)) |>
    ggplot2::ggplot(ggplot2::aes(factor(.data$fishery_id), .data$time_step, fill = dplyr::if_else(.data$percent_diff > 0, 'pos', 'neg'), alpha=abs(.data$percent_diff))) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(scales::percent(round(.data$percent_diff,3)), '\n', .data$reg_change ))) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::labs(
      subtitle = 'FRAM input comparison heatmap',
      y = 'Time-Step',
      x = 'Fishery'
    ) +
    ggplot2::scale_alpha_identity()
}




#' Generates an input summary based on a FisheryScalers dataframe.
#' Probably end up streamlining / revising this.
#' @param .data FisheryFishery scalers dataframe
#' @param run_id Run ID number
#' @examples
#' \dontrun{fishery_scalers_dataframe |> input_summary()}
input_summary_ <- function(.data, run_id){
  .data |>
    dplyr::filter(
      .data$run_id == .env$run_id
    ) |>
    dplyr::mutate(
      total_quota = dplyr::case_when(
        .data$fishery_flag %in% c(1,2) ~ .data$quota,
        .data$fishery_flag %in% c(7,8) ~ .data$msf_quota,
        .data$ fishery_flag %in% c(17,18,27,28) ~ .data$quota + .data$msf_quota),
      regulation = dplyr::case_when(
        .data$fishery_flag %in% c(1,2) ~ 'NS',
        .data$fishery_flag %in% c(7,8) ~ 'MSF',
        .data$fishery_flag %in% c(17,18,27,28) ~ 'NS+MSF'
      )
    ) |>
    dplyr::select(.data$run_id,.data$fishery_id,.data$fishery_flag, .data$time_step, .data$total_quota, .data$regulation)
}


#' Outputs mismatches between runs to the console
#' @param fram_db FRAM database object
#' @param run_ids Vector of run_ids (two)
#' @param tolerance Perecent of difference for detection
#' @export
#' @examples
#' \dontrun{fram_db |> compare_runs(c(100,101))}
#'
compare_runs <- function(fram_db, run_ids, tolerance = 0.1) {
  # abort if don't have two run ids
  if(length(run_ids) != 2){cli::cli_abort('Two valid run ids must be provided')}
  # dplyr::pull things to make reading this easier
  # run names
  run <- fram_db |> fetch_table('RunID')
  run_names <- run |>
    dplyr::filter(.data$run_id %in% .env$run_ids) |>
    dplyr::select(.data$run_id, .data$run_name)

  run_1_name <- run_names |>
    dplyr::filter(.data$run_id == .env$run_ids[1]) |>
    dplyr::pull(.data$run_name)

  run_2_name <- run_names |>
    dplyr::filter(.data$run_id == .env$run_ids[2]) |>
    dplyr::pull(.data$run_name)

  cli::cli_h1('Comparing run {run_1_name} with {run_2_name}')

  # stock names
  stocks <- fram_db |> fetch_table('Stock') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$stock_id, .data$stock_name)

  # fishery names
  fisheries <- fram_db |> fetch_table('Fishery') |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)

  # recruit scalers
  recruits <- fram_db |>
    fetch_table('StockRecruit') |>
    dplyr::filter(.data$run_id %in% .env$run_ids) |>
    dplyr::select(.data$run_id, .data$stock_id, .data$recruit_scale_factor) |>
    tidyr::pivot_wider(names_from = .data$run_id,
                values_from = .data$recruit_scale_factor,
                names_glue = 'run_{run_id}') |>
    dplyr::filter(!!rlang::sym(glue::glue('run_{run_ids[1]}')) != !!rlang::sym(glue::glue('run_{run_ids[2]}'))) |>
    dplyr::inner_join(stocks, by = 'stock_id') |>
    dplyr::select(.data$stock_id, .data$stock_name, dplyr::everything())


  # show reslult
  if (nrow(recruits) > 0) {
    cli::cli_alert_warning('Recruit scalers mismatch between {run_1_name} and {run_2_name}')
    recruits |> print(n = Inf)
  } else{
    cli::cli_alert_success('Recruit scalers between {run_1_name} and {run_2_name} match')
  }

  # fishery scaler table differences
  if (tolerance != .1) {
    cli::cli_alert_info(glue::glue('Tolerance not set to {tolerance}'))
  }

  fishery <- fram_db |>
    fetch_table('FisheryScalers') |>
    dplyr::filter(.data$run_id %in% .env$run_ids) |>
    dplyr::select(dplyr::where(is.numeric)) |>
    tidyr::pivot_longer(-c(.data$run_id:.data$time_step)) |>
    dplyr::filter(.data$name != 'primary_key') |>
    dplyr::group_by(.data$fishery_id, .data$time_step, .data$name, .data$value) |>
    dplyr::mutate(count = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$count != 2)

  if (nrow(fishery) > 0) {
    cli::cli_alert_warning('Fishery Scaler mismatch between {run_1_name} and {run_2_name}')
    fishery |>
      dplyr::select(-.data$count) |>
      tidyr::pivot_wider(names_from = .data$run_id,
                  values_from = .data$value,
                  names_glue = 'run_{run_id}') |>
      dplyr::mutate(perecent_diff = abs((
        !!rlang::sym(glue::glue('run_{run_ids[2]}'))-!!rlang::sym(glue::glue('run_{run_ids[1]}'))
      ) / !!rlang::sym(glue::glue('run_{run_ids[1]}')))) |>
      dplyr::arrange(-.data$perecent_diff, .data$name) |>
      dplyr::filter(.data$perecent_diff > .env$tolerance) |>
      dplyr::inner_join(fisheries, by = 'fishery_id') |>
      dplyr::select(.data$fishery_id, .data$fishery_name, dplyr::everything()) |>
      print(n = Inf)
  } else{
    cli::cli_alert_success('Fishery scalers between {run_1_name} and {run_2_name} match')
  }

  non_retention <- fram_db |>
    fetch_table('NonRetention') |>
    dplyr::filter(.data$run_id %in% .env$run_ids) |>
    dplyr::select(dplyr::where(is.numeric)) |>
    tidyr::pivot_longer(-c(.data$run_id:.data$time_step)) |>
    dplyr::filter(.data$name != 'primary_key') |>
    dplyr::group_by(.data$fishery_id, .data$time_step, .data$name, .data$value) |>
    dplyr::mutate(count = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$count != 2)


  if (nrow(non_retention) > 0) {
    cli::cli_alert_warning('Non-retention mismatch between {run_1_name} and {run_2_name}')
    non_retention |>
      dplyr::select(-.data$count) |>
      tidyr::pivot_wider(names_from = .data$run_id,
                  values_from = .data$value,
                  values_fill = 0,
                  names_glue = 'run_{run_id}') |>
      dplyr::mutate(perecent_diff = abs((
        !!rlang::sym(glue::glue('run_{run_ids[2]}'))-!!rlang::sym(glue::glue('run_{run_ids[1]}'))
      ) / !!rlang::sym(glue::glue('run_{run_ids[1]}')))) |>
      dplyr::arrange(-.data$perecent_diff, .data$name) |>
      dplyr::filter(.data$perecent_diff > .env$tolerance) |>
      dplyr::inner_join(fisheries, by = 'fishery_id') |>
      dplyr::select(.data$fishery_id, .data$fishery_name, dplyr::everything()) |>
      print(n = Inf)
  } else {
    cli::cli_alert_success('Non-retention between {run_1_name} and {run_2_name} match')
  }


}
