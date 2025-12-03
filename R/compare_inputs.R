#' Generates a dataframe that compares fishery scalers table for two runs identified by run_id's
#' @param fram_db FRAM database object
#' @param run_ids Vector of two run_ids
#' @details
#' Comparisons assume the first run provided is the baseline, and provide relative changes from that.
#' This includes percent changes ($percent.diff)include percent changes (infinite when)
#'
#' @return Data frame of differences. `$percentdiff` = change in quota (comparing the appropriate quotas based on fishery flags),
#'  `$regulation_comparison`  = change in regulation (NS, MSF, NS + MSF). Columns present in the
#'  FisheriesScalers database are included, with `_original` and `_comparison`
#'  suffixes identifying entries associated with the first and second entries of
#'  `run_ids`, respectively.
#'
#' @export
#' @seealso [compare_inputs_chart()]
#' @examples
#' \dontrun{fram_db |> compare_inputs(c(100,101))}
#'
compare_inputs <- function(fram_db, run_ids){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids)
  # abort if do have two run ids
  if(length(run_ids) != 2 | !is.numeric(run_ids)){cli::cli_abort('Two valid run ids must be provided')}
  scalers <- fram_db |>
    fetch_table('FisheryScalers', label = FALSE) |>
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


#' Generate heat map of changed values between two run inputs.
#'
#' Can be a very busy chart if not filtered down. Consider using a filter on the dataframe before piping into `compare_input_chart`.
#'
#' @param .data Dataframe origination from the compare_inputs() function
#' @export
#'
#' @seealso [compare_inputs()]
#'
#' @examples
#' \dontrun{fram_db |> compare_inputs(c(100, 101)) |> compare_inputs_chart()}
compare_inputs_chart <- function(.data){
  validate_data_frame(.data)
  .data |>
    dplyr::mutate(
      percent_diff = dplyr::if_else(is.infinite(.data$percent_diff), 1, .data$percent_diff),
      percent_diff = dplyr::if_else(is.na(.data$percent_diff), 0, .data$percent_diff),
      reg_change = dplyr::if_else(is.na(.data$reg_change), '', .data$reg_change)
    ) |>
    #dplyr::filter(!is.na(.data$percent_diff)) |>
    ggplot2::ggplot(ggplot2::aes(factor(.data$fishery_id), .data$time_step, fill = dplyr::if_else(.data$percent_diff > 0, 'pos', 'neg'))) +
    ggplot2::geom_tile(ggplot2::aes(alpha=abs(.data$percent_diff))) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(scales::percent(round(.data$percent_diff,3)), '\n', .data$reg_change ),
                                    alpha = abs(.data$percent_diff)+.4)) +
    ggplot2::geom_hline(yintercept = (sort(unique(.data$time_step))-.5)[-1],
                        col = 'darkgray')+
    ggplot2::geom_vline(xintercept = sort(unique(as.numeric(factor(.data$fishery_id)))-.5)[-1],
                        col = 'darkgray')+
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'none',
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(
      subtitle = 'FRAM input comparison heatmap',
      y = 'Time-Step',
      x = 'Fishery'
    ) +
    ggplot2::scale_y_continuous(expand = c(0,0))+
    ggplot2::scale_x_discrete(expand = c(0,0))+
    ggplot2::scale_alpha_identity()
}



#' Generates an input summary based on a FisheryScalers dataframe.
#' Probably end up streamlining / revising this.
#' @param .data FisheryFishery scalers dataframe
#' @param run_id Run ID number
#' @examples
#' \dontrun{fishery_scalers_dataframe |> input_summary()}
input_summary_ <- function(.data, run_id){
  validate_data_frame(.data)
  .data |>
    dplyr::filter(
      .data$run_id == .env$run_id
    ) |>
    dplyr::mutate(
      total_quota = dplyr::case_when(
        .data$fishery_flag %in% c(1,2) ~ .data$quota,
        .data$fishery_flag %in% c(7,8) ~ .data$msf_quota,
        .data$fishery_flag %in% c(17,18,27,28) ~ .data$quota + .data$msf_quota),
      regulation = dplyr::case_when(
        .data$fishery_flag %in% c(1,2) ~ 'NS',
        .data$fishery_flag %in% c(7,8) ~ 'MSF',
        .data$fishery_flag %in% c(17,18,27,28) ~ 'NS+MSF'
      )
    ) |>
    dplyr::select(.data$run_id,.data$fishery_id,.data$fishery_flag, .data$time_step, .data$total_quota, .data$regulation)
}



#' Compares the recruit scalers of two runs
#' @param fram_db FRAM database object
#' @param run_ids Two run ids
#' @param tolerance Tolerance for detecting changes
#' @param verbose If `TRUE`, print an update to screen when there are no differences in recruits.
#' @export
#' @seealso [compare_runs()]
#' @examples
#' \dontrun{fram_db |> compare_recruits()}
compare_recruits <- function(fram_db, run_ids, tolerance = .01, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids)

  if (!is.numeric(tolerance) || length(tolerance) != 1) {
    cli::cli_abort('`tolerance` must be a numeric of length 1')
  }

  if(tolerance < 0){
    cli::cli_abort('`tolerance` must be positive')
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    cli::cli_abort('`verbose` must be a logical of length 1')
  }


  runs <- fram_db |>
    fetch_table('RunID', label = FALSE) |>
    dplyr::select(.data$run_id, .data$run_name)


  base_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic

  new_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic



  recruit_scalers <- fram_db |>
    fetch_table('StockRecruit', label = FALSE)

  base_period_recruit <- fram_db |>
    fetch_table('BaseCohort', label = FALSE)

  stocks <- fram_db |>
    fetch_table('Stock', label = FALSE) |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$stock_id, .data$stock_name)

  run_base_period <- fram_db |>
    fetch_table('RunID', label = FALSE) |>
    dplyr::select(.data$run_id, .data$base_period_id)

  # recalc recruit cohort size
  base_recruits <- recruit_scalers |>
    dplyr::inner_join(run_base_period, by = 'run_id') |>
    dplyr::inner_join(base_period_recruit, by = c('base_period_id', 'stock_id', 'age')) |>
    dplyr::mutate(recruit_cohort_size = .data$recruit_scale_factor * .data$base_cohort_size) |>
    dplyr::select(.data$run_id, .data$stock_id, .data$age, .data$recruit_cohort_size)

  recruit_changes <- base_recruits |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    dplyr::inner_join(stocks, by = 'stock_id') |>
    dplyr::inner_join(runs, by = 'run_id') |>
    dplyr::select(-.data$run_id) |>
    tidyr::pivot_wider(names_from = .data$run_name,
                       values_from = .data$recruit_cohort_size,
                       values_fill = NA_real_) |>
    dplyr::filter(abs((!!new_run_name-!!base_run_name) / !!base_run_name) > tolerance) |>
    dplyr::select(.data$stock_id,
                  .data$age,
                  .data$stock_name,
                  !!base_run_name,
                  !!new_run_name)
  if(nrow(recruit_changes)==0 & verbose){cli::cli_text(cli::col_blue("No differences in recruits between these runs"))}
  return(recruit_changes)
}

#' Compares the fishery inputs of two runs
#' @inheritParams compare_recruits
#' @export
#' @seealso [compare_runs()]
#' @examples
#' \dontrun{fram_db |> compare_fishery_inputs(c(55, 56))}
compare_fishery_inputs <- function(fram_db, run_ids, tolerance = .01, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids)

  if (!is.numeric(tolerance) || length(tolerance) != 1) {
    cli::cli_abort('`tolerance` must be a numeric of length 1')
  }

  if(tolerance < 0){
    cli::cli_abort('`tolerance` must be positive')
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    cli::cli_abort('`verbose` must be a logical of length 1')
  }


  fishery_scalers <- fram_db |>
    fetch_table('FisheryScalers', label = FALSE)

  runs <- fram_db |>
    fetch_table('RunID', label = FALSE) |>
    dplyr::select(.data$run_id, .data$run_name)

  fisheries <- fram_db |>
    fetch_table('Fishery', label = FALSE) |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)

  base_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic

  new_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic


  fishery_scaler_compare <- fishery_scalers |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    filter_flag() |>
    dplyr::select(.data$run_id:.data$time_step,
                  .data$fishery_scale_factor:.data$msf_quota)

  fishery_changed = fishery_scaler_compare |>
    dplyr::inner_join(runs, by = 'run_id') |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    dplyr::select(-.data$run_id) |>
    tidyr::pivot_longer(.data$fishery_scale_factor:.data$msf_quota) |>
    tidyr::pivot_wider(
      names_from = .data$run_name,
      values_from = .data$value,
      values_fill = 0
    ) |>
    dplyr::filter(abs((!!new_run_name-!!base_run_name) / !!base_run_name) > .01) |>
    dplyr::select(.data$fishery_id,
                  .data$fishery_name,
                  .data$time_step,
                  .data$name,
                  !!base_run_name,
                  !!new_run_name) |>
    `attr<-`('species', fram_db$fram_db_species) # making accessible to package filters
  if(nrow(fishery_changed)==0 & verbose){cli::cli_text(cli::col_blue("No differences in fishery inputs between these runs"))}
  return(fishery_changed)

}


#' Compares the fishery flags of two runs
#' @inheritParams compare_recruits
#' @export
#' @seealso [compare_runs()]
#' @examples
#' \dontrun{fram_db |> compare_fishery_input_flags(c(55, 56))}
compare_fishery_input_flags <- function(fram_db, run_ids, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids)

 if (!is.logical(verbose) || length(verbose) != 1) {
    cli::cli_abort('`verbose` must be a logical of length 1')
  }


  fishery_scalers <- fram_db |>
    fetch_table('FisheryScalers', label = FALSE) |>
    dplyr::filter(.data$run_id %in% run_ids)

  runs <- fram_db |>
    fetch_table('RunID', label = FALSE) |>
    dplyr::select(.data$run_id, .data$run_name)

  fisheries <- fram_db |>
    fetch_table('Fishery', label = FALSE) |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)


  base_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic

  new_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic


  # flag comparison in fishery scalers
  flags_changed <- fishery_scalers |>
    dplyr::inner_join(runs, by = 'run_id') |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    dplyr::select(
      .data$fishery_id,
      .data$time_step,
      .data$fishery_flag,
      .data$run_name,
      .data$fishery_name
    ) |>
    tidyr::pivot_wider(names_from = .data$run_name, values_from = .data$fishery_flag,
                       values_fill = 0) |>
    dplyr::filter((!!base_run_name != !!new_run_name) |
                    xor(is.na(!!base_run_name), is.na(!!new_run_name))) |>
    dplyr::select(
      .data$fishery_id,
      .data$time_step,
      .data$fishery_name,!!base_run_name,!!new_run_name
    ) |>
    `attr<-`('species', fram_db$fram_db_species) # making accessible to package filters
  if(nrow(flags_changed)==0 & verbose){cli::cli_text(cli::col_blue("No differences in fishery flags between these runs"))}
  return(flags_changed)
}


#' Compares the non retention inputs of two runs
#' @inheritParams compare_recruits
#' @export
#' @seealso [compare_runs()]
#' @examples
#' \dontrun{fram_db |> compare_non_retention_inputs(c(55, 56))}
compare_non_retention_inputs <- function(fram_db, run_ids, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids)
  if (!is.logical(verbose) || length(verbose) != 1) {
    cli::cli_abort('`verbose` must be a logical of length 1')
  }


  runs <- fram_db |>
    fetch_table('RunID', label = FALSE) |>
    dplyr::select(.data$run_id, .data$run_name)

  fisheries <- fram_db |>
    fetch_table('Fishery', label = FALSE) |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)


  # non retention
  non_retention <- fram_db |>
    fetch_table('NonRetention', label = FALSE) |>
    dplyr::select(.data$run_id,
                  .data$fishery_id,
                  .data$time_step,
                  dplyr::starts_with('cnr_input'))

  base_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic

  new_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic


  nonretention_changed = non_retention |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    dplyr::inner_join(runs, by = 'run_id') |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    dplyr::select(-.data$run_id) |>
    tidyr::pivot_longer(dplyr::starts_with('cnr_input')) |> # re rectangle
    tidyr::pivot_wider(names_from = .data$run_name, values_from = .data$value, values_fill = 0) |>
    dplyr::filter(!!base_run_name != !!new_run_name) |>
    dplyr::select(
      .data$fishery_id,
      .data$time_step,
      .data$name,
      .data$fishery_name,
      !!base_run_name,
      !!new_run_name
    ) |>
    `attr<-`('species', fram_db$fram_db_species) # making accessible to package filters
  if(nrow(nonretention_changed)==0 & verbose){cli::cli_text(cli::col_blue("No differences in non retention between these runs"))}
  return(nonretention_changed)
}


#' Compares the non retention flags of two runs
#' @inheritParams compare_recruits
#' @param run_ids Two run ids
#' @export
#' @seealso [compare_runs()]
#' @examples
#' \dontrun{fram_db |> compare_non_retention_inputs(c(55, 56))}
compare_non_retention_input_flags <- function(fram_db, run_ids, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids)
  if (!is.logical(verbose) || length(verbose) != 1) {
    cli::cli_abort('`verbose` must be a logical of length 1')
  }


  runs <- fram_db |>
    fetch_table('RunID', label = FALSE) |>
    dplyr::select(.data$run_id, .data$run_name)

  fisheries <- fram_db |>
    fetch_table('Fishery', label = FALSE) |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)


  # non retention
  non_retention <- fram_db |>
    fetch_table('NonRetention', label = FALSE) |>
    dplyr::select(.data$run_id,
                  .data$fishery_id,
                  .data$time_step,
                  .data$non_retention_flag)

  base_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic

  new_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic

  nonretention_flags_changed <- non_retention |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    dplyr::inner_join(runs, by = 'run_id') |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    dplyr::select(.data$fishery_id, .data$time_step, .data$non_retention_flag , .data$run_name, .data$fishery_name) |>
    tidyr::pivot_wider(names_from = .data$run_name, values_from = .data$non_retention_flag) |>
    dplyr::filter((!!base_run_name != !!new_run_name) |
                    xor(is.na(!!base_run_name), is.na(!!new_run_name))) |>
    dplyr::select(
      .data$fishery_id,
      .data$time_step,
      .data$fishery_name,
      !!base_run_name,
      !!new_run_name
    ) |>
    `attr<-`('species', fram_db$fram_db_species) # making accessible to package filters
  if(nrow(nonretention_flags_changed)==0 & verbose){cli::cli_text(cli::col_blue("No differences in non retention flags between these runs"))}
  return(nonretention_flags_changed)
}


#' Compares the stock fishery rate scalers of two runs
#' @param fram_db FRAM database object
#' @param run_ids Two run ids
#' @export
#' @seealso [compare_runs()]
#' @examples
#' \dontrun{fram_db |> compare_stock_fishery_rate_scalers(c(55, 56))}
compare_stock_fishery_rate_scalers <- function(fram_db, run_ids){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids)

  if(fram_db$fram_db_species == "CHINOOK"){
    cli::cli_abort("Fishery rate scalers are only relevant for Coho, and this is a Chinook database.")
  }

  runs <- fram_db |>
    fetch_table('RunID', label = FALSE) |>
    dplyr::select(.data$run_id, .data$run_name)

  stocks <- fram_db |>
    fetch_table('Stock', label = FALSE) |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$stock_id, .data$stock_name)

  fisheries <- fram_db |>
    fetch_table('Fishery', label = FALSE) |>
    dplyr::filter(.data$species == fram_db$fram_db_species) |>
    dplyr::select(.data$fishery_id, .data$fishery_name)


  # stock fishery rate scalers
  sfrs <- fram_db |>
    fetch_table('StockFisheryRateScaler', label = FALSE) |>
    dplyr::select(.data$run_id,
                  .data$stock_id,
                  .data$fishery_id,
                  .data$time_step,
                  .data$stock_fishery_rate_scaler)

  if(!all(run_ids %in% sfrs$run_id)){
    cli::cli_abort(paste0("One or more runs in `run_ids` is not defined in the StockFisheryRateScaler table. Available runs:\n",
                          paste(sort(unique(sfrs$run_id)), collapse = ", ")))
  }

  base_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic

  new_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_name) |>
    rlang::sym() #... don't ask, R voodoo magic

  sfrs |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    dplyr::inner_join(runs, by = 'run_id') |>
    dplyr::inner_join(stocks, by = 'stock_id') |>
    dplyr::inner_join(fisheries, by = 'fishery_id') |>
    dplyr::select(
      .data$stock_id,
      .data$stock_name,
      .data$fishery_id,
      .data$fishery_name,
      .data$time_step,
      .data$stock_fishery_rate_scaler,
      .data$run_name
    ) |> #View()
    tidyr::pivot_wider(
      names_from = .data$run_name,
      values_from = .data$stock_fishery_rate_scaler
    ) |> #print(n=Inf)
    dplyr::filter((!!base_run_name != !!new_run_name) |
                    xor(is.na(!!base_run_name), is.na(!!new_run_name))) |>
    dplyr::select(.data$stock_id,
                  .data$stock_name,
                  .data$time_step,!!base_run_name,!!new_run_name) |>
    `attr<-`('species', fram_db$fram_db_species) # making accessible to package filters

}


#' Generates a report to the console of changes to inputs between two runs
#' @param fram_db FRAM database object
#' @param run_ids Two run ids. Run names must differ; change in FRAM if necessary.
#' @param tolerance Tolerance of detection, 1 percent default
#' @export
#' @examples
#' \dontrun{fram_db |> compare_runs(c(55, 56))}
compare_runs <- function(fram_db, run_ids, tolerance = .01){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids)
  if (!is.numeric(tolerance) || length(tolerance) != 1) {
    cli::cli_abort('`tolerance` must be a positive numeric of length 1')
  }
  if(tolerance < 0){
    cli::cli_abort('`tolerance` must be positive')
  }


  runs <- fram_db |>
    fetch_table('RunID', label = FALSE)

  base_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_name)

  new_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_name)

  if(base_run_name == new_run_name){
    cli::cli_abort("Both runs named {new_run_name}; function will not work unless runs have different names. Recommend renaming one using {.kbd FRAM} > {.kbd FRAM Utilities} > {.kbd Edit Model Run Details}.")
  }

  base_run_time <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_time_date) |>
    strftime('%Y-%m-%d %r',tz = "UTC")

  new_run_time <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_time_date) |>
    strftime('%Y-%m-%d %r',tz = "UTC")


  cli::cli_h1('Comparing run {base_run_name} to {new_run_name}')
  cli::cli_alert_info('{base_run_name} was run at {base_run_time}, {new_run_name} was run at {new_run_time}')

  # non-retention
  cli::cli_h2('Non-Retention Inputs')

  cli::cli_h3('Checking for changes in non-retention flagging')
  retention_flags <- fram_db |> compare_non_retention_input_flags(run_ids, verbose = FALSE)
  if(nrow(retention_flags) > 0){
    cli::cli_alert_info('Changes detected in non-retention flagging, below is a table outlining them')
    print(retention_flags, n=Inf)
    flags.used <- retention_flags |>
      dplyr::select(-.data$fishery_id, -.data$time_step, -.data$fishery_name) |>
      tibble::deframe() |>
      unique() |>
      sort() |>
      purrr::map_vec(function(x) paste0(x, " = ", NR_flag_translate(x)))
    cli::cli_text(paste0("Flags: ", paste0(flags.used, collapse = ";  ")))
  } else {
    cli::cli_alert_success('No changes detected in non-retention flagging')
  }

  cli::cli_h3('Checking for changes in non-retention inputs')
  retention_inputs <- fram_db |> compare_non_retention_inputs(run_ids, verbose = FALSE)
  if(nrow(retention_inputs) > 0){
    cli::cli_alert_info('Changes detected in non-retention inputs, below is a table outlining them')
    print(retention_inputs, n=Inf)
  } else {
    cli::cli_alert_success('No changes detected in non-retention inputs')
  }

  # recruit scalers
  cli::cli_h2('Recruit Inputs')
  cli::cli_h3('Checking for changes to recruits')

  recruits <- fram_db |> compare_recruits(run_ids, verbose = FALSE)
  if(nrow(recruits) > 0){
    cli::cli_alert_info('Changes detected in recruits inputs, below is a table outlining them')
    print(recruits, n=Inf)
  } else {
    cli::cli_alert_success('No changes detected in recruit inputs')
  }


  # fishery scalers
  cli::cli_h2('Fishery Inputs')
  cli::cli_h3('Checking for changes to fishery flags')

  fishery_flags <- fram_db |> compare_fishery_input_flags(run_ids, verbose = FALSE)
  if(nrow(fishery_flags) > 0){
    cli::cli_alert_info('Changes detected in fishery flag inputs, below is a table outlining them')
    print(fishery_flags, n=Inf)
    flags.used <- fishery_flags |>
      dplyr::select(-.data$fishery_id, -.data$time_step, -.data$fishery_name) |>
      tibble::deframe() |>
      unique() |>
      sort() |>
      purrr::map_vec(function(x) paste0(x, " = ", scalers_flag_translate(x)))
    cli::cli_text(paste0("Flags: ", paste0(flags.used, collapse = ";  ")))
  } else {
    cli::cli_alert_success('No changes detected in fishery flag inputs')
  }

  cli::cli_h3('Checking for changes to fishery inputs')
  cli::cli_alert_info('Detection tolerance set to: {scales::percent(tolerance)}')
  fishery_inputs <- fram_db |> compare_fishery_inputs(run_ids, tolerance = tolerance, verbose = FALSE)
  if(nrow(fishery_inputs) > 0){
    cli::cli_alert_info('Changes detected in fishery inputs, below is a table outlining them')
    print(fishery_inputs, n=Inf)
  } else {
    cli::cli_alert_success('No changes detected in fishery inputs')
  }
  if(fram_db$fram_db_species=="COHO"){
    cli::cli_h3('Checking for changes to stock fishery rate scalers')
    sfrs <- fram_db |> compare_stock_fishery_rate_scalers(run_ids)
    if(nrow(sfrs) > 0){
      cli::cli_alert_info('Changes detected in stock fishery rate scalers, below is a table outlining them')
      print(sfrs, n=Inf)
    } else {
      cli::cli_alert_success('No changes detected in fishery rate scalers')
    }
  }

}





