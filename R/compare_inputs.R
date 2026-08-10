## helper function to add attributes to comparison dataframes
attach_comparison_attributes <- function(.data, fram_db, run_ids){

  runs <- fram_db |>
    fetch_table_('RunID') |>
    dplyr::select("run_id", "run_name")

  original_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_name)

  comparison_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_name)

  attr(.data, "original_run_id") <- run_ids[1]
  attr(.data, "comparison_run_id") <- run_ids[2]
  attr(.data, "original_run_name") <- original_run_name
  attr(.data, "comparison_run_name") <- comparison_run_name
  attr(.data, "species") <- fram_db$fram_db_species

  return(.data)
}


#' Compare FisheryScalers tables between two runs.
#'
#' Generates a dataframe that compares fishery scalers table for two runs identified by run_id's.
#'
#' @param fram_db FRAM database object
#' @param run_ids Vector of two run_ids
#'
#' @returns Data frame of differences. `*_original` columns show the values in the first run of `run_ids`, while `*_comparison` show the values of the second run of `run_ids`. Quota and MSF quota have been combined into `$total_quota_*`. `$prop_diff` = proportional change in quota (comparing the appropriate quotas based on fishery flags). `$reg_change` = change in regulations.
#'
#' @family comparisons
#' @export
#' @examples
#' \dontrun{fram_db |> compare_inputs(c(100,101))}
#'
compare_inputs <- function(fram_db, run_ids){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids, n = 2)
  if(run_ids[1] == run_ids[2]){
    fram_abort("Run IDs must be different! Currently both values of run_ids are {.val {run_ids[1]}}")
  }
  validate_same_bp(fram_db, run_ids)

  scalers <- fram_db |>
    fetch_table_('FisheryScalers') |>
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
    ) |>
    dplyr::mutate(run_id = dplyr::coalesce(.data$run_id_original, .data$run_id_comparison)) |>
    label_fisheries_db(fram_db = fram_db) |>
    dplyr::select(-"run_id") |>
    attach_comparison_attributes(fram_db = fram_db,
                                 run_ids = run_ids)
}

#' Compare Sublegal Ratio tables between two runs.
#'
#' Provides a dataframe that compares the "SLRatio" table for two runs identified by run_ids.
#' Only works for Chinook databases (Coho do not have an SLRatio table).
#'
#' @inheritParams compare_inputs
#'
#' @returns Data frame of differences. `*_original` columns show the values in the first run of `run_ids`, while `*_comparison` show the values of the second run of `run_ids`. `*_diff` = comparison - original.
#'
#' @family comparisons
#' @export
#' @examples
#' \dontrun{fram_db |> compare_sl_ratio(c(100,101))}

compare_sl_ratio <- function(fram_db, run_ids){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids, n = 2)
  if(run_ids[1] == run_ids[2]){
    fram_abort("Run IDs must be different! Currently both values of run_ids are {.val {run_ids[1]}}")
  }
  validate_same_bp(fram_db, run_ids)
  if(fram_db$fram_db_species != "CHINOOK"){fram_abort('Database must be a Chinook database.')}
  # abort if do have two run ids

  sl_ratio <- fram_db |>
    fetch_table_("SLRatio")

  original <- sl_ratio |>
    dplyr::filter(.data$run_id == .env$run_ids[1]) |>
    dplyr::select("run_id", "fishery_id", "age", "time_step",
                  "target_ratio", "run_encounter_rate_adjustment")


  comparison <- sl_ratio |>
    dplyr::filter(.data$run_id == .env$run_ids[2])|>
    dplyr::select("run_id", "fishery_id", "age", "time_step",
                  "target_ratio", "run_encounter_rate_adjustment")

  sl_ratio_changes <- original |>
    dplyr::full_join(comparison,
                     by = c("fishery_id", "age", "time_step"), suffix = c('_original', '_comparison')) |>
    dplyr::mutate(
      target_ratio_diff = .data$target_ratio_comparison - .data$target_ratio_original,
      run_encounter_rate_adjustment_diff = .data$run_encounter_rate_adjustment_comparison - .data$run_encounter_rate_adjustment_original,
      na_mismatch = is.na(.data$target_ratio_original) != is.na(.data$target_ratio_comparison) |
        is.na(.data$run_encounter_rate_adjustment_original) !=
        is.na(.data$run_encounter_rate_adjustment_comparison)) |>
    dplyr::filter(.data$target_ratio_diff != 0 |
                    .data$run_encounter_rate_adjustment_diff != 0 |
                    .data$na_mismatch
    ) |>
    dplyr::mutate(run_id = dplyr::coalesce(.data$run_id_original, .data$run_id_comparison)) |>
    label_fisheries_db(fram_db = fram_db) |>
    dplyr::select(-"na_mismatch", -"run_id", -"run_id_original", -"run_id_comparison") |>
    attach_comparison_attributes(fram_db, run_ids = run_ids)

  return(sl_ratio_changes)
}


#' Generate heat map of changed values between two run inputs.
#'
#' Can be a very busy chart if not filtered down. Consider using a `filter_*()` function on the dataframe before piping into `compare_input_chart`.
#'
#' @param .data Dataframe origination from the compare_inputs() function
#' @export
#'
#' @family comparisons
#'
#' @returns ggplot object with heatmap of changes in inputs.
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
#' @keywords internal
#' @return tibble with `$run_id`, `$fishery_id`, `$fishery_flag`, `$time_step`, `$total_quota` (sum of quota and msf_quota), and `regulation` ("NS", "MSF", "NS+MSF", or NA)
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
        .data$fishery_flag == 0 ~ 'none',
        .data$fishery_flag %in% c(1,2) ~ 'NS',
        .data$fishery_flag %in% c(7,8) ~ 'MSF',
        .data$fishery_flag %in% c(17,18,27,28) ~ 'NS+MSF'
      )
    ) |>
    dplyr::select(
      "run_id",
      "fishery_id",
      "fishery_flag",
      "time_step",
      "total_quota",
      "regulation"
    )
}

#' Compares the recruit scalers of two runs
#'
#' @inheritParams compare_inputs
#' @param tolerance Minimum % change needed to flag a difference. Set to 0 to flag any changes at all. Defaults to 0.01.
#' @param verbose If `TRUE`, print an update to screen when there are no differences in recruits.
#'
#' @export
#' @family comparisons
#'
#' @returns tibble with `$stock_id`, `$age`, and `$stock_name` identifying stock x age combinations in which the recruit cohort sizes changed by at least `tolerance x 100`%. `recruit_cohort_original` and `$..._comparison` give the recruit cohort for the first and second run_ids provided. These are calculated directly from "StockRecruit" column RecruitScaleFactor and the "BaseCohort" table, as the RecruitCohort column of the "StockRecruit" table can be misleading. `$prop_diff` gives the proportional change from the original to comparison runs (ie 0.16  = 16% increase).
#'
#' @examples
#' \dontrun{fram_db |> compare_recruits()}
compare_recruits <- function(fram_db, run_ids, tolerance = .01, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids, n = 2)
  if(run_ids[1] == run_ids[2]){
    fram_abort("Run IDs must be different! Currently both values of run_ids are {.val {run_ids[1]}}")
  }
  validate_same_bp(fram_db, run_ids)
  validate_numeric(tolerance, n = 1)
  validate_flag(verbose)

  if(tolerance < 0 | tolerance > 1){
    fram_abort('`tolerance` must be a numeric between 0 and 1.')
  }

  runs <- fram_db |>
    fetch_table_('RunID') |>
    dplyr::select("run_id", "run_name")

  runs_lut = data.frame(run_id = run_ids,
                        run_identifier = c("original", "comparison"))

  recruit_scalers <- fram_db |>
    fetch_table_('StockRecruit')

  base_period_recruit <- fram_db |>
    fetch_table_('BaseCohort')

  run_base_period <- fram_db |>
    fetch_table_('RunID') |>
    dplyr::select("run_id", "base_period_id")

  # recalc recruit cohort size
  base_recruits <- recruit_scalers |>
    dplyr::inner_join(run_base_period, by = 'run_id') |>
    dplyr::inner_join(base_period_recruit, by = c('base_period_id', 'stock_id', 'age')) |>
    dplyr::mutate(recruit_cohort_size = .data$recruit_scale_factor * .data$base_cohort_size) |>
    dplyr::select("run_id", "stock_id", "age", "recruit_cohort_size")

  recruit_changes <- base_recruits |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    label_stocks_db(fram_db = fram_db) |>
    dplyr::inner_join(runs_lut, by = "run_id") |>
    dplyr::select(-"run_id") |>
    tidyr::pivot_wider(names_from = "run_identifier",
                       values_from = "recruit_cohort_size",
                       names_prefix = "recruit_cohort_",
                       values_fill = NA_real_) |>
    dplyr::mutate(prop_diff = (dplyr::coalesce(.data$recruit_cohort_comparison, 0) - dplyr::coalesce(.data$recruit_cohort_original, 0)) / dplyr::coalesce(.data$recruit_cohort_original, 0)) |>
    dplyr::filter(abs(.data$prop_diff) > .env$tolerance) |>
    dplyr::select("stock_id",
                  "age",
                  "stock_label",
                  "recruit_cohort_original",
                  "recruit_cohort_comparison",
                  "prop_diff") |>
    attach_comparison_attributes(fram_db, run_ids = run_ids)

  if(nrow(recruit_changes)==0 & verbose){cli::cli_text(cli::col_blue("No differences in recruits between these runs"))}

  return(recruit_changes)
}

#' Compares the fishery inputs of two runs
#' @inheritParams compare_recruits
#'
#' @returns All fishery x timesteps in which the fishery inputs changed by at least (`tolerance` x 100) % between the specified runs.
#' `$fishery_id`, `$fishery_label`, and `$timestep` identify the fishery x timestep, `$parameter` identifies which parameter changed (.e.g, quota, msf_quota, etc.). `$original` and `$comparison` show the values from the first and second runs, respectively. `$prop_diff` shows the proportional change from the first to second run (e.g., 0.16 = 16% increase).
#'
#' @export
#' @family comparisons
#'
#' @examples
#' \dontrun{fram_db |> compare_fishery_inputs(c(55, 56))}
compare_fishery_inputs <- function(fram_db, run_ids, tolerance = .01, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids, n = 2)
  if(run_ids[1] == run_ids[2]){
    fram_abort("Run IDs must be different! Currently both values of run_ids are {.val {run_ids[1]}}")
  }
  validate_same_bp(fram_db, run_ids)
  validate_numeric(tolerance, n = 1)
  if(tolerance < 0 | tolerance > 1){
    fram_abort('`tolerance` must be a numeric with a value from 0 to 1')
  }
  validate_flag(verbose)

  fishery_scalers <- fram_db |>
    fetch_table_('FisheryScalers')

  runs <- fram_db |>
    fetch_table_('RunID') |>
    dplyr::select("run_id", "run_name")

  runs_lut <- data.frame(run_id = run_ids,
                         run_label = c("original", "comparison"))

  fishery_scaler_compare <- fishery_scalers |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    # na_scalers_from_flag() |>
    dplyr::select("run_id":"time_step",
                  "fishery_scale_factor":"msf_quota")

  fishery_changed = fishery_scaler_compare |>
    dplyr::inner_join(runs_lut, by = 'run_id') |>
    label_fisheries_db(fram_db = fram_db) |>
    dplyr::select(-"run_id") |>
    tidyr::pivot_longer("fishery_scale_factor":"msf_quota",
                        names_to = "parameter") |>
    tidyr::pivot_wider(
      names_from = "run_label",
      values_from = "value",
      values_fill = 0
    ) |>
    dplyr::mutate(prop_diff = (dplyr::coalesce(.data$comparison, 0) - dplyr::coalesce(.data$original, 0)) /
                    dplyr::coalesce(.data$original, 0)) |>
    dplyr::filter(abs(.data$prop_diff) > .env$tolerance) |>
    dplyr::select("fishery_id",
                  "fishery_label",
                  "time_step",
                  "parameter",
                  "original",
                  "comparison",
                  "prop_diff") |>
    attach_comparison_attributes(fram_db, run_ids = run_ids)
  if(nrow(fishery_changed)==0 & verbose){cli::cli_text(cli::col_blue("No differences in fishery inputs between these runs"))}
  return(fishery_changed)

}


#' Compares the fishery flags of two runs
#' @inheritParams compare_recruits
#'
#' @returns Tibble with all fishery x timesteps that changed fishery input flags. `$fishery_id`, `$fishery_label`, and `$time_step`  identify the fishery x timestep combination, and `$flag_original` and `$flag_comparison` show the flags in the first and second runs, respectively.
#'
#' @export
#' @family comparisons
#'
#' @examples
#' \dontrun{fram_db |> compare_fishery_input_flags(c(55, 56))}
compare_fishery_input_flags <- function(fram_db, run_ids, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids, n = 2)
  if(run_ids[1] == run_ids[2]){
    fram_abort("Run IDs must be different! Currently both values of run_ids are {.val {run_ids[1]}}")
  }
  validate_same_bp(fram_db, run_ids)
  validate_flag(verbose)

  fishery_scalers <- fram_db |>
    fetch_table_('FisheryScalers') |>
    dplyr::filter(.data$run_id %in% run_ids)

  runs_lut <- data.frame(run_id = run_ids,
                         run_label = c("original", "comparison"))

  # flag comparison in fishery scalers
  flags_changed <- fishery_scalers |>
    dplyr::inner_join(runs_lut, by = 'run_id') |>
    label_fisheries_db(fram_db = fram_db) |>
    # dplyr::inner_join(fisheries, by = 'fishery_id') |>
    dplyr::select(
      "fishery_id",
      "time_step",
      "fishery_flag",
      "run_label",
      "fishery_label"
    ) |>
    tidyr::pivot_wider(names_from = "run_label", values_from = "fishery_flag",
                       values_fill = 0,
                       names_prefix = "flag_") |>
    dplyr::filter((.data$flag_original != .data$flag_comparison) |
                    xor(is.na(.data$flag_original), is.na(.data$flag_comparison))) |>
    dplyr::select(
      "fishery_id",
      "fishery_label",
      "time_step",
      "flag_original",
      "flag_comparison"
    ) |>
    attach_comparison_attributes(fram_db = fram_db, run_ids = run_ids)

  parms_used <- fishery_scalers |>
    na_scalers_from_flag() |>
    dplyr::select(-"fishery_flag") |>
    tidyr::pivot_longer(cols = "fishery_scale_factor":"msf_quota") |>
    dplyr::left_join(runs_lut, by = "run_id") |>
    dplyr::mutate(name = paste0("used_", .data$name, "_", .data$run_label)) |>
    dplyr::select(-"run_label", -'run_id') |>
    tidyr::pivot_wider(names_from = "name", values_from = "value")

  if(nrow(flags_changed)==0 & verbose){cli::cli_text(cli::col_blue("No differences in fishery flags between these runs"))}

  flags_changed <- dplyr::left_join(flags_changed, parms_used, by = c("fishery_id", "time_step"))

  return(flags_changed)
}


#' Compares the non retention inputs of two runs
#' @inheritParams compare_recruits
#'
#' @returns Tibble with all non-retention parameters that changed between the first and second runs. `$fishery_id`, `$fishery_label`, and `$time_step` identify the fishery x timestep that changed, `$parameter` identifies the parameter, and `$original` and `$comparison` present the values in the first and second runs, respectively.
#'
#' @export
#' @family comparisons
#'
#' @examples
#' \dontrun{fram_db |> compare_non_retention_inputs(c(55, 56))}
compare_non_retention_inputs <- function(fram_db, run_ids, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids, n = 2)
  if(run_ids[1] == run_ids[2]){
    fram_abort("Run IDs must be different! Currently both values of run_ids are {.val {run_ids[1]}}")
  }
  validate_same_bp(fram_db, run_ids)
  validate_flag(verbose)

  runs_lut <- data.frame(
    run_id = run_ids,
    run_label = c("original", "comparison")
  )

  non_retention <- fram_db |>
    fetch_table_('NonRetention') |>
    dplyr::select("run_id",
                  "fishery_id",
                  "time_step",
                  dplyr::starts_with('cnr_input'))

  nonretention_changed = non_retention |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    dplyr::inner_join(runs_lut, by = 'run_id') |>
    label_fisheries_db(fram_db = fram_db) |>
    dplyr::select(-"run_id") |>
    tidyr::pivot_longer(dplyr::starts_with('cnr_input'),
                        names_to = "parameter") |> # re rectangle
    tidyr::pivot_wider(names_from = "run_label", values_from = "value", values_fill = 0) |>
    dplyr::filter(.data$original != .data$comparison) |>
    dplyr::select(
      "fishery_id",
      "fishery_label",
      "time_step",
      "parameter",
      "original",
      "comparison"
    ) |>
    attach_comparison_attributes(fram_db = fram_db,
                                 run_ids = run_ids)

  if(nrow(nonretention_changed)==0 & verbose){cli::cli_text(cli::col_blue("No differences in non retention between these runs"))}

  return(nonretention_changed)
}


#' Compares the non retention flags of two runs
#' @inheritParams compare_recruits
#' @param run_ids Two run ids
#' @export
#' @seealso [compare_runs()]
#' @examples
#' \dontrun{fram_db |> compare_non_retention_input_flags(c(55, 56))}
compare_non_retention_input_flags <- function(fram_db, run_ids, verbose = TRUE){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids, n = 2)
  if(run_ids[1] == run_ids[2]){
    fram_abort("Run IDs must be different! Currently both values of run_ids are {.val {run_ids[1]}}")
  }
  validate_same_bp(fram_db, run_ids)
  validate_flag(verbose)

  runs_lut <- data.frame(
    run_id = run_ids,
    run_label = c("original", "comparison")
  )

  non_retention <- fram_db |>
    fetch_table_('NonRetention') |>
    dplyr::select("run_id",
                  "fishery_id",
                  "time_step",
                  "non_retention_flag")

  nonretention_flags_changed <- non_retention |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    dplyr::inner_join(runs_lut, by = 'run_id') |>
    label_fisheries_db(fram_db = fram_db) |>
    dplyr::select("fishery_id",
                  "fishery_label",
                  "time_step",
                  "non_retention_flag",
                  "run_label") |>
    tidyr::pivot_wider(names_from = "run_label", values_from = "non_retention_flag",
                       names_prefix = "flag_") |>
    dplyr::filter((.data$flag_original != .data$flag_comparison) |
                    xor(is.na(.data$flag_original), is.na(.data$flag_comparison))) |>
    dplyr::select(
      "fishery_id",
      "fishery_label",
      "time_step",
      "flag_original",
      "flag_comparison"
    )  |>
    attach_comparison_attributes(fram_db = fram_db, run_ids = run_ids)

  if(nrow(nonretention_flags_changed)==0 & verbose){cli::cli_text(cli::col_blue("No differences in non retention flags between these runs"))}
  return(nonretention_flags_changed)
}


#' Compares the stock fishery rate scalers of two runs
#'
#' Only relevant for Coho runs.
#'
#' @param fram_db FRAM database object
#' @param run_ids Two run ids
#'
#' @returns Tibble of any stock x fishery x timesteps in which the Stock Fishery Rate Scalers (SFRS) changed. `$stock_id`, `$stock_label`, `$fishery_id`, `$fishery_label`, and `$time_step` identify the stock x fishery x timestep, and `$sfrs_original` and `$sfrs_comparison` list the SFRS values in the first and second runs, respectively.
#'
#' @export
#' @family comparisons
#' @examples
#' \dontrun{fram_db |> compare_stock_fishery_rate_scalers(c(55, 56))}
compare_stock_fishery_rate_scalers <- function(fram_db, run_ids){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids, n = 2)
  if(run_ids[1] == run_ids[2]){
    fram_abort("Run IDs must be different! Currently both values of run_ids are {.val {run_ids[1]}}")
  }
  validate_same_bp(fram_db, run_ids)

  if(fram_db$fram_db_species == "CHINOOK"){
    fram_abort("Fishery rate scalers are only relevant for Coho, and this is a Chinook database.")
  }

  runs_lut <- data.frame(
    run_id = run_ids,
    run_label = c("original", "comparison")
  )

  # stock fishery rate scalers
  sfrs <- fram_db |>
    fetch_table_('StockFisheryRateScaler') |>
    dplyr::select("run_id",
                  "stock_id",
                  "fishery_id",
                  "time_step",
                  "stock_fishery_rate_scaler")

  if(!all(run_ids %in% sfrs$run_id)){
    fram_abort(paste0("One or more runs in `run_ids` is not defined in the StockFisheryRateScaler table. Available runs:\n",
                          paste(sort(unique(sfrs$run_id)), collapse = ", ")))
  }

  sfrs |>
    dplyr::inner_join(runs_lut, by = 'run_id') |>
    label_fisheries_db(fram_db = fram_db) |>
    label_stocks_db(fram_db = fram_db) |>
    dplyr::select(
      "stock_id",
      "stock_label",
      "fishery_id",
      "fishery_label",
      "time_step",
      "stock_fishery_rate_scaler",
      "run_label"
    ) |> #View()
    tidyr::pivot_wider(
      names_from = "run_label",
      values_from = "stock_fishery_rate_scaler",
      names_prefix = "sfrs_"
    ) |> #print(n=Inf)
    dplyr::filter((.data$sfrs_original != .data$sfrs_comparison) |
                    xor(is.na(.data$sfrs_original), is.na(.data$sfrs_comparison))) |>
    dplyr::select("stock_id",
                  "stock_label",
                  "fishery_id",
                  "fishery_label",
                  "time_step",
                  "sfrs_original",
                  "sfrs_comparison")  |>
    attach_comparison_attributes(fram_db = fram_db, run_ids = run_ids)

}

#' Compare key aspects of two FRAM runs
#'
#' Outputs summary of comparisons to the console (or optionally to a text file instead). Summary includes output of `compare_non_retention_flags()`, `compare_non_retention_inputs()`, `compare_sl_ratio()` (Chinook only), `compare_recruits()`, `compare_fishery_input_flags()`, `compare_fishery_inputs()`, and `compare_stock_fishery_rate_scalers()` (Coho only).
#'
#' @param fram_db FRAM database object
#' @param run_ids Two run ids. Run names must differ; change in FRAM if necessary.
#' @param save_file If provided, diagnostics text is sent to file instead of console. If file already exists, will overwrite. Character, defaults to NULL.
#' @param tolerance Minimum proportional change to flag as a "difference" for relevant comparisons (comparison of recruits, fishery inputs). Numeric, defaults to 0.01 (e.g., 1% change).
#'
#' @export
#' @family comparisons
#'
#' @returns invisibly returns a list of the comparison dataframes: `$retention_flags`, `$retention_inputs`, `$sl_ratio`, `$recruits`, `fishery_flags`, `$fishery_inputs`, `$sfrs`
#'
#' @examples
#' \dontrun{fram_db |> compare_runs(c(55, 56))}
compare_runs <- function(fram_db, run_ids, save_file = NULL, tolerance = 0.01){

  if(!is.null(save_file)){

    validate_character(save_file)
    cat("", file = save_file, append = FALSE)
    out_con <- file(save_file, open = "a")  # normal console output
    msg_con <- file(save_file, open = "a")  # messages/warnings

    # redirect console to text
    sink(out_con, type = "output")
    sink(msg_con, type = "message")

    on.exit({
      sink(file = NULL, type = "output")
      sink(file = NULL, type = "message")
      close(out_con)
      close(msg_con)
    }, add = TRUE)
  }

  out <- compare_runs_(fram_db = fram_db,
                       run_ids = run_ids,
                       tolerance = tolerance)

  return(invisible(out))
}


# internal function for compare_runs()
#' @keywords internal
compare_runs_ <- function(fram_db, run_ids, tolerance = .01){
  validate_fram_db(fram_db)
  validate_run_id(fram_db, run_ids, n = 2)
  if(run_ids[1] == run_ids[2]){
    fram_abort("Run IDs must be different! Currently both values of run_ids are {.val {run_ids[1]}}")
  }
  validate_same_bp(fram_db, run_ids)
  validate_numeric(tolerance, n = 1)
  if(tolerance < 0 | tolerance > 1){
    fram_abort('`tolerance` must be a numeric between 0 and 1')
  }


  runs <- fram_db |>
    fetch_table_('RunID')

  base_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_name)

  new_run_name <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_name)



  base_run_time <- runs |>
    dplyr::filter(.data$run_id == run_ids[[1]]) |>
    dplyr::pull(.data$run_time_date) |>
    strftime('%Y-%m-%d %r',tz = "UTC")

  new_run_time <- runs |>
    dplyr::filter(.data$run_id == run_ids[[2]]) |>
    dplyr::pull(.data$run_time_date) |>
    strftime('%Y-%m-%d %r',tz = "UTC")

  bp_id <- runs |>
    dplyr::filter(.data$run_id %in% run_ids) |>
    dplyr::pull(.data$base_period_id)

  ## for testing case where runids are same run
  if(length(bp_id) == 1){
    bp_id = c(bp_id, bp_id)
  }

  bp_lut <- fram_db |>
    fetch_table_("BaseID")
  bp_names = c(
    bp_lut[bp_lut$base_period_id == bp_id[1], "base_period_name"],
    bp_lut[bp_lut$base_period_id == bp_id[2], "base_period_name"]
  )

  if(diff(bp_id) != 0){
    cli::cli_alert_warning(cli::col_red("These runs have different base periods ({bp_id[1]} vs {bp_id[2]}})!!!"))
  }


  cli::cli_h1('Comparing run {.val {base_run_name}} (run_id = {.val {run_ids[1]}}) to {.val {new_run_name}} (run_id = {.val {run_ids[2]}})')
  cli::cli_alert_info('{.val {base_run_name}} was run at {.val {base_run_time}} using Base Period "{.val {bp_names[1]}}"')
  cli::cli_alert_info('{.val {new_run_name}} was run at {.val {new_run_time}} using Base Period "{.val {bp_names[2]}}"')

  # cli::cli_h2('Run Integrity')
  #
  #   cli::cli_h3('Checking inputs of "{base_run_name}" against base period')
  #
  #   fram_db |>
  #     check_bp_coverage(run_ids[1])
  #
  #   cli::cli_h3('Checking inputs of "{new_run_name}" against base period')
  #
  #   fram_db |>
  #     check_bp_coverage(run_ids[2])

  # non-retention
  cli::cli_h2('Non-Retention Inputs')

  cli::cli_h3('Checking for changes in non-retention flagging')
  retention_flags <- fram_db |> compare_non_retention_input_flags(run_ids, verbose = FALSE)
  if(nrow(retention_flags) > 0){
    cli::cli_alert_info('Changes detected in non-retention flagging, below is a table outlining them')
    # print(retention_flags, n=Inf)
    cli::cat_print(retention_flags)
    flags.used <- retention_flags |>
      dplyr::select(-"fishery_id",
                    -"time_step",
                    -"fishery_label") |>
      tibble::deframe() |>
      unique() |>
      sort() |>
      purrr::map_vec(function(x) paste0(x, " = ", translate_nr_flag(x)))
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

  # sl_ratios
  if(fram_db$fram_db_species == "CHINOOK"){
    cli::cli_h2('Sublegal Ratios')
    cli::cli_h3('Checking for changes to SLRatio')

    sl_ratio <- fram_db |>
      compare_sl_ratio(run_ids)
    if(nrow(sl_ratio) > 0){
      cli::cli_alert_info('Changes detected in SL Ratios, below is a table outlining them')
      # print(sl_ratio, n=Inf)
      cli::cat_print(sl_ratio)
    } else {
      cli::cli_alert_success('No changes detected in SL Ratios')
    }
  } else {
    sl_ratio = NULL
  }

  # recruit scalers
  cli::cli_h2('Recruit Inputs')
  cli::cli_h3('Checking for changes to recruits')

  recruits <- fram_db |> compare_recruits(run_ids, tolerance = tolerance, verbose = FALSE)
  if(nrow(recruits) > 0){
    cli::cli_alert_info('Changes detected in recruits inputs, below is a table outlining them')
    # print(recruits, n=Inf)
    cli::cat_print(recruits)
  } else {
    cli::cli_alert_success('No changes detected in recruit inputs')
  }


  # fishery scalers
  cli::cli_h2('Fishery Inputs')
  cli::cli_h3('Checking for changes to fishery flags')

  fishery_flags <- fram_db |> compare_fishery_input_flags(run_ids, verbose = FALSE)
  if(nrow(fishery_flags) > 0){
    cli::cli_alert_info('Changes detected in fishery flag inputs, below is a table outlining them')
    # print(fishery_flags |> dplyr::select(-dplyr::starts_with("used_")), n=Inf)
    cli::cat_print(fishery_flags |>
                     dplyr::select(-dplyr::starts_with("used_")))
    flags_used = c(fishery_flags$flag_comparison, fishery_flags$flag_original) |>
      unique() |>
      purrr::map_vec(function(x) paste0(x, " = ", translate_scalers_flag(x)))
    cli::cli_text(paste0("Flags: ", paste0(flags_used, collapse = ";  ")))
  } else {
    cli::cli_alert_success('No changes detected in fishery flag inputs')
  }

  cli::cli_h3('Checking for changes to fishery inputs')
  cli::cli_alert_info('Detection tolerance set to: {scales::percent(tolerance)}')
  fishery_inputs <- fram_db |> compare_fishery_inputs(run_ids, tolerance = tolerance, verbose = FALSE)
  if(nrow(fishery_inputs) > 0){
    cli::cli_alert_info('Changes detected in fishery inputs, below is a table outlining them')
    # print(fishery_inputs, n=Inf)
    cli::cat_print(fishery_inputs)
  } else {
    cli::cli_alert_success('No changes detected in fishery inputs')
  }
  if(fram_db$fram_db_species=="COHO"){
    cli::cli_h3('Checking for changes to stock fishery rate scalers')
    sfrs <- fram_db |> compare_stock_fishery_rate_scalers(run_ids)
    if(nrow(sfrs) > 0){
      cli::cli_alert_info('Changes detected in stock fishery rate scalers, below is a table outlining them')
      # print(sfrs, n=Inf)
      cli::cat_print(sfrs)
    } else {
      cli::cli_alert_success('No changes detected in fishery rate scalers')
    }
  } else {
    sfrs = NULL
  }

  all_comparisons = list(
    retention_flags = retention_flags,
    retention_inputs = retention_inputs,
    sl_ratio = sl_ratio,
    recruits = recruits,
    fishery_flags = fishery_flags,
    fishery_inputs = fishery_inputs,
    sfrs = sfrs
  )

  return(invisible(all_comparisons))
}





