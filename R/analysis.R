# analysis

compare_inputs <- function(fram_db, run_ids){
  # abort if do have two run ids

  if(length(run_ids) != 2){rlang::abort('Two valid run ids must be provided')}
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
      percent_diff = (total_quota_comparison - total_quota_original) / total_quota_original,
      reg_change = dplyr::case_when(
        regulation_comparison != regulation_original ~ paste0(regulation_original, '->', regulation_comparison),
        total_quota_original == 0 & total_quota_comparison > 0 ~ paste0('NR->', regulation_comparison),
        total_quota_comparison == 0 & total_quota_original > 0 ~ paste0(regulation_original, '->NR')
      )
    )
}

compare_inputs_chart <- function(.data){
  .data |>
    dplyr::mutate(
      percent_diff = dplyr::if_else(is.infinite(percent_diff), 1, percent_diff),
      reg_change = dplyr::if_else(is.na(reg_change), '', reg_change)
    ) |>
    dplyr::filter(!is.na(percent_diff)) |>
    ggplot2::ggplot(ggplot2::aes(factor(fishery_id), time_step, fill = dplyr::if_else(percent_diff > 0, 'pos', 'neg'), alpha=abs(percent_diff))) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(scales::percent(percent_diff), '\n', reg_change ))) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::labs(
      subtitle = 'FRAM input comparison heatmap',
      y = 'Time-Step',
      x = 'Fishery'
    ) +
    ggplot2::scale_alpha_identity()
}

input_summary_ <- function(.data, run_id){
  .data |>
    dplyr::filter(
      .data$run_id == .env$run_id
    ) |>
    dplyr::mutate(
      total_quota = dplyr::case_when(
        fishery_flag %in% c(1,2) ~ quota,
        fishery_flag %in% c(7,8) ~ msf_quota,
        fishery_flag %in% c(17,18,27,28) ~ quota + msf_quota),
      regulation = dplyr::case_when(
        fishery_flag %in% c(1,2) ~ 'NS',
        fishery_flag %in% c(7,8) ~ 'MSF',
        fishery_flag %in% c(17,18,27,28) ~ 'NS+MSF'
      )
    ) |>
    dplyr::select(run_id,fishery_id,fishery_flag, time_step, total_quota, regulation)
}

