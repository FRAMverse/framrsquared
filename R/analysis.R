#' Generates a dataframe that compares two runs identified by run_id's
#' @param fram_db FRAM database object
#' @param run_ids Vector of run_ids (two)
#' @export
#' @examples
#' \dontrun{fram_db |> compare_inputs(c(100,101))}
#'
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
      reg_change = dplyr::if_else(is.na(.data$reg_change), '', .data$reg_change)
    ) |>
    dplyr::filter(!is.na(.data$percent_diff)) |>
    ggplot2::ggplot(ggplot2::aes(factor(.data$fishery_id), .data$time_step, fill = dplyr::if_else(.data$percent_diff > 0, 'pos', 'neg'), alpha=abs(.data$percent_diff))) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(scales::percent(.data$percent_diff), '\n', .data$reg_change ))) +
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
