#' Provides English translation of numeric non-retention flags
#' @param vec vector of flags
#' @export
#' @examples
#' \dontrun{NR_flag_translate(sample(1:4, 10, replace = T))}
#'

NR_flag_translate = function(vec) {

  if(!all(is.numeric(vec))){
    cli::cli_abort("flags must be numeric")
  }

  if(!all(vec %in% 1:4)){
    cli::cli_abort("input includes flags not matching non-retention flags")
  }

  dplyr::case_match(
    vec,
    1 ~ "Computed CNR",
    2 ~ "Ratio of CNR Days",
    3 ~ "Legal/Sublegal Encounters",
    4 ~ "Total Encounters"
  )
}

#' Provides English translation of numeric scalers flags
#' @param vec vector of flags
#' @export
#' @examples
#' \dontrun{scaler_flags_translate(sample(c(1, 2, 7, 8, 17, 18, 27, 28), 10, replace = T))}
#'
scalers_flag_translate = function(vec) {
  if(!all(is.numeric(vec))){
    cli::cli_abort("flags must be numeric")
  }

  if(!all(vec %in% c(0, 1, 2, 7, 8, 17, 18, 27, 28))){
    cli::cli_abort("input includes flags not matching non-retention flags")
  }

  dplyr::case_match(
    vec,
    0 ~ "ZERO",
    1 ~ "Fishery Scaler",
    2 ~ "Fishery Quota",
    7 ~ "MSF Scaler",
    8 ~ "MSF Quota",
    17 ~ "Scaler + MSF Scaler",
    18 ~ "Scaler + MSF Quota",
    27 ~ "Quota + MSF Scaler",
    28 ~ "Quota + MSF Quota"
  )
}

#' Adds a column with a text version of flags for either non-retention or fishery scalers
#' @param .data fetched FisheryScalers or NonRetentions
#' @export
#' @examples
#' \dontrun{ mortality_table |> add_flag_text()}
add_flag_text = function(.data) {
  if(!any(c("fishery_flag", "non_retention_flag") %in% names(.data))){
    cli::cli_abort("Missing 'fishery_flag' or 'non_retention_flag' column in data")
  }
  if ("fishery_flag" %in% names(.data)) {
    dplyr::mutate(.data,
                  fishery_flag_text = scalers_flag_translate(.data$fishery_flag)) |>
      dplyr::relocate(.data$fishery_flag_text,
                      .after = .data$fishery_flag)
  } else{
    ##otherwise it's a non_retention_flag
    dplyr::mutate(.data,
                  non_retention_flag_text = NR_flag_translate(.data$non_retention_flag)) |>
      dplyr::relocate(.data$non_retention_flag_text,
                      .after = .data$non_retention_flag)
  }
}
