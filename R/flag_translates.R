#' Provides English translation of numeric non-retention flags
#' @param vec vector of flags
#' @export
#' @examples
#' \dontrun{NR_flag_translate(sample(1:4, 10, replace = T))}

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
#' \dontrun{scalers_flags_translate(sample(c(1, 2, 7, 8, 17, 18, 27, 28), 10, replace = T))}
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
  validate_data_frame(.data)
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

#' NA's all the information in the FisheryScalers that's not being used
#' e.g Flag 1 only NS Scalers will be returned
#' @param .data Fishery Scalers table
#' @export
#' @examples
#' \dontrun{ fishery_scalers_table |> filter_flag()}
#'
filter_flag <- function(.data){
  validate_data_frame(.data)
  if(!all(c("fishery_scale_factor", "msf_fishery_scale_factor",
            "quota", "msf_quota") %in% names(.data))){
    cli::cli_abort("Input is not a fishery scaler dataframe.")
  }
  .data |>
    dplyr::group_by(.data$fishery_id, .data$time_step) |>
    dplyr::mutate(
      fishery_scale_factor = dplyr::if_else(any(.data$fishery_flag %in% c(1,17,18)), .data$fishery_scale_factor, NA_real_),
      msf_fishery_scale_factor = dplyr::if_else(any(.data$fishery_flag %in% c(7,17,27)), .data$msf_fishery_scale_factor, NA_real_),
      quota = dplyr::if_else(any(.data$fishery_flag %in% c(2,27,28)), .data$quota, NA_real_),
      msf_quota = dplyr::if_else(any(.data$fishery_flag %in% c(8,18,28)), .data$msf_quota, NA_real_)
    ) |>
      dplyr::ungroup()
}

