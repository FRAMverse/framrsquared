#' Provides English translation of numeric non-retention flags
#' @param vec vector of flags
#' @export
#' @examples
#' \dontrun{NR_flag_translate(sample(1:4, 10, replace = T))}

NR_flag_translate = function(vec) {

  validate_numeric(vec)

  if(!all(vec %in% 0:4)){
    cli::cli_abort("input includes flags not matching non-retention flags")
  }

  dplyr::case_match(
    vec,
    0 ~ "ZERO",
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
#' \dontrun{scalers_flag_translate(sample(c(1, 2, 7, 8, 17, 18, 27, 28), 10, replace = T))}
#'
scalers_flag_translate = function(vec) {

  validate_numeric(vec)

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
#' @param species Optional, identifying species if `.data` doesn't. If provided, should be "CHINOOK" or "COHO" (or variants)
#' @param warn Logical, defaults to TRUE. Warn if neither flag column is present in dataframe?
#' @export
#' @examples
#' \dontrun{ mortality_table |> add_flag_text()}
label_flags = function(.data,
                       species = NULL,
                       warn = TRUE) {
  validate_data_frame(.data)
  species = validate_species(.data, species)
  if(!any(c("fishery_flag", "non_retention_flag") %in% names(.data))){
    if(warn){
      cli::cli_alert_warning("Missing 'fishery_flag' or 'non_retention_flag' column in data")
    }
  } else {
    if ("fishery_flag" %in% names(.data)) {
      .data <- .data |>
        dplyr::mutate(fishery_flag_label = scalers_flag_translate(.data$fishery_flag),
                      .after = .data$fishery_flag)
    }
    if("non_retention_flag" %in% names(.data)){
      if(species == "CHINOOK"){
        .data <- .data |>
          dplyr::mutate(non_retention_flag_label = NR_flag_translate(.data$non_retention_flag),
                        .after =.data$non_retention_flag)
      } else {
        .data <- .data |>
          dplyr::mutate(non_retention_flag_label = "Total dead fish",
                        .after =.data$non_retention_flag)
      }
    }
  }
  return(.data)
}

#' NA's all the information in the FisheryScalers that's not being used
#' e.g Flag 1 only NS Scalers will be returned
#' @param .data Dataframe of the Fishery Scalers table
#' @export
#' @examples
#' \dontrun{ fishery_scalers_table |> filter_flag()}
#'
filter_flag <- function(.data){
  validate_data_frame(.data)
  species = attr(.data, "species")
  if(!all(c("fishery_scale_factor", "msf_fishery_scale_factor",
            "quota", "msf_quota") %in% names(.data))){
    cli::cli_abort("Input is not a fishery scaler dataframe.")
  }
  res <- .data |>
    dplyr::group_by(.data$fishery_id, .data$time_step) |>
    dplyr::mutate(
      fishery_scale_factor = dplyr::if_else(.data$fishery_flag %in% c(1,17,18), .data$fishery_scale_factor, NA_real_),
      msf_fishery_scale_factor = dplyr::if_else(.data$fishery_flag %in% c(7,17,27), .data$msf_fishery_scale_factor, NA_real_),
      quota = dplyr::if_else(.data$fishery_flag %in% c(2,27,28), .data$quota, NA_real_),
      msf_quota = dplyr::if_else(.data$fishery_flag %in% c(8,18,28), .data$msf_quota, NA_real_)
    ) |>
    dplyr::ungroup()
  attr(res, "species") <- species
  return(res)
}

#' NA's all the cnr_input_# columns of a non-retention table that are not being used due to the flagging.
#' @param .data Dataframe of the Fishery Scalers table
#' @export
#' @examples
#' \dontrun{ non_retention_table |> filter_nr_flag()}
#'
filter_nr_flag <- function(.data) {
  validate_data_frame(.data)
  species = attr(.data, "species")
  if(!all(c("non_retention_flag", "cnr_input1",
            "cnr_input2", "cnr_input3", "cnr_input4") %in% names(.data))){
    cli::cli_abort("Input is not a non-retention dataframe.")
  }

  if(species == "COHO"){
    res <- .data |>
      dplyr::mutate(cnr_input2 = NA_real_,
                    cnr_input3 = NA_real_,
                    cnr_input4 = NA_real_)
  } else if(species == "CHINOOK"){
    ## CHINOOK
    res <- .data |>
      dplyr::mutate(
        cnr_input1 = dplyr::if_else(.data$non_retention_flag %in% 2:4,
                                     .data$cnr_input1,
                                    NA),
        cnr_input2 = dplyr::if_else(.data$non_retention_flag %in% 2:3,
                                    .data$cnr_input2,
                                    NA),
        cnr_input3 = dplyr::if_else(.data$non_retention_flag %in% 1:2,
                                    .data$cnr_input3,
                                    NA),
        cnr_input4 = dplyr::if_else(.data$non_retention_flag %in% 1:2,
                                    .data$cnr_input4,
                                    NA)
      )
  } else {
    cli::cli_abort("Species must be 'COHO' or 'CHINOOK'!")
  }
  attr(res, "species") <- species
  return(res)
}

# if non_retention_flag is 1, NA cnr_input_1 and cnr_input_2
# if non_retention_flag is 2, leave everything alone
# if non_retention_flag is 3, NA cnr_input_3 and cnr_input_4
# if non_retetnion_flag is 4, NA cnr_input_2, cnr_input_3, and cnr_input_4
