#' Provides English translation of numeric non-retention flags
#'
#' Assumes the flags are for a Chinook run, as Coho only have one type of non-retention (dead fish).
#'
#' @param vec numeric vector of non-retention flags (possible values: 0 through 4)
#'
#' @returns Character vector of same length as argument `vec`.
#'
#' @export
#'
#' @examples
#' data <- data.frame(nr_flag = sample(1:4, size = 10, replace = TRUE))
#' data$translation = translate_nr_flag(data$nr_flag)
#' data

translate_nr_flag = function(vec) {

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
#'
#' Works for both Chinook and Coho (they use the same flagging for scalers).
#'
#' @param vec vector of scaler flags (possible values: 1, 2, 7, 8, 17, 18, 27, 28).
#'
#' @returns Character vector of same length as argument `vec`.
#'
#' @export
#'
#' @examples
#' data <- data.frame(scalers_flag = sample(c(1, 2, 7, 8, 17, 18, 27, 28), 10, replace = TRUE))
#' data$translation = translate_scalers_flag(data$scalers_flag)
#' data

translate_scalers_flag = function(vec) {

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

#' NA's unused scalers
#'
#' Turns values in scaler columns (`fishery_scale_factor`, `msf_fishery_scale_factor`, `quota`, and `msf_quota`) into NAs if the `fishery_flag` column indicates they're not being used. e.g if `fishery_flag` is 1, `fishery_scale_factor` value will be left alone, but the `msf_fishery_scale_Factor`, `quota`, and `msf_quota` values will be turned into NAs.
#'
#' @param .data Dataframe of the Fishery Scalers table
#' @export
#'
#' @returns dataframe `.data` but with some values of the scaler columns replaced with NAs.
#'
#' @examples
#'
## generate example data
#' data = data.frame(fishery_flag = c(1, 2, 7, 8, 17, 18, 27, 28),
#'                  fishery_scale_factor = runif(8)*2,
#'                  quota = sample(500:10000, size = 8),
#'                  msf_fishery_scale_factor = runif(8)*2,
#'                  msf_quota = sample(500:10000, size = 8)
#' )
#' ## here's what it looks like before applying the function
#' data
#' ## applying the function:
#' data |>
#'   na_scalers_from_flag()

na_scalers_from_flag <- function(.data){
  validate_data_frame(.data)
  species = attr(.data, "species")
  if(!all(c("fishery_scale_factor", "msf_fishery_scale_factor",
            "quota", "msf_quota") %in% names(.data))){
    cli::cli_abort("Input is not a fishery scaler dataframe.")
  }
  res <- .data |>
    dplyr::mutate(
      fishery_scale_factor = dplyr::if_else(.data$fishery_flag %in% c(1,17,18), .data$fishery_scale_factor, NA_real_),
      msf_fishery_scale_factor = dplyr::if_else(.data$fishery_flag %in% c(7,17,27), .data$msf_fishery_scale_factor, NA_real_),
      quota = dplyr::if_else(.data$fishery_flag %in% c(2,27,28), .data$quota, NA_real_),
      msf_quota = dplyr::if_else(.data$fishery_flag %in% c(8,18,28), .data$msf_quota, NA_real_)
    ) #|>
  attr(res, "species") <- species
  return(res)
}

#' NA's unused CNR input columns.
#'
#' Turns values in `$cnr_input_*` columns of a non-retention table into NAs if the `$non_retention_flag` column indicates they're not being used. For COHO databases, `cnr_input_1`
#'
#' @param .data Dataframe of the Fishery Scalers table
#'
#' @returns dataframe `.data`, with some values of `cnr_input1:cnr_input4` converted to NAs.
#'
#' @export
#' @examples
#' data = data.frame(non_retention_flag = 0:4,
#'                   cnr_input1 = sample(50:150, size = 5),
#'                   cnr_input2 = sample(50:150, size = 5),
#'                   cnr_input3 = sample(50:150, size = 5),
#'                   cnr_input4 = sample(50:150, size = 5)
#' )
#' ## needs a species attribute -- this is automatically applied when using `fetch_table()`
#' attr(data, "species") <- "CHINOOK"
#' ## here's what it looks like before applying the function
#' data
#' ## applying the function:
#' data |>
#'   na_non_retention_from_flag()

na_non_retention_from_flag <- function(.data) {
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
