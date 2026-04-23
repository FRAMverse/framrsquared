#' Vectorized approach to calculating the management week
#'
#' @param date An atomic or vector of dates
#'
#' @returns a numeric vector with same length as argument `date`
#'
#' @export
#' @examples
#' management_week(as.Date(Sys.Date()))
#' \dontrun{
#' data_fram |>
#'   mutate(mngmt_week = management_week(date_field))
#' }
#'
management_week <- function(date) {
  UseMethod('management_week')
}

#' @export
management_week.Date <- function(date) {
  management_week_(date)
}

#' @export
management_week.POSIXct <- function(date) {
  #validate_data_frame(.data)
  # convert posixct to date
  date <- as.Date(date)

  management_week_(date)
}

#' @export
management_week.character <- function(date) {
  #validate_data_frame(.data)
  # try a few common formats
  if(!anyNA(as.Date(date, '%Y-%m-%d'))) {

    date <- as.Date(date, '%Y-%m-%d')

  } else if(!anyNA(as.Date(date, '%m/%d/%Y'))) {

    date <- as.Date(date, '%m/%d/%Y')

  } else {

    fram_abort('Date is in an ambiguous format')

  }

  management_week_(date)
}


management_week_ <- function(date){
  #validate_data_frame(.data)
  dplyr::if_else(
    lubridate::wday(lubridate::floor_date(date, 'year')) == 1
    ,
    as.integer(strftime(date, '%U'))
    ,
    as.integer(strftime(date, '%U')) + 1
  )
}
