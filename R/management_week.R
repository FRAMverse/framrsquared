#' Vectorized approach to calculating the management week, returns
#' an integer
#' @param date A column with dates
#' @export
#' @examples
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
  # convert posixct to date
  date <- as.Date(date)

  management_week_(date)
}

#' @export
management_week.character <- function(date) {
  # try a few common formats
  if(!anyNA(as.Date(date, '%Y-%m-%d'))) {

    date <- as.Date(date, '%Y-%m-%d')

  } else if(!anyNA(as.Date(date, '%m/%d/%Y'))) {

    date <- as.Date(date, '%m/%d/%Y')

  } else {

    rlang::abort('Date is in an ambiguous format')

  }

  management_week_(date)
}


management_week_ <- function(date){
  dplyr::if_else(
    lubridate::wday(lubridate::floor_date(date, 'year')) == 1
    ,
    as.integer(strftime(date, '%U'))
    ,
    as.integer(strftime(date, '%U')) + 1
  )
}
