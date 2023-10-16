#' Vectorized approach to calculating the statistical week, returns
#' and integer
#' @param date A column with dates
#' @export
#' @examples
#' \dontrun{
#' data_fram |>
#'   mutate(mngmt_week = statistical_week(date_field))
#' }
#'
statistical_week <- function(date) {
  UseMethod('statistical_week')
}

#' @export
statistical_week.Date <- function(date) {
  statistical_week_(date)
}

#' @export
statistical_week.POSIXct <- function(date) {
  # convert posixct to date
  date <- as.Date(date)

  statistical_week_(date)
}

#' @export
statistical_week.character <- function(date) {
  # try a few common formats
  if(!anyNA(as.Date(date, '%Y-%m-%d'))) {

    date <- as.Date(date, '%Y-%m-%d')

  } else if(!anyNA(as.Date(date, '%m/%d/%Y'))) {

    date <- as.Date(date, '%m/%d/%Y')

  } else {

    rlang::abort('Date is in an ambiguous format')

  }

  statistical_week_(date)
}


statistical_week_ <- function(date){
  dplyr::if_else(
    lubridate::wday(lubridate::floor_date(date, 'year')) == 1
    ,
    as.integer(strftime(date, '%W'))
    ,
    as.integer(strftime(date, '%W')) + 1
  )
}
