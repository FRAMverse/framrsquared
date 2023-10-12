#' Vectorized approach to calculating the management week
#' @param date A date column
#' @export
#' @examples
#' \dontrun{
#' data_fram |>
#'   mutate(mngmt_week = management_week(date_field))
#' }
#'
management_week <- function(date) {
  # this wont be slow now
  if(!inherits(date, 'Date')){rlang::abort('This only works on a date object')}
  # test if sunday is first day of year
  if_else(
    lubridate::wday(lubridate::floor_date(date, 'year')) == 1
    ,
    as.integer(strftime(date, '%U'))
    ,
    as.integer(strftime(date, '%U')) + 1
  )
}

#' Vectorized approach to calculating the statistical week
#' @param date A date column
#' @export
#' @examples
#' \dontrun{
#' data_fram |>
#'   mutate(stat_week = statistical_week(date_field))
#' }
#'
statistical_week <- function(date){
  if(!inherits(date, 'Date')){rlang::abort('This only works on a date object')}
  # test if monday is first day of year
  if_else(
    lubridate::wday(lubridate::floor_date(date, 'year')) == 2
    ,
    as.integer(strftime(date, '%W'))
    ,
    as.integer(strftime(date, '%W')) + 1
  )

}
