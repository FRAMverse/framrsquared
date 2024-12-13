#' Check code for common errors
#'
#' Tool to streamline development. Currently this checks for use of filter() without dplyr::. This would call
#' stats::filter(), which is usually not what I intend. As possible, add additional
#' checks for issues that cause problems but do not give an informative error message (or any error message).
#'
#' @inheritParams styleguide
#'
#' @export
#'
#' @examples
#' \dontrun{
#' error_check_code("R/copy.R")
#' }
error_check_code = function(filepath, n = Inf){
  cli::cli_text(cli::col_blue(paste("Checking", gsub(".*[/]", "", filepath), "for accidental use of `filter()` without `dplyr::`")))
  cli::cli_text(cli::col_grey("Unless intended to call `stats::filter()`, these should be changed."))
  df <- readr::read_lines(filepath) |>
    tibble::as_tibble() |>
    dplyr::rename(line.entry = .data$value)
  df$linenum <- 1:nrow(df)
  df <- df |>
    dplyr::filter(stringr::str_detect(.data$line.entry, "[^:][^:]filter")) |>
    print(n = n)
  if(nrow(df)==0){
    cli::cli_alert_success("No uses of `filter()` without refering to dplyr. Good work!")
  }else{
    df |> print(n = n)
  }
}
