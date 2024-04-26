#' Framrsquared style guide
#'
#' For easy readability, we want to use consistent coding style when developing code for framrsquared.
#' Presently this includes (a) using `<-` for assignment, and (b) using snakecase for
#' variable and function names. The functions here streamlining checking R code for
#' consistency with this style.
#'
#' `frs_stylecheck_assignment()` takes the path to an R file, and prints (and returns)
#' any rows that may be mis-using the `=` for assignment. Note that it will give false positives
#' for arguments defined in function calls if the call spans multiple lines, as well as
#' `=` signs included in character strings.
#'
#' `frs_stylecheck_snakecase` takes the path to an R file
#' and prints (and returns) the names of any variables assigned using `<-` that do not include underscores.
#' This will identify variables that do not use snakecase, but will also give false positive
#' matches for variables that are single words and thus do not need snakecase.
#'
#' @param filepath Path to R file to be checked
#' @param n Number of rows to print. Default is to print all rows, but set to smaller values if output is overwhelming.
#' @name styleguide
NULL
#> NULL

#' @rdname styleguide
#' @export
#' @example
#' frs_stylecheck_assignment("R/copy.R")

frs_stylecheck_assignment = function(filepath, n = Inf){
  cli::cli_text(cli::col_blue(paste("Checking", gsub(".*[/]", "", filepath), "for accidental uses of `=` for assignment")))
  cli::cli_text(cli::col_grey("Note that this is not perfect -- multi-line function calls which (correctly) use `=` for arguments
  will show up here, as will SQL calls and other edge cases."))
  df <- readr::read_lines(filepath) |>
    tibble::as_tibble() |>
    dplyr::rename(line.entry = value)
  df$linenum <- 1:nrow(df)
  df <- df |>
    dplyr::mutate(before.parens = gsub("[(].*", "", line.entry)) |>
    dplyr::filter(stringr::str_detect(before.parens, "[^=]=[^=]")) |>
    dplyr::select(-before.parens)
  if(nrow(df)==0){
    cli::cli_alert_success("No possible cases of accidental assignment using `=`. Good work!")
  }else{
    df |> print(n = n)
  }
}

#' @rdname styleguide
#' @export
frs_stylecheck_snakecase = function(filepath, n = Inf){
  cli::cli_text(cli::col_blue(paste("Checking", gsub(".*[/]", "", filepath), "for variables that are not named using snake_case.")))
  cli::cli_text(cli::col_grey("Note that this will also list single-word variables, which should be fine. Make sure assignment all uses `<- ` (`frs_stylecheck_assignment()` streamlines this)"))
  df <- readr::read_lines(filepath) |>
    tibble::as_tibble() |>
    dplyr::rename(line.entry = value)
  df$linenum <- 1:nrow(df)
  df <- df |>
    dplyr::filter(stringr::str_detect(line.entry, "<-")) |>
    dplyr::mutate(variable.name = stringr::str_trim(gsub("<-.*", "", line.entry)),
           .before = line.entry) |>
    dplyr::filter(!stringr::str_detect(line.entry, "_"))
  if(nrow(df)==0){
    cli::cli_alert_success("No possible cases of variable names not in snakecase. Good work!")
  }else{
    df |> print(n = n)
  }
}
