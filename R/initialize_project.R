#'  `r lifecycle::badge("experimental")`
#' Initializes a FRAM project
#' @param folders Vector of folders to create
#' @param renv Boolean to initialize renv
#' @param templates Boolean. If TRUE, copy template quarto file and style.css from snippets repo
#' @export
#' @examples
#' \dontrun{
#' framrsquared::initialize_project()
#' }
initialize_project <- function(folders = c('scripts', 'original_data',
                                           'cleaned_data', 'figures',
                                           'results'),
                               templates = FALSE,
                               renv = FALSE){

  purrr::walk(
    folders,
    \(folder) dir.create(here::here(glue::glue("{folder}")))
  )
  cli::cli_alert_success('Successfully initialized FRAM project')

  if (renv){
    cli::cli_alert_info(
      "Initializing {.pkg renv}, don't forget to run {.fn renv::snapshot} before saving project"
    )
    invisible(readline('Press [Enter] to conitue...'))
    renv::init()
  }


  if (templates){
    cli::cli_alert_info(
      "Copying quarto templates from the snippets repo..."
    )
    if(file.exists("scripts/")){
      prefix <-  "scripts/"
    } else {
      prefix <-  ""
    }
    usethis::use_github_file("https://github.com/FRAMverse/snippets/blob/main/R/markdown-and-quarto/custom-yaml-header.Rmd",
                             save_as = paste0(prefix, "quarto-template-header.qmd"))
    usethis::use_github_file("https://github.com/FRAMverse/snippets/blob/main/R/markdown-and-quarto/style.css",
                             save_as = paste0(prefix, "style.css"))

  }


}


