#'  `r lifecycle::badge("experimental")`
#' Initializes a FRAM project
#' @param folders Vector of folders to create
#' @param renv Boolean to initialize renv
#' @export
#' @examples
#' \dontrun{
#' framrsquared::initialize_project()
#' }
initialize_project <- function(folders = c('scripts', 'original_data',
                                           'cleaned_data', 'figures',
                                           'results'),
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





}


