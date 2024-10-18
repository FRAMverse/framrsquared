#'  `r lifecycle::badge("experimental")`
#' Initializes a FRAM project
#'
#' By default, creates suggested folder structure from [best coding practices](https://framverse.github.io/coding-practices/), and
#' adds WDFW-style yaml and style.css files to give quarto files consistent appearance. If you
#' belong to another organization and want this function to support your own organization-specific quarto styling,
#' reach out to the developers with a `_quarto.yml` and (optionally) `style.css` file.
#'
#' @details
#' Dev note: new template files for additional organizations should be added to `inst/`
#' in a subfolder matching an R-friendly organization name,
#' and the same name should be added to the `organization` parameter description here and the `supported_organizations`
#' in `fetch_quarto_templates()`.
#'
#'
#' @param folders Vector of folders to create
#' @param quarto Boolean. If TRUE, add quarto yaml file and style.css
#' @param organization Character, defaults to "WDFW". Specifies the set of quarto templates to use. Currently only supports "WDFW".
#' @param renv Boolean to initialize renv. Defaults to FALSE.
#' @param template.overwrite Boolean. Overwrite _quarto.yml and style.css files if they already exist? Defaults to TRUE
#' @param quiet Boolean, defaults to FALSE. If TRUE, suppresses informational messages.
#' @export
#' @examples
#' \dontrun{
#' framrsquared::initialize_project()
#' }
initialize_project <-
  function(folders = c(
    'scripts',
    'original_data',
    'cleaned_data',
    'figures',
    'results',
    'results/quarto_output'
  ),
  quarto = TRUE,
  organization = "WDFW",
  renv = FALSE,
  template.overwrite = TRUE,
  quiet = TRUE) {
    organization  <- rlang::arg_match(organization, c("WDFW"))

    purrr::walk(folders,
                \(folder) dir.create(here::here(glue::glue("{folder}"))))
    cli::cli_alert_success('Successfully initialized FRAM project')

    if (renv) {
      if (!quiet) {
        cli::cli_alert_info(
          "Initializing {.pkg renv}, don't forget to run {.fn renv::snapshot} before saving project"
        )
      }
      invisible(readline('Press [Enter] to conitue...'))
      renv::init()
    }


    if (quarto) {
      if (!quiet) {
        cli::cli_alert_info("Copying quarto templates")
      }
      fetch_quarto_templates(to.path = ".",
                             organization = organization,
                             overwrite = template.overwrite)

    }
    if (!quiet) {
      cli::cli_bullets(
        c(
          "v" = "Quarto template files added.",
          "i" = "Quarto documents saved in root project directory will now use {organization} template formatting, as specified in `_quarto.yml` and `style.css` files.",
          "i" = "You must still include a YAML header in said quarto documents, which can contain any desired YAML arguments (and likely should include title and author).",
          "i" = "By default, quarto documents using the template will be rendered in `results/quarto_output/`"
        )
      )
    }


  }


#' Creates quarto template files
#'
#' Creates template files from specified organization in the specified path. Generally
#' `initialize_project()` will be more useful for new R projects, while `fetch_quarto_templates()`
#' can be helpful when working with existing projects. See `initialize_project()`
#' for details on adding template files for new organizations.
#'
#' @param to.path Character string. Destination file path for template files. Typically, root of Rproject directory.
#' @param overwrite Boolean. Overwrite _quarto.yml and style.css files if they already exist? Defaults to FALSE.
#' @inheritParams initialize_project
#'
#' @return Nothing.
#' @export
#'
fetch_quarto_templates = function(to.path,
                                  organization = "WDFW",
                                  overwrite = FALSE) {
  rlang::arg_match(organization, c("WDFW")) ## add more as appropriate.
  ## The associated yaml and style files should be added to the `inst` folder with a subfolder
  ## that matches the organization name

  organization  <-  rlang::arg_match(organization, c("WDFW"))
  yaml.path <-  system.file(glue::glue("{organization}/_quarto.yml"), package = "framrsquared")
  style.path <-  system.file(glue::glue("{organization}/style.css"), package = "framrsquared")
  invisible(file.copy (
    c(yaml.path,
      style.path),
    to = to.path,
    overwrite = overwrite
  ))
}
