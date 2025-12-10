#' Check if exported functions are documented in a file
#'
#' Reads a package NAMESPACE file to extract exported functions and checks
#' whether each function appears in a specified documentation file (e.g., a
#' Quarto document or vignette). Reports coverage statistics to the console
#' using the cli package.
#'
#' @param namespace_path Character string. Path to the NAMESPACE file, typically
#'   located at the root of an R package directory.
#' @param doc_path Character string. Path to the documentation file to check
#'   (e.g., a .qmd, .Rmd, or .md file).
#'
#' @return Invisibly returns a list with three elements:
#' \describe{
#'   \item{`found`}{Character vector of function names found in the document}
#'   \item{`missing`}{Character vector of function names not found in the document}
#'   \item{`coverage`}{Numeric percentage of functions documented}
#' }
#'
#' @details
#' The function searches for function calls in the form `function_name(` to
#' determine if a function is present in the documentation. It prints a
#' formatted report to the console showing which functions are documented
#' and which are missing.
#'
#' @examples
#' \dontrun{
#' # Check if all exported functions appear in a vignette
#' check_demo_coverage("NAMESPACE", "vignettes/function_demo.qmd")
#'
#' # Store results for further analysis
#' results <- check_demo_coverage("NAMESPACE", "README.md")
#' results$missing
#' }
#'

check_demo_coverage <- function(namespace_path, doc_path) {
  # Read NAMESPACE file
  if (!file.exists(namespace_path)) {
    cli::cli_abort("NAMESPACE file not found at {.path {namespace_path}}")
  }

  namespace_lines <- readLines(namespace_path)

  # Extract exported functions
  export_lines <- namespace_lines[grepl("^export\\(", namespace_lines)]
  exported_functions <- gsub("export\\((.+)\\)", "\\1", export_lines)

  if (length(exported_functions) == 0) {
    cli::cli_alert_warning("No exported functions found in NAMESPACE")
    return(invisible(NULL))
  }

  # Read document
  if (!file.exists(doc_path)) {
    cli::cli_abort("Document not found at {.path {doc_path}}")
  }

  doc_content <- paste(readLines(doc_path), collapse = "\n")

  # Check which functions are present
  functions_found <- sapply(exported_functions, function(fn) {
    grepl(paste0(fn, "\\("), doc_content, perl = TRUE)
  })

  found <- exported_functions[functions_found]
  missing <- exported_functions[!functions_found]

  # Report results
  cli::cli_h1("Function Coverage Check")
  cli::cli_alert_info("Total exported functions: {length(exported_functions)}")
  cli::cli_alert_success("Functions found: {length(found)}")
  cli::cli_alert_danger("Functions missing: {length(missing)}")

  if (length(found) > 0) {
    cli::cli_h2("Found ({length(found)})")
    cli::cli_ul(found)
  }

  if (length(missing) > 0) {
    cli::cli_h2("Missing ({length(missing)})")
    cli::cli_ul(missing)
  }

  # Summary
  coverage <- round(length(found) / length(exported_functions) * 100, 1)
  cli::cli_rule()
  cli::cli_alert_info("Coverage: {coverage}%")

  invisible(list(found = found, missing = missing, coverage = coverage))
}
