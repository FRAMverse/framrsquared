# **\[experimental\]** Initializes a FRAM project

By default, creates suggested folder structure from [best coding
practices](https://framverse.github.io/coding-practices/), and adds
WDFW-style yaml and style.css files to give quarto files consistent
appearance. If you belong to another organization and want this function
to support your own organization-specific quarto styling, reach out to
the developers with a `_quarto.yml` and (optionally) `style.css` file.

## Usage

``` r
initialize_project(
  folders = c("scripts", "original_data", "cleaned_data", "figures", "results",
    "results/quarto_output"),
  quarto = TRUE,
  organization = c("WDFW"),
  renv = FALSE,
  template_overwrite = TRUE,
  color = "coffee",
  quiet = TRUE
)
```

## Arguments

- folders:

  Vector of folders to create

- quarto:

  Boolean. If TRUE, add quarto yaml file and style.css

- organization:

  Character, defaults to "WDFW". Specifies the set of quarto templates
  to use. Currently only supports "WDFW".

- renv:

  Boolean to initialize renv. Defaults to FALSE.

- template_overwrite:

  Boolean. Overwrite \_quarto.yml and style.css files if they already
  exist? Defaults to TRUE

- color:

  Character string, defaults to "coffee". Specifies quarto template to
  use; organizations may have several.

- quiet:

  Boolean, defaults to FALSE. If TRUE, suppresses informational
  messages.

## Details

Dev note: new template files for additional organizations should be
added to `inst/` in a subfolder matching an R-friendly organization
name, and the same name should be added to the `organization` parameter
description here and the `supported_organizations` in
[`fetch_quarto_templates()`](https://framverse.github.io/framrsquared/reference/fetch_quarto_templates.md).

## See also

[`fetch_quarto_templates()`](https://framverse.github.io/framrsquared/reference/fetch_quarto_templates.md)

## Examples

``` r
if (FALSE) { # \dontrun{
framrsquared::initialize_project()
} # }
```
