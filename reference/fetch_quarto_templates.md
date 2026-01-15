# Creates quarto template files

Creates template files from specified organization in the specified
path. Generally
[`initialize_project()`](https://framverse.github.io/framrsquared/reference/initialize_project.md)
will be more useful for new R projects, while `fetch_quarto_templates()`
can be helpful when working with existing projects. See
[`initialize_project()`](https://framverse.github.io/framrsquared/reference/initialize_project.md)
for details on adding template files for new organizations.

## Usage

``` r
fetch_quarto_templates(
  to.path,
  organization = c("WDFW"),
  color = "coffee",
  overwrite = FALSE
)
```

## Arguments

- to.path:

  Character string. Destination file path for template files. Typically,
  root of Rproject directory.

- organization:

  Character, defaults to "WDFW". Specifies the set of quarto templates
  to use. Currently only supports "WDFW".

- color:

  Character string, defaults to "coffee". Specifies quarto template to
  use; organizations may have several.

- overwrite:

  Boolean. Overwrite \_quarto.yml and style.css files if they already
  exist? Defaults to FALSE.

## Value

Nothing.

## See also

[`initialize_project()`](https://framverse.github.io/framrsquared/reference/initialize_project.md)
