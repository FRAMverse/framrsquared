# Check if exported functions are documented in a file

Reads a package NAMESPACE file to extract exported functions and checks
whether each function appears in a specified documentation file (e.g., a
Quarto document or vignette). Reports coverage statistics to the console
using the cli package.

## Usage

``` r
check_demo_coverage(namespace_path, doc_path)
```

## Arguments

- namespace_path:

  Character string. Path to the NAMESPACE file, typically located at the
  root of an R package directory.

- doc_path:

  Character string. Path to the documentation file to check (e.g., a
  .qmd, .Rmd, or .md file).

## Value

Invisibly returns a list with three elements:

- `found`:

  Character vector of function names found in the document

- `missing`:

  Character vector of function names not found in the document

- `coverage`:

  Numeric percentage of functions documented

## Details

The function searches for function calls in the form `function_name(` to
determine if a function is present in the documentation. It prints a
formatted report to the console showing which functions are documented
and which are missing.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check if all exported functions appear in a vignette
check_demo_coverage("NAMESPACE", "vignettes/function_demo.qmd")

# Store results for further analysis
results <- check_demo_coverage("NAMESPACE", "README.md")
results$missing
} # }
```
