# Fetch a complete table from a FRAM database.

Returns a cleaned tibble, with column labels that were camel case (e.g.,
TimeStep) converted to snake case (e.g., time_step). **WARNING**: the
Chinook "BackwardsFRAM" table uses a *different* stock_id numbering
system from every other table. To avoid errors when joining that with
other tables, instead fetch with
[`fetch_table_bkchin()`](https://framverse.github.io/framrsquared/reference/fetch_table_bkchin.md)

## Usage

``` r
fetch_table(fram_db, table_name = NULL, label = TRUE, warn = TRUE)
```

## Arguments

- fram_db:

  FRAM database object

- table_name:

  Atomic character of name of table to be fetched. If not given, a list
  of options will be printed.

- label:

  Logical, defaults to TRUE. Add human-readable columns for flags,
  fisheries, stocks?

- warn:

  Print a warning when fetching BackwardsFRAM table from a Chinook
  database? Logical, defaults to `TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db <- connect_fram_db("validat2024.mdb")
fram_db |> fetch_table('Mortality')} # }
```
