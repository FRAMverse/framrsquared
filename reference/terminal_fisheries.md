# **\[experimental\]** List terminal stock information

For each TAA, lists the associated fisheries

## Usage

``` r
terminal_fisheries(fram_db, species = NULL)
```

## Arguments

- fram_db:

  Fram database object

- species:

  "COHO" or "CHINOOK". Optional, defaults to the database species.
  Provide this only if fram_db connects to a database with both Chinook
  and Coho information. And try to avoid that – those databases are
  sketchy to work with.

## Value

tibble of taa fisheries

## Examples

``` r
if (FALSE) fram_db |> terminal_fisheries() # \dontrun{}
```
