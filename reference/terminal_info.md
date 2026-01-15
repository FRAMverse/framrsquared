# **\[experimental\]** Parse TAAETRS table

Terminal run information used by FRAM is stored in the TAAETRSList and
(soon) the TAAETRSListChinook tables, but stored in a way that is not
very human readable. `parse_terminal_info()` translates this to
human-readable form, primarily to then be used by
[`terminal_stocks()`](https://framverse.github.io/framrsquared/reference/terminal_stocks.md)
and
[`terminal_fisheries()`](https://framverse.github.io/framrsquared/reference/terminal_fisheries.md).

## Usage

``` r
terminal_info(fram_db, old_table_name = TRUE, species = NULL)
```

## Arguments

- fram_db:

  Fram database object

- old_table_name:

  Logical, defaults to TRUE. We intend to change the FRAM table from
  TAAETRSList to TAAETRSListChinook to avoid confusion. When working
  with a database where that hasn't been done, leave this argument to
  TRUE.

- species:

  "COHO" or "CHINOOK". Optional, defaults to the database species.
  Provide this only if fram_db connects to a database with both Chinook
  and Coho information. And try to avoid that – those databases are
  sketchy to work with.

## Value

tibble of TAAETRSList or TAAETRSListChinook tables translated to long
form.

## See also

[`terminal_stocks()`](https://framverse.github.io/framrsquared/reference/terminal_stocks.md),
[`terminal_fisheries()`](https://framverse.github.io/framrsquared/reference/terminal_fisheries.md)

## Examples

``` r
if (FALSE) fram_db |> parse_terminal_info() # \dontrun{}
```
