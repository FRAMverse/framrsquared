# Adds a column with a text version of flags for either non-retention or fishery scalers

Adds a column with a text version of flags for either non-retention or
fishery scalers

## Usage

``` r
label_flags(.data, species = NULL, warn = TRUE)
```

## Arguments

- .data:

  fetched FisheryScalers or NonRetentions

- species:

  Optional, identifying species if `.data` doesn't. If provided, should
  be "CHINOOK" or "COHO" (or variants)

- warn:

  Logical, defaults to TRUE. Warn if neither flag column is present in
  dataframe?

## Examples

``` r
if (FALSE)  mortality_table |> add_flag_text() # \dontrun{}
```
