# Handle species identification for filters

Convenience function to condense code. `filter_*` either uses the
"species" attr, or the optional `species` argument, and must provide
informative errors when both are missing or both are present and
mismatch.

## Usage

``` r
validate_species(.data, species = NULL)
```

## Arguments

- .data:

  Dataframe

- species:

  Optional, either "COHO" or "CHINOOK".

## Value

Character vector "species"
