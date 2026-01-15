# **\[experimental\]** List terminal stock information

For each TAA, lists the associated FRAM stocks and timesteps.

## Usage

``` r
terminal_stocks(fram_db, species = NULL)
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

taa of taa stocks and timesteps

## Examples

``` r
if (FALSE) fram_db |> terminal_stocks() # \dontrun{}
```
