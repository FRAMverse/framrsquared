# Extract AEQ mortality from Chinook FRAM database. Refactored and stripped down from the framr package written by Dan Auerbach. https://github.com/FRAMverse/framr/

Extract AEQ mortality from Chinook FRAM database. Refactored and
stripped down from the framr package written by Dan Auerbach.
https://github.com/FRAMverse/framr/

## Usage

``` r
aeq_mortality(fram_db, run_id = NULL, msp = TRUE, label = TRUE)
```

## Arguments

- fram_db:

  Fram database object

- run_id:

  numeric, RunID(s) as ID or ID:ID

- msp:

  Do we use MSP expansion? Logical, default true.

- label:

  Logical, defaults to TRUE. Add human-readable columns for flags,
  fisheries, stocks?

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |> aeq_mortality(run_id = 132)
} # }
```
