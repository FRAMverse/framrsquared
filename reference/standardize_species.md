# Allow multiple species identifiers

framrsquared functions are written around fram database species labels,
"COHO" and "CHINOOK". This function translates alternate designations
(lowercase, "chin" for "chinook") into those two forms.

## Usage

``` r
standardize_species(species)
```

## Arguments

- species:

  Character atomic, either "COHO", "CHIN", or "CHINOOK", with any
  capitalization

## Value

Character atomic, either "COHO" or "CHINOOK"
