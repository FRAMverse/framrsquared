# framrsquared 0.7.0

## New features

- `modify_table()` allows flexible modification of FRAM databases, `calc_fram_scaling` allows scaling of existing values of FRAM databases
- `sensitivity_scaled()` and `sensitivity_exact()` allow easy creation of sensitivity analysis runs that are compatible with the multi-run fork of FRAM
- `sensitivity_custom()` allows definition of complex scenarios to run for a sensitivity analysis, presumably using programmatic generation. For example, could be used to explore how changing age composition (without changing abundance) affects ERs.
- Vignette on sensitivity analysis.

## Bug fixes and minor improvements

- Fix to `post_season_abundance()`: previously the function would misbehave if both chinook and coho stocks were defined in the "Stock" table. This is no longer the case.
- Fix to `mortality_scaler()`: previously did not correctly account for terminal area fisheries, does now.
- minor additional validation functions, including `validate_table()`, `provide_table_name()` and `fetch_table_colnames()`.

# framrsquared 0.6.0

## New features

## Bug fixes and minor improvements

# framrsquared 0.5.0

## New features

## Bug fixes and minor improvements

...
