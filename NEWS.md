# framrsquared ongoing

## New Features

- Tracks connections made with `connect_fram_db()` in `.fram_connections`, a new addition to the global environment. This allows identification of existing connections (including orphans) with `list_extant_fram_connections()` and disconnecting all existing connections (including orphans) with `disconnect_all_fram_connections()`.
- `label_flags()` inserts columns `fishery_flag_label` and `non_retention_flag_label` with human-readable versions of flag numbers if the corresponding flag column is present.
- Re-export `framrosetta::label_fisheries()` and `framrosetta::label_stocks()` which add human-readable columns `$fishery_label` and `$stock_label` to dataframes if `$fishery_id` or `$stock_id` are present, based on LUT in framrosetta. Added `label_stocks_db()` and `label_fisheries_db()` to fulfil the same prupose but using the Stock or Fishery tables of a provided FRAM connection.
- `fetch_table()` and `aeq_mortality()` now default to adding labels for fisheries, stocks, and flags. This can be turned off with `label = FALSE`. DEV NOTE: `fetch_table()_` is a non-exported alias for `fetch_table()` with `label = FALSE`, ditto `aeq_mortality_()` for `aeq_mortality()`.
- `add_total_mortality()` inserts a `total_mortalities` column into a mortality database, which is the sum of the eight mortality columns. Also works on the output of `aeq_mortality()`. This is now used under the hood in several framrsquared functions like `stock_fate_*()`.
- For developers: non-exported function `check_demo_coverage()` takes path to a NAMESPACE file and a document file (like the `framrsquared_test_and_demo.qmd` file) and checks if each of the exported files from the package are present in the document. Useful for double-checking a complete tesitng file. 



## Bug fixes and minor improvements
- Minor improvements to documentation, including addition of `@seeAlso` for most functions.
- `fetch_table()` now gives a warning if it fetches a mortality table with negative mortalities.
- `remove_run()` can now remove multiple runs.
- `change_run_id()` no longer gives a warning when connecting tables after use.
- More input validation. Also more / better validation functions to streamline this process. `validate_flag()` checks for a length 1 logical; `validate_character()` checks for a length n character. `validate_fishery_ids()` confirms the fishery ids are present in the database.
- Several `validate_*()` functions now have optional argument `n` -- if provided, validate that the input is of length `n`.
- `get_run_ids()`,`get_fishery_ids()`, `get_stock_ids()` added to return vector of available runs, fisheries, stocks. Primarily used for internal validation functions.
- added "See also" cross-referencing to many functions
- Revised documentation for many functions to enhance readability.
- improved pass-through of "species" attribute.
- `make_impacts_per_catch_heatmap()` renamed to `plot_impacts_per_catch_heatmap()`. Still a mouthfull, but aligns with naming of other plot functions.
- `stock_comp()` renamed to `plot_stock_comp()` for more consistent naming schemes.
- `terminal_info()` updated with optional argument in anticipation of new table for stock splitting purposes. Default behavior is appropriate for the current table structure.
- plot_stock_mortality now skips timestep 1 for Chinook, as that timestep happens before/during the NoF process.
- style guide functions are now not exported. Still available for use by devs, although considering removal as they are surpassed by styling plugins.
- `make_batch_runs()` exported. Primarily useful internally in sensitivity functions. 



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
