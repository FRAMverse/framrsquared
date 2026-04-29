# framrsquared 0.8.1.9001

## Bug fixes
- fixed bug in which `copy_runs()` did not correctly copy entries of "SLRatio" table for Chinook. 
- fixed bug in which `fetch_table()` errored out when fetching the `Stock` or `Fishery` tables.
- fixed potential bug in which `aeq_mortality()` function can accept a run_id for a run that is present in the database but has not been run in FRAM, and thus has no values in the mortality table. Now provides informative error message in this case.

## New features and improvements

- added functions to handle SONCC calculations for STT.
- updated `compare_runs()` to apply specified tolerance to recruits as well as fishery inputs, compare SLRatio table (Chinook only), and add option to save output to text file instead of console, and invisibly returns list of the comparison dataframes. `compare_sl_ratio()` handles the sublegal ratio comparisons.
- `compare_runs()` now returns changes to fishery inputs even if the flag indicates those inputs won't be used (e.g., changes to msf_quota when the fishery flag is 1). This should help identify if changes were accidentally made in the wrong place. 
- Overhauled `plot_impacts_per_catch_heatmap`: can control more aspects of the plots (rounding, font size, abbreviated or full stock name for title), fishery labels include id numbers and timestep labels include months. Can toggle between "landed catch per impacts" (default) and "impacts per thousand landed catch". Function can now accept multiple stock ids, making it more useful for managing to objectives that are based on a sum of FRAM stocks. Function no longer includes CNR in the impacts per landed catch.
- improved speed of `stock_mortality()` and `fishery_mortality()`. Optionally accept either `stock_id` or `fishery_id` arguments which filter the fetched `mortality` function for improved speed. 
- Updated `plot_stock_mortality()` and `plot_stock_mortality_timestep()` to account for CNR. Default behavior now
provides the % CNR on the plot. Setting optional argument `split_cnr` to `TRUE` produces separate panels for CNR and non-CNR mortalities. Also: cosmetic changes and optional arguments to control them, support (and appropriate warnings) for taking multiple stock_id. `warn` and `verbose` arguments control printing of informative messages in the console.
- filter functions now have optional argument `return_ids` -- when set to `TRUE`, functions return the fishery (or stock) ids used to filter rather than the filtered dataframe. Useful for double-checking things, creating "union filters" and for planned future work.
- new filter functions added: `filter_stt()` and `filter_stt_nt()` for STT Coho stocks; `filter_hatchery()`, `filter_wild()`, `filter_mixed()` for coho stocks.
- Added `filter_nr_flags()` to apply NAs to non-retention dataframes based on the non_retention_flag column. Analogous to `filter_flags()`
- Added `check_bp_coverage()`, which identifies if fishery inputs or CNR inputs contain fishery x time steps that are not represented in the base period. This is now called in compare_runs()
- `calculate_stock_comp()` -- new function that does the calculations for `plot_stock_comp()`. Splitting these makes it easier to do other work with stock compositions, like making tables.


## Possible breaking changes

- `connect_fram_db()` now errors out if two species are present in the "RunID" table; we cannot guarantee the functions of this package would behave appropriately in that situation. Such a database should never happen during normal NOF processes, so this should not interfere with users. (It may not even be possible to create this in FRAM?)
-`filter_flag()` renamed to `na_scalers_from_flag()` and `filter_nr_flag()` renamed to `na_non_retention_from_flag()` for clarity (they're not actually filtering, but turning unused scalers or cnr params to NAs) and to make package naming more consistent (All other `filter_*()` functions filter a dataframe based on `$fishery_id` or `$stock_id`)
- `NR_flag_translate()` renamed to `translate_nr_flag()` for consistency (no capitalization, starts with verb).
- `scalers_flag_translate()` renamed to `translate_scalers_flag()` for consistency (starts with verb).
- `copy_tamms()` renamed to `copy_tamm()` to better reflect what it does and to align with `copy_run()`.

## developer-facing changes

- Added better testing framework for ad-hoc and integration tests, linked to external testing database directory. Only affects developers who want to run the unit tests -- see the "Contribute" section of Readme for details on setup. Convenience connection functions are included in `tests/testthat/helper-dir.R", but will only work if the testing database directory is set up. 
- `fram_abort()` -- wrapper for cli_abort that adds custom error class. Allows for better testthat behavior (expect_error() can confirm that the error comes from this package, not others)
- Overhaul of internal usage functions. Functions designed for internal use that are not exported are now listed with `@keywords internal` to enable documentation of help Rds (e.g., `validate_numeric()`. Functions that were previously intended for internal use but were exported (e.g., `provide_table_names()`) are still exported but have been given `@keywords internal`. This means they are exposed to users and will continue to function in existing scripts/packages, but aren't included in lists of functions intended for casual users.
- Unit tests and integration tests added to functions in the following files: "Integrity.R"



# framrsquared 0.8.1

- fixed bug in which `fetch_table()` could cause row duplications if a database had more than 1 version (or species) of stock or fishery id unless labeling was turned off. This has been resolved. `fetch_table()` no longer labels tables when working with a transfer database, as in that case the mapping of labels to stock or fishery IDs are ambigious with overhaul of framrosetta (and possibly even then).

# framrsquared 0.8.0

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
