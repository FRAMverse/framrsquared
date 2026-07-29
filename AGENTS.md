# framrsquared — Posit Assistant Memory

## Project
R package for interacting with FRAM (Fisheries Regulation Assessment Model) databases.

## Testing conventions

### Mock database helpers
All mock DB builder functions used in tests must include a `return_list` parameter (default `FALSE`).
When `return_list = TRUE`, return the named list of data frames instead of a `make_queryable_mock_db_list()` connection object. See `make_reasonable_mock_chinook_db()` in `tests/testthat/test-aeq_mortality.R` for the canonical example.

```r
make_my_mock_db <- function(..., return_list = FALSE) {
  table_list <- list(
    Mortality = ...,
    RunID     = ...
  )
  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list = table_list, species = ...)
}
```

### Other patterns
- Clean up mock DB connections with `withr::defer(disconnect_mock_fram_db(db))`.
- Use `expect_error(..., class = "framrsquared_error")` for package-specific error assertions.
- `make_queryable_mock_db_list()` and related helpers live in `R/test_helpers.R`.
- Validation helpers (e.g. `validate_run_id`, `validate_fishery_ids`) need `RunID` / `Fishery` tables in the mock DB when those arguments are non-`NULL`.
