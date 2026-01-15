# Make batch runs

Make multiple copies of a FRAM run, make copies of specified TAMM in
`target_folder` with run_id suffixes matching newly created runs.
Intended to streamline using the multi-run fork of FRAM. Primarily for
use internally, in the sensitivity analysis functions

## Usage

``` r
make_batch_runs(
  fram_db,
  target_run,
  tamm_name,
  target_folder,
  times = 1,
  label = "copy",
  force_many_runs = FALSE,
  verbose = TRUE
)
```

## Arguments

- fram_db:

  Fram database connection

- target_run:

  Run id of target run

- tamm_name:

  Filepath/name of tamm to copy

- target_folder:

  Location TAMMs should be saved

- times:

  number of run copies to make. Numeric, defaults to 1.

- label:

  Title suffix for newly created runs. Character, defaults to 'copy'. An
  index number is added after this suffix to distinguish copied runs.

- force_many_runs:

  Ignore limits on number of runs in fram database? Logical, defaults to
  FALSE.

- verbose:

  Print details to console? Logical, defaults to TRUE.

## See also

[`sensitivity_exact()`](https://framverse.github.io/framrsquared/reference/sensitivity_exact.md),
[`sensitivity_scaled()`](https://framverse.github.io/framrsquared/reference/sensitivity_scaled.md),
[`sensitivity_custom()`](https://framverse.github.io/framrsquared/reference/sensitivity_custom.md)
