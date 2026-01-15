# **\[experimental\]** Copies a run a number of times

FRAM is stored in an access database; these have hard size limits of
2GB. Chinook and Coho are expected to reach this limit with ~540 runs.
This function includes a failsafe to prevent databases from exceeding
500 runs. This can be overridden with optional `force_many_runs`
argument.

## Usage

``` r
copy_run(
  fram_db,
  target_run,
  times = 1,
  label = "copy",
  force_many_runs = FALSE,
  verbose = TRUE
)
```

## Arguments

- fram_db:

  FRAM database object

- target_run:

  Run ID to be copied from

- times:

  Number of copies

- label:

  Label of each copy e.g. copy 1, copy 2

- force_many_runs:

  `copy_runs` has failsafe to keep total run number no more than 500.
  This is expected to be the approximate limit for .mdb file size after
  runs have been run. When `force_man_runs` is `TRUE`, ignore this
  failsafe.

- verbose:

  Show warning message about run count? Official FRAM is hard-coded to
  only handle databases with \<= 150 runs in them. If `TRUE` (default),
  provides alert when updated database will exceed this.

## Value

Invisibly returns the run ids of the copied runs, for use in other
functions.

## Examples

``` r
if (FALSE) framdb |> copy_run(target_run = 141, times = 1) # \dontrun{}
```
