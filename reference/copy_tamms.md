# **\[experimental\]** Copy TAMM for FRAM batch runs

Preps a folder for batch running in the 'Run Multiple Runs' screen of
the FRAM automation fork (https://github.com/FRAMverse/FRAM_automation),
for use with the `advanced` approach to identify multiple runs. One TAMM
file will be copied multiple times in the `target_folder` with suffixes
that identify each of the run_ids. The "Use folder" button on the "Run
Multiple Runs" screen can then use the target folder to set up large
batch runs. Typically users should use
[`make_batch_runs()`](https://framverse.github.io/framrsquared/reference/make_batch_runs.md)
instead (this first copies runs and then uses `copy_tamms` to create
tamms that match the new runs).

## Usage

``` r
copy_tamms(tamm_name, target_folder, run_id_vec, overwrite = FALSE)
```

## Arguments

- tamm_name:

  TAMM file to copy, including file path. Character string

- target_folder:

  directory to put new batch TAMM files into. Character string

- run_id_vec:

  vector of run_ids (numeric or character), corresponding to run ids in
  a FRAM database.

- overwrite:

  If one or more files already exist in `target_folder` with names
  matching the combination of `tamm_name` and run ids, overwrite
  (`TRUE`) or leave those files untouched (`FALSE`). Defaults to `FALSE`
  for safety; recommend setting to `TRUE` to avoid confusion when
  iterating on work.

## Value

invisibly returns logical vector of
[`file.copy()`](https://rdrr.io/r/base/files.html) success.

## Examples

``` r
if (FALSE) copy_tamms(tamm_name = "C:/TAMMs/Chin2020.xlsx",
target_folder = "C:/Batch_run_5", run_id_vec = 10:20) # \dontrun{}
```
