# Experimental copying scaler inputs from one run to another DANGEROUS

Experimental copying scaler inputs from one run to another DANGEROUS

## Usage

``` r
copy_fishery_scalers(fram_db, from_run, to_run, fishery_id = NULL)
```

## Arguments

- fram_db:

  FRAM database object

- from_run:

  Run ID to be copied from

- to_run:

  Run ID to be copied to

- fishery_id:

  ID or IDs for specific fishery(s) to copy inputs to/from. If not
  provided, interactive option to copy inputs for all fisheries.

## Examples

``` r
if (FALSE) framdb |> copy_fishery_scalers(132, 133, 87) # \dontrun{}
```
