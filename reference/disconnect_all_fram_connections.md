# Clear all connections

It is relatively easy to create an orphan connection using framrsquared
by assigning a connection to `fram_db`, and then assigning another
connection to `fram_db` without disconnecting the first connection using
[`disconnect_fram_db()`](https://framverse.github.io/framrsquared/reference/disconnect_fram_db.md).
Orphaned connections can make it frustrating to work with database files
(moving, deleting, etc) without restarting rstudio or rebooting your
computer. `disconnect_all_fram_connections()` disconnects any existing
connections made by framrsquared in this R session.

## Usage

``` r
disconnect_all_fram_connections()
```

## Value

nothing

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db = connect_fram_db("Chin2025.mdb")
fram_db = connect_fram_db("Chin2025.mdb")
disconnect_fram_db(fram_db)

list_extant_fram_connections()
## oops, still a connection left.

disconnect_all_fram_connections()

list_extant_fram_connections
} # }
```
