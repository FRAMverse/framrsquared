# Connect to FRAM database

This produces a connection object to a FRAM database, which is a
necessary precursor for almost all framrsquared functions. For most
users, can just treat the connection object as a black box that's used
as an argument in other functions. See details for an explanation of the
returned object.

## Usage

``` r
connect_fram_db(db_path, read_only = FALSE, quiet = FALSE)
```

## Arguments

- db_path:

  Path to a FRAM database.

- read_only:

  Logical, defaults to FALSE. Optional argument to flag this connection
  as read-only (if set to `TRUE`). If `TRUE`, framrsquared functions
  that modify the database will abort rather than run. Use as a safety
  feature when working with a database that *must not* be modified.

- quiet:

  Logical, defaults to FALSE. Optional argument; when TRUE, silences
  success message and database summary.

## Details

The returned object of `connect_fram_db()` is a list of useful objects
for other framrsquared functions.

- `$fram_db_connection`:

  connection object, used for SQL calls.

- `$fram_db_connection_id`:

  connection name in .fram_connections, used for orphan cleanup via
  [`disconnect_all_fram_connections()`](https://framverse.github.io/framrsquared/reference/disconnect_all_fram_connections.md).

- `$fram_db_type`:

  "full" or "transfer", useful for validation for specific functions.

- `$fram_db_species`:

  "COHO" or "CHINOOK"

- `$fram_db_species`:

  filetype, typeically "mdb"

- `$fram_read_only`:

  Optional user-specified safety measure, TRUE or FALSE. Functions that
  modify the fram database should error out if TRUE.

framrsquared creates a special environment in the global environment,
`.fram_connections`. Whenever a new connection is made with
`connect_fram_db`, it is added to that environment as well; whenever the
connection is closed with
[`disconnect_fram_db()`](https://framverse.github.io/framrsquared/reference/disconnect_fram_db.md),
the connection is removed from that environment. This tracking allows
[`disconnect_all_fram_connections()`](https://framverse.github.io/framrsquared/reference/disconnect_all_fram_connections.md)
to clear out any orphan connections made by assigning a connection to an
existing connection object.

## See also

[`disconnect_fram_db()`](https://framverse.github.io/framrsquared/reference/disconnect_fram_db.md),
[`disconnect_all_fram_connections()`](https://framverse.github.io/framrsquared/reference/disconnect_all_fram_connections.md)

## Examples

``` r
if (FALSE) fram_db <- connect_fram_db('<path>')
fram_db |> fetch_table("Mortality") # \dontrun{}
#> Error: object 'fram_db' not found
```
