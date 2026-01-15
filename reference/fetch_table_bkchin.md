# Safely fetch Chinook BackwardsFRAM table

The BackwardsFRAM table uses a stock_id different numbering system from
all other tables, and includes sums of joint stocks (e.g. for a marked
and unmarked pair of stocks, BackwardsFRAM will typically have an
additional stock which represents the sum of those two). Because the
numbering is different but the column name is the same, joining the
Chinook BackwardsFRAM table with other tables can cause problems.

## Usage

``` r
fetch_table_bkchin(fram_db)
```

## Arguments

- fram_db:

  FRAM database object

## Details

This function augments fetch_table by renaming the `stock_id` column to
`bk_stock_id`, and then adding on the associated stock_id (with NAs when
the bkfram stock is one of these new "sum" stocks and the associated
bkfram stock names). This function only works for Chinook databases.

**The resulting dataframe will necessarily NOT be an exact match to the
BackwardsFRAM table in the FRAM database. The stock_id column will
differ (containing normal stock ID values instead of bk stock ID
values), and there will be two additional columns.**

## Examples

``` r
#' @examples
if (FALSE) { # \dontrun{
##Potentially problematic stock_id won't align with other tables
fram_db |> fetch_table('BackwardsFRAM')
## "safe" version of the table; stock_id WILL align with other tables
fram_db |> fetch_table_bkchin()
} # }
```
