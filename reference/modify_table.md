# **\[experimental\]** Modify FRAM database based on match/replace dataframe

Uses a special match/replace dataframe to modify values in a FRAM table.

## Usage

``` r
modify_table(fram_db, table_name, df)
```

## Arguments

- fram_db:

  FRAM database

- table_name:

  Name of FRAM table

- df:

  The match/replace dataframe or tibble with specially named columns.
  Columns must start with either "match\_" or "replace\_", and should
  otherwise match the names of columns in `table`. For example,
  modifications to the Cohort table might be achieved with columns
  "match_RunID", "match_StockID", "match_age", "match_TimeStep",
  "replace_StartCohort". See Details.

## Details

At a high level, modifying a FRAM table requires identify which rows to
change, and then replacing the values of one or more of the columns of
those row with new values. We often want to make multiple changes at
once, and `modify_table` is written around using a dataframe to define
the matching and replacing, so that it is relatively easy to check all
of the changes being made. This dataframe (hereafter the "match/replace
dataframe") should have column names starting with "match\_" and
"replace\_", and ending with the exact match of column names in the FRAM
table identified with argument `table_name`. For each row of argument
`df`, `modify_table()` will use columns starting with "match\_" as
conditions to identify rows in the FRAM database to modify, and then for
those rows will replace the values of columns identified with
"replace\_" with the corresponding values in the `df` columns.

As a simple example, imagine we want to see how modifying the size
limits for Area 7 Sport (chinook fishery id 36) affect our ERs. We would
probably start by using copy_run to create multiple duplicate runs, and
then we can use `modify_table` to change just the `MinimumSize` values
of the "SizeLimits" table for just those rows for which fishery id was
36. If our run ids were 100, 101, and 102, and we wanted to look at
minimum sizes of 450, 550, and 650, our `df` argument might look like
`data.frame(match_RunID = c(100, 101, 102), match_FisheryID = c(36, 36, 36), replace_MinimumSize = c(450, 550, 650))`.
Notably, we might create `df` programmatically to combine different run
ids with multiple changes at once or to apply some kind of randomized
parameter sampling scheme. Or we could even use an excel sheet to write
out the experiment in a `df` format and then read in the sheet and feed
it into `modify_table`.

## Examples

``` r
if (FALSE) { # \dontrun{
df_total <- tibble(match_Age = c(3, 4, 5))
df_total$match_StockID <- 100
df_total$match_RunID <- 396
df_total$replace_RecruitScaleFactor <- 1:3
df_total$replace_RecruitCohortSize <- 100:102
fram_db |> modify_db(table_name = "StockRecruit", df = df_total)
} # }
```
