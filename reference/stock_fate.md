# **\[experimental\]** Summarizes the three true outcomes of a stocks abundance, where it dies to fishery related mortality, natural mortality, or reaches some sort of escapement. When run against the coho database spawning escapement will be displayed, when run against the Chinook database escapement to the river will be dislpayed along with recruits to the next year 'age_up'

**\[experimental\]** Summarizes the three true outcomes of a stocks
abundance, where it dies to fishery related mortality, natural
mortality, or reaches some sort of escapement. When run against the coho
database spawning escapement will be displayed, when run against the
Chinook database escapement to the river will be dislpayed along with
recruits to the next year 'age_up'

## Usage

``` r
stock_fate(fram_db, run_id = NULL, units = c("fish", "percentage"))
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  Run ID (optional)

- units:

  'fish' or 'percentage'. Percentage is proportion of starting adundance

## Examples

``` r
if (FALSE) { # \dontrun{
fram_db |> stock_fate(run_id = 145)
} # }
```
