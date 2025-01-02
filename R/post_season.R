#' Generates post-season January age 3 abundances by stock from post-season databases.
#' Used for forecasting.
#' @param fram_db FRAM database object
#' @export
#' @examples
#' \dontrun{framdb |> post_season_abundance()}
#'

post_season_abundance <- function(fram_db){

  validate_fram_db(fram_db)

  if(fram_db$fram_db_species != "COHO"){
    cli::cli_abort('This function currently only works with coho')
  }

  # get data
  stock_recruit <- fram_db |> fetch_table('StockRecruit')
  base_cohort <- fram_db |> fetch_table('BaseCohort')
  stock <- fram_db |> fetch_table('Stock')
  run_id <- fram_db |> fetch_table('RunID') |>
    dplyr::filter(.data$run_type == 'Post') # only want post-season runs

  cohort_table <- run_id |>
    dplyr::inner_join(base_cohort,
               by = 'base_period_id',
               relationship = 'many-to-many') |> # making sure baseperiod is correct for run
    dplyr::inner_join(stock, by = 'stock_id') |>
    dplyr::filter(
      .data$run_year >= 2010, # don't care about earlier apparently
    ) |> # don't care about earlier apparently
    dplyr::inner_join(stock_recruit, by = c('run_id', 'stock_id'),
                      relationship = 'many-to-many') |>
    dplyr::mutate(
      recruit_cohort_size = .data$recruit_scale_factor * .data$base_cohort_size,
      origin = dplyr::case_when(
        stringr::str_detect(stock_long_name, 'Wild') |
          stringr::str_detect(stock_long_name, 'Nat') |
          stringr::str_detect(stock_long_name, '/Wild')  ~ 'Wild',
        stringr::str_detect(stock_long_name, 'Hatchery') |
          stringr::str_detect(stock_long_name, 'Net Pen')  ~ 'Hatchery',
        .default = 'Misc'

      )
    ) |> #count(origin)
    dplyr::select(.data$run_year, .data$run_id, .data$stock_id,  .data$stock_name,
           .data$recruit_scale_factor, .data$base_cohort_size,
           .data$recruit_cohort_size, .data$origin)


  cli::cli_alert_info('Abundances given in terms of January age 3')
  # summary sheet
  cohort_table |>
    dplyr::select(.data$stock_id, .data$stock_name, .data$run_year,
                  .data$recruit_cohort_size, .data$origin) |>
    tidyr::pivot_wider(names_from = .data$run_year, values_from = .data$recruit_cohort_size)

}
