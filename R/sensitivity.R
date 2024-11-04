#' Experimental copying scaler inputs from
#' one run to another DANGEROUS
#' @param fram_db FRAM database object
#' @param run_ids A set of run ids to augement
#' @param stock_id Run ID to be copied to
#' @param age Age (Defult 3)
#' @param start_input Start Input
#' @param end_input End Input
#' @export
#' @examples
#' \dontrun{fram_db |> sensitivity_recruits(140:269,
#'  stock_id = 7, start_input = 0,
#'  end_input = 10000)}
#'


sensitivity_recruits <- function(fram_db,
                                 run_ids,
                                 stock_id,
                                 age = 3,
                                 start_input,
                                 end_input) {

  #start_input = 0; end_input = 1000; run_ids = 149:269; stock_id = 5
  inputs <- seq(start_input, end_input, length.out = length(run_ids))

  base_cohort <- fram_db |>
    fetch_table('BaseCohort')

  runs <- fram_db |>
    fetch_table('RunID')


  cohort <- base_cohort |>
    dplyr::inner_join(runs, by = 'base_period_id',
                      relationship = 'many-to-many') |>
    dplyr::select(
      .data$run_id, .data$base_cohort_size, .data$age, .data$stock_id
    ) |>
    dplyr::filter(
      .data$run_id %in% .env$run_ids,
      .data$stock_id == .env$stock_id,
      .data$age == .env$age
    ) |>
    dplyr::pull(.data$base_cohort_size)



  for (i in seq_along(run_ids)) {

    DBI::dbExecute(fram_db$fram_db_connection,
      statement = glue::glue(
        "UPDATE StockRecruit
          SET RecruitScaleFactor = {inputs[i] / cohort[i]},
              RecruitCohortSize = {inputs[i]}
         WHERE RunID = {run_ids[i]} AND
               Age = {age} AND
               StockID = {stock_id};
    "
      )

    )
  }
}

#' Experimental copying scaler inputs from
#' one run to another DANGEROUS
#' @param fram_db FRAM database object
#' @param run_ids A set of run ids to augement
#' @param fishery_id Run ID to be copied to
#' @param flag Age (Defult 3)
#' @param time_step Time-Step
#' @param cnr_input_1 Range of inputs e.g. 100:1000
#' @param cnr_input_2 Start Input
#' @param cnr_input_3 Start Input
#' @param cnr_input_4 Start Input
#' @export
#' @examples
#' \dontrun{fram_db |> sensitivity_non_retention(149:269,
#'  fishery_id = 11,
#'   time_step = 3,
#'   flag = 3,
#'   cnr_input_1 = 100:1000,
#'    cnr_input_2 = 4000:5000)}
#'


sensitivity_non_retention <- function(fram_db,
                                 run_ids,
                                 fishery_id,
                                 flag = 3,
                                 time_step,
                                 cnr_input_1 = 0,
                                 cnr_input_2 = 0,
                                 cnr_input_3 = 0,
                                 cnr_input_4 = 0
                                 ) {

  #start_input = 0; end_input = 1000; run_ids = 149:269; stock_id = 5
  cnr_1 <- seq(min(cnr_input_1), max(cnr_input_1), length.out = length(run_ids))
  cnr_2 <- seq(min(cnr_input_2), max(cnr_input_2), length.out = length(run_ids))
  cnr_3 <- seq(min(cnr_input_3), max(cnr_input_3), length.out = length(run_ids))
  cnr_4 <- seq(min(cnr_input_4), max(cnr_input_4), length.out = length(run_ids))


  for (i in seq_along(run_ids)) {

    DBI::dbExecute(fram_db$fram_db_connection, glue::glue(
                     "UPDATE NonRetention
          SET CNRInput1 = {cnr_1[i]},
              CNRInput2 = {cnr_2[i]},
              CNRInput3 = {cnr_3[i]},
              CNRInput4 = {cnr_4[i]}
         WHERE RunID = {run_ids[i]} AND
               TimeStep = {time_step} AND
               FisheryID = {fishery_id};
    "
                   )

    )
  }
}

