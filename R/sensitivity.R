


sensitivity_recruits <- function(fram_db,
                                 run_ids,
                                 stock_id,
                                 age = 3,
                                 start_input,
                                 end_input) {


  inputs <- seq(start_input, end_input, length.out = length(run_ids))


  for (i in run_ids) {
    DBI::dbExecute(fram_db$fram_db_connection,
      statement = glue::glue(
        "UPDATE StockRecruit
          SET StockRecruitScaler = {inputs[i]}
         WHERE RunID = {i} AND
               Age = {age} AND
               StockID = {stock_id};
    "
      )

    )
  }
}
