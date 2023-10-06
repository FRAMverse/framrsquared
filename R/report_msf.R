# Functions collected here output the MSF Screen report

msf_mortalities <- function(fram_db, run_id){
  # checks on the run id
  if(is.null(run_id)){rlang::abort('Run ID must be provided.')}
  if(!is.numeric(run_id)) {rlang::abort('Run ID must be and integer')}

  if(DBI::dbIsValid(fram_db$fram_db_connection)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_mortalities_chinook_(fram_db, run_id),
      'COHO' = print('Not coded yet')
    )
  } else {
    rlang::abort('Connect to a FRAM database first...')
  }
}


msf_encounters <- function(fram_db, run_id){
  # checks on the run id
  if(is.null(run_id)){rlang::abort('Run ID must be provided.')}
  if(!is.numeric(run_id)) {rlang::abort('Run ID must be and integer')}
  if(fram_db$fram_db_type == 'transfer'){
    rlang::abort('
                 Cannot be connected to a transfer database to use this function
                 ')
  }

  if(DBI::dbIsValid(fram_db$fram_db_connection)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_encounters_chinook_(fram_db, run_id),
      'COHO' = print('Not coded yet')
    )
  } else {
    rlang::abort('Connect to a FRAM database first...')
  }
}


msf_landed_catch <- function(fram_db, run_id){
  # checks on the run id
  if(is.null(run_id)){rlang::abort('Run ID must be provided.')}
  if(!is.numeric(run_id)) {rlang::abort('Run ID must be and integer')}

  if(DBI::dbIsValid(fram_db$fram_db_connection)){
    switch(
      fram_db$fram_db_species,
      'CHINOOK' = msf_landed_catch_chinook_(fram_db, run_id),
      'COHO' = print('Not coded yet')
    )
  } else {
    rlang::abort('Connect to a FRAM database first...')
  }
}





