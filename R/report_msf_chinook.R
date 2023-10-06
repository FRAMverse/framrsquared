msf_mortalities_chinook_ <- function(fram_db, run_id){
  mortalities_ <- fram_db |>
                    fetch_table('Mortality') |>
    dplyr::filter(.data$run_id == .env$run_id)

  #run_id <- 132
  mortalities_ <- mortalities_ |>
    dplyr::select(run_id:time_step, starts_with('msf_'))  |>
    dplyr::mutate( # needs to happen after legal/sublegal identification
      mark_status = dplyr::if_else(stock_id %% 2 == 0, 'M', 'UM')
    )


  mortalities <- mortalities_ |>
    tidyr::pivot_longer(c(msf_landed_catch:msf_drop_off)) |>
    dplyr::group_by(fishery_id, time_step, name, mark_status) |> # over stock
    dplyr::summarize(value = sum(value), .groups = 'drop') |>
    dplyr::mutate(
      legality  = dplyr::if_else(
        name %in% c(
          'msf_shaker'
        ),
        'sublegal',
        'legal')
    ) |>
    tidyr::unite('legal_mark_status', legality, mark_status) |>
    dplyr::group_by(fishery_id, time_step, legal_mark_status) |>
    dplyr::summarize(value = sum(value), .groups = 'drop')

  # output
  mortalities |>
    tidyr::pivot_wider(names_from = legal_mark_status, values_from = value)

}

msf_encounters_chinook_ <- function(fram_db, run_id){
  encounters_ <- DBI::dbGetQuery(
    fram_db$fram_db_connection,
    glue::glue('SELECT * FROM Mortality WHERE RunID = {run_id}')) |>
    fram_clean_tables()

  # ensure the correct rates are always used
  sublegal_mortality_rates <- DBI::dbGetQuery(
    fram_db$fram_db_connection,
    glue::glue('SELECT
                    RunID.RunID, ShakerMortRate.FisheryID,
                    ShakerMortRate.TimeStep,
                    ShakerMortRate.ShakerMortRate
                FROM RunID INNER JOIN
               ShakerMortRate ON RunID.BasePeriodID = ShakerMortRate.BasePeriodID
              WHERE (((RunID.RunID)={run_id}));')
  ) |> fram_clean_tables()

  encounters <- encounters_ |>
    dplyr::select(run_id, stock_id, fishery_id, time_step, msf_shaker, msf_encounter)  |>
    dplyr::mutate( # needs to happen after legal/sublegal identification
      mark_status = dplyr::if_else(stock_id %% 2 == 0, 'M', 'UM'),
    )

  encounters |>
    dplyr::group_by(run_id, mark_status, fishery_id, time_step) |>
    dplyr::summarize(msf_encounter = sum(msf_encounter), msf_shaker = sum(msf_shaker), .groups='drop') |>
    dplyr::inner_join(sublegal_mortality_rates, by=c('fishery_id', 'time_step', 'run_id')) |>
    dplyr::mutate(
      shaker_encounters = msf_shaker / shaker_mort_rate
    ) |>
    dplyr::select(-c(msf_shaker:shaker_mort_rate), legal = msf_encounter, sublegal= shaker_encounters) |>
    tidyr::pivot_longer(legal:sublegal) |>
    tidyr::unite('legal_mark_status', name, mark_status) |>
    tidyr::pivot_wider(names_from = legal_mark_status, values_from = value)
}


msf_landed_catch_chinook_ <- function(fram_db, run_id){
  landed_catch_ <- fram_db |>
    fetch_table('Mortality') |>
    dplyr::filter(.data$run_id == .env$run_id)


  landed_catch <- landed_catch_ |>
    dplyr::select(run_id, stock_id, fishery_id, time_step, msf_landed_catch, msf_shaker)  |>
    dplyr::mutate( # needs to happen after legal/sublegal identification
      mark_status = dplyr::if_else(stock_id %% 2 == 0, 'M', 'UM'),
    )


  landed_catch |>
    dplyr::group_by(run_id, mark_status, fishery_id, time_step) |>
    dplyr::summarize(msf_landed_catch = sum(msf_landed_catch), .groups='drop') |>
    dplyr::select(everything(), legal = msf_landed_catch) |>
    tidyr::pivot_longer(legal) |>
    tidyr::unite('legal_mark_status', name, mark_status) |>
    tidyr::pivot_wider(names_from = legal_mark_status, values_from = value)
}
