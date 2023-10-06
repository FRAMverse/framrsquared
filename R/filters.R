# filters

filter_sport <- function(.data){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        fishery_id %in% c(36, 42, 45, 53, 54, 56, 57, 64, 67)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        fishery_id %in% c(91, 92, 93, 106, 107, 115, 118, 129, 136, 152)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }

}
