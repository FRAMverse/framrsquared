#' Filters a dataframe to sport fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe generated within this package
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_sport()
#' }
#'
filter_sport <- function(.data){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(
          3, 8, 11, 13, 14, 15,
          18, 22, 27, 29, 31, 33,
          35, 48, 60, 62, 72, 36,
          42, 45, 53, 54, 56, 57,
          64, 67)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(
          3, 5, 7, 15, 17, 19, 21, 23, 24,
          28, 29, 31, 33, 37, 40, 41, 45, 46,
          48, 49, 51, 54, 58, 59, 60, 61, 62,
          65, 66, 67, 70, 73, 76, 89, 90, 91,
          92, 93, 94, 95, 99, 100, 106, 107,
          108, 115, 116, 117, 118, 127, 129,
          135, 136, 149, 150, 151, 152, 163,
          164, 165, 166, 169, 186, 187, 188,
          189, 190, 191, 192, 193

          )
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }

}



#' Filters a dataframe to Puget Sound fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe generated within this package
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_puget_sound()
#' }
#'
filter_puget_sound <- function(.data){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(36:71)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(76:166)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }

}


#' Filters a dataframe to Washington State fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe generated within this package
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_wa()
#' }
#'
filter_wa <- function(.data){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(16:71)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(23:166)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }

}

#' Filters a dataframe to Canadian (BC) fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe generated within this package
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_bc()
#' }
#'
filter_bc <- function(.data){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(4:15)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(167:193)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }

}

#' Filters a dataframe to Alaska fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe generated within this package
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_ak()
#' }
#'
filter_ak <- function(.data){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(1:3)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(194:198)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }
}


#' Filters a dataframe to California fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe generated within this package
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_ca()
#' }
#'
filter_ca <- function(.data){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(32:34)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(1:8)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }
}

#' Filters a dataframe to Oregon fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe generated within this package
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_or()
#' }
#'
filter_or <- function(.data){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(28:33)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(5:32)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }
}


#' Filters a dataframe to Coastal fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe generated within this package
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_coast()
#' }
#'
filter_coast <- function(.data, species = NULL){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(1:35)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(1:22, 33:75)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }
}

#' Filters a dataframe to marine fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe generated within this package
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_marine()
#' }
#'
filter_marine <- function(.data, species = NULL){
  if(!'fishery_id' %in% colnames(.data)){
    rlang::abort('fishery_id column must be present in dataframe.')
  }

  if(attr(.data, 'species') == 'CHINOOK'){
    .data |>
      dplyr::filter(
        !.data$fishery_id %in% c(72:73)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(
          3:8, 15:22, 33:50,
          79:83, 87:88, 91:93,
          96:97, 101:102, 105:107,
          109:112, 115, 118:124,
          129:133, 136:146, 152:160,
          170:198)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }
}
