#' Filters a dataframe to sport fisheries (Puget Sound for now). Will
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
        .data$fishery_id %in% c(36, 42, 45, 53, 54, 56, 57, 64, 67)
      )
  } else if(attr(.data, 'species') == 'COHO') {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(91, 92, 93, 106, 107, 115, 118, 129, 136, 152)
      )
  } else {
    rlang::abort('Table metadata missing... Table not generated from this package?')
  }

}
