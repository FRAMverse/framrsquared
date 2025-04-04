#' Filters a dataframe to sport fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @param .data Dataframe containing `fishery_id` column. Commonly, output from `framrsquared::fetch_table()`.
#' @param species Optional argument to identify species if `.data` doesn't already. If provided, must be "COHO" or "CHINOOK". Defaults to `NULL`
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_sport()
#' }
#' framrosetta::fishery_chinook_fram |> filter_sport(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_sport(species = "COHO")
#'
filter_sport <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(
          3, 8, 11, 13, 14, 15,
          18, 22, 27, 29, 31, 33,
          35, 48, 60, 62, 72, 36,
          42, 45, 53, 54, 56, 57,
          64, 67
        )
      )
  } else if (species == "COHO") {
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
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}

#' Filters a dataframe to net fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#'
#' @inheritParams filter_sport
#'
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_net()
#' }
#' framrosetta::fishery_chinook_fram |> filter_net(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_net(species = "COHO")
filter_net <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  # if it's not sport it must be net
  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        !.data$fishery_id %in% c(
          3, 8, 11, 13, 14, 15,
          18, 22, 27, 29, 31, 33,
          35, 48, 60, 62, 72, 36,
          42, 45, 53, 54, 56, 57,
          64, 67
        )
      )
  } else if (species == "COHO") {
    .data |>
      dplyr::filter(
        !.data$fishery_id %in% c(
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
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}




#' Filters a dataframe to Puget Sound fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_puget_sound()
#' }
#' framrosetta::fishery_chinook_fram |> filter_puget_sound(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_puget_sound(species = "COHO")
filter_puget_sound <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(36:71)
      )
  } else if (species == "COHO") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(76:166)
      )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}


#' Filters a dataframe to Washington State fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_wa()
#' }
#' framrosetta::fishery_chinook_fram |> filter_wa(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_wa(species = "COHO")
filter_wa <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(16:71)
      )
  } else if (species == "COHO") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(23:166)
      )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}

#' Filters a dataframe to Canadian (BC) fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_bc()
#' }
#' framrosetta::fishery_chinook_fram |> filter_bc(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_bc(species = "COHO")
filter_bc <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(4:15)
      )
  } else if (species == "COHO") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(167:193)
      )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}

#' Filters a dataframe to Alaska fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_ak()
#' }
#' framrosetta::fishery_chinook_fram |> filter_ak(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_ak(species = "COHO")
filter_ak <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(1:3)
      )
  } else if (species == "COHO") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(194:198)
      )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}


#' Filters a dataframe to California fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_ca()
#' }
#' framrosetta::fishery_chinook_fram |> filter_ca(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_ca(species = "COHO")
filter_ca <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(32:34)
      )
  } else if (species == "COHO") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(1:8)
      )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}

#' Filters a dataframe to Oregon fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_or()
#' }
#' framrosetta::fishery_chinook_fram |> filter_or(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_or(species = "COHO")
filter_or <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(28:33)
      )
  } else if (species == "COHO") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(5:32)
      )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}


#' Filters a dataframe to Coastal fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_coast()
#' }
#' framrosetta::fishery_chinook_fram |> filter_coast(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_coast(species = "COHO")
filter_coast <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(1:35)
      )
  } else if (species == "COHO") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(1:22, 33:75)
      )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}

#' Filters a dataframe to marine fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. Requires
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @examples
#' \dontrun{
#' fram_dataframe |> filter_marine()
#' }
#' framrosetta::fishery_chinook_fram |> filter_marine(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_marine(species = "COHO")
filter_marine <- function(.data, species = NULL) {
  validate_data_frame(.data)

  if (!"fishery_id" %in% colnames(.data)) {
    cli::cli_abort("fishery_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    .data |>
      dplyr::filter(
        !.data$fishery_id %in% c(72:73)
      )
  } else if (species == "COHO") {
    .data |>
      dplyr::filter(
        .data$fishery_id %in% c(
          3:8, 15:22, 33:50,
          79:83, 87:88, 91:93,
          96:97, 101:102, 105:107,
          109:112, 115, 118:124,
          129:133, 136:146, 152:160,
          170:198
        )
      )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}
