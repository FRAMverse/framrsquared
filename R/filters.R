#' Filters a dataframe to sport fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @param .data Dataframe containing `fishery_id` column. Commonly, output from `framrsquared.dev::fetch_table()`.
#' @param species Optional argument to identify species if `.data` doesn't already. If provided, must be "COHO" or "CHINOOK" or variations thereof. Defaults to `NULL`
#' @param return_ids Return the fishery ids used in filtering rather than a filtered dataframe?
#' Logical, defaults to FALSE
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_sport(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_sport(species = "COHO")
#'
filter_sport <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec =  c(
      3, 8, 11, 13, 14, 15,
      18, 22, 27, 29, 31, 33,
      35, 48, 60, 62, 72, 36,
      42, 45, 53, 54, 56, 57,
      64, 67
    )
  } else if (species == "COHO") {
    fishery_vec = c(
      3, 5, 7, 15, 17, 19, 21, 23, 24,
      28, 29, 31, 33, 37, 40, 41, 45, 46,
      48, 49, 51, 54, 58, 59, 60, 61, 62,
      65, 66, 67, 70, 73, 76, 89, 90, 91,
      92, 93, 94, 95, 99, 100, 106, 107,
      108, 115, 116, 117, 118, 127, 128, 129,
      135, 136, 149, 150, 151, 152, 163,
      164, 165, 166, 169, 186, 187, 188,
      189, 190, 191, 192, 193
    )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }

}

#' Filters a dataframe to net fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#'
#' @inheritParams filter_sport
#'
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_net(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_net(species = "COHO")
filter_net <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  # if it's not sport it must be net
  if (species == "CHINOOK") {
    fishery_vec = c(
      3, 8, 11, 13, 14, 15,
      18, 22, 27, 29, 31, 33,
      35, 48, 60, 62, 72, 36,
      42, 45, 53, 54, 56, 57,
      64, 67
    )
  } else if (species == "COHO") {
    fishery_vec = c(
      3, 5, 7, 15, 17, 19, 21, 23, 24,
      28, 29, 31, 33, 37, 40, 41, 45, 46,
      48, 49, 51, 54, 58, 59, 60, 61, 62,
      65, 66, 67, 70, 73, 76, 80, 89, 90, 91,
      92, 93, 94, 95, 99, 100, 106, 107,
      108, 115, 116, 117, 118, 127, 129,
      135, 136, 149, 150, 151, 152, 163,
      164, 165, 166, 169, 186, 187, 188,
      189, 190, 191, 192, 193
    )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }

}


#' Filters a dataframe to Puget Sound fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_puget_sound(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_puget_sound(species = "COHO")
filter_puget_sound <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec = c(36:71)
  } else if (species == "COHO") {
    fishery_vec = c(76:166)
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }
}


#' Filters a dataframe to Washington State fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_wa(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_wa(species = "COHO")
filter_wa <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec = c(16:29, 36:71)
  } else if (species == "COHO") {
    fishery_vec = c(23:166)
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }

}

#' Filters a dataframe to Canadian (BC) fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_bc(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_bc(species = "COHO")
filter_bc <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec = c(4:15)
  } else if (species == "COHO") {
    fishery_vec = c(167:193)
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }

}

#' Filters a dataframe to Alaska fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_ak(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_ak(species = "COHO")
filter_ak <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec = c(1:3)
  } else if (species == "COHO") {
    fishery_vec = c(194:198)
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }

}


#' Filters a dataframe to California fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_ca(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_ca(species = "COHO")
filter_ca <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec =  c(32:34)
  } else if (species == "COHO") {
    fishery_vec =  c(1:8)
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }

}

#' Filters a dataframe to Oregon fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_or(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_or(species = "COHO")
filter_or <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec =  c(28:33)
  } else if (species == "COHO") {
    fishery_vec = c(5:32)
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }
}


#' Filters a dataframe to Coastal fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_coast(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_coast(species = "COHO")
filter_coast <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec = c(1:35)
  } else if (species == "COHO") {
    fishery_vec =  c(1:22, 33:75)
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }
}

#' Filters a dataframe to marine fisheries. Will
#' automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_marine(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_marine(species = "COHO")
filter_marine <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec = 1:71
  } else if (species == "COHO") {
    fishery_vec = c(
      3:8, 15:22, 33:50,
      79:83, 87:88, 91:93,
      96:97, 101:102, 105:107,
      109:112, 115, 118:124,
      129:133, 136:146, 152:160,
      170:198
    )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }
}

#' Filters a dataframe to WA non-treaty commercial fisheries.
#'
#' Will automatically detect whether it's working with a Chinook or Coho
#' dataset if the tables were generated within this package. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_chinook_fram |> filter_commercial_wa_nt(species = "CHINOOK")
#' framrosetta::fishery_coho_fram |> filter_commercial_wa_nt(species = "COHO")

filter_commercial_wa_nt <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    fishery_vec = c(58, 65, 68, 70, 37, 39, 43, 46, 49, 51)

  } else if (species == "COHO") {
    fishery_vec =  c(80, 82, 87, 96, 101, 109, 111, 119, 121, 123, 130, 132, 137, 139, 141, 143, 145, 153, 155, 157, 159)
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }
}

## New filters, not complete ==================================================

#' Filters a Coho dataframe to STT fisheries
#'
#' Currently only works on Coho datasets. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_coho_fram |> filter_stt()

filter_stt <- function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    cli::cli_abort("`filter_stt` not defined for Chinook")
  } else if (species == "COHO") {
    fishery_vec = c(33, 37, 40, 41, # Ocean Sport
                    34, 35, 38, 42, # NT Troll
                    36, 39, 43, # Treaty Troll
                    17, 18, 19, 29, 21, 22 # Oregon fisheries
    )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }
}

#' Filters a Coho dataframe to non-treaty STT fisheries
#'
#' Currently only works on Coho datasets. `.data` must have
#' a `fishery_id` column name.
#' @inheritParams filter_sport
#' @export
#' @family fishery_filters
#' @examples
#' framrosetta::fishery_coho_fram |> filter_stt_nt()
filter_stt_nt <-  function(.data, species = NULL, return_ids = FALSE) {
  validate_fishery_filter_inputs(.data, species, return_ids)
  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    cli::cli_abort("`filter_stt_nt` not defined for Chinook")
  } else if (species == "COHO") {
    fishery_vec = c(33, 37, 40, 41, # Ocean Sport
                    34, 35, 38, 42, # NT Troll
                    17, 18, 19, 29, 21, 22 # Oregon fisheries
    )
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(fishery_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$fishery_id %in% fishery_vec)
    )
  }
}

## Coho stocks for Marlene + Steph
## primary function for coho stock filtering
filter_coho_stocks <- function(.data, species = NULL, return_ids = FALSE,
                               filter_criterion){

  if (!"stock_id" %in% colnames(.data)) {
    cli::cli_abort("stock_id column must be present in dataframe.")
  }

  species <- validate_species(.data, species)

  if (species == "CHINOOK") {
    cli::cli_abort("This function not defined for Chinook")
  } else if (species == "COHO") {
    stock_vec = coho_stock_marlene |>
      dplyr::filter(.data$stock_type %in% filter_criterion) |>
      dplyr::pull(.data$stock_id)
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }

  if(return_ids){
    return(stock_vec)
  } else {
    return(.data |>
             dplyr::filter(.data$stock_id %in% stock_vec)
    )
  }
}


#' Filters a Coho stock dataframe to hatchery stocks
#'
#' Currently only works on Coho datasets. `.data` must have
#' a `stock_id` column name.
#'
#' @inheritParams filter_sport
#' @param return_ids Return the stock ids used in filtering rather than a filtered dataframe?
#' Logical, defaults to FALSE
#' @export
#' @family stock_filters
#' @examples
#' framrosetta::stock_coho_fram |> filter_hatchery()
filter_hatchery <- function(.data, species = NULL, return_ids = FALSE) {
  validate_data_frame(.data)
  validate_flag(return_ids)

  filter_coho_stocks(.data, species = species, return_ids = return_ids,
                     filter_criterion = "Hatchery")
}

#' Filters a Coho stock dataframe to wild stocks
#'
#' Currently only works on Coho datasets. `.data` must have
#' a `stock_id` column name.
#'
#' @inheritParams filter_hatchery
#' @export
#' @family stock_filters
#' @examples
#' framrosetta::stock_coho_fram |> filter_wild()
filter_wild <- function(.data, species = NULL, return_ids = FALSE) {
  validate_data_frame(.data)
  validate_flag(return_ids)

  filter_coho_stocks(.data, species = species, return_ids = return_ids,
                     filter_criterion = "Wild")
}

#' Filters a Coho stock dataframe to mixed stocks
#'
#' Currently only works on Coho datasets. `.data` must have
#' a `stock_id` column name.
#'
#' @inheritParams filter_hatchery
#' @export
#' @family stock_filters
#' @examples
#' framrosetta::stock_coho_fram |> filter_filter_mixed()
filter_mixed <- function(.data, species = NULL, return_ids = FALSE) {
  validate_data_frame(.data)
  validate_flag(return_ids)

  filter_coho_stocks(.data, species = species, return_ids = return_ids,
                     filter_criterion = "Mix")
}


categorize_stock <- function(.data, species = NULL) {
  species <- validate_species(.data, species)

  if (!"stock_id" %in% colnames(.data)) {
    cli::cli_abort("stock_id column must be present in dataframe.")
  }

  if (species == "CHINOOK") {
    cli::cli_abort("`categorize_stock` not defined for Chinook")
  } else if (species == "COHO") {
    .data |>
      dplyr::left_join(coho_stock_marlene,
                       by = "stock_id")
  } else {
    cli::cli_abort('`species` must be "COHO" or "CHINOOK", not "{species}".')
  }
}
