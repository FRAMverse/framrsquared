#' `r lifecycle::badge("experimental")` Parse TAAETRS table
#'
#' Terminal run information used by FRAM is stored in the TAAETRSList and (soon) the
#' TAAETRSListChinook tables, but stored in a way that is not very human readable.
#' `parse_terminal_info()` translates this to human-readable form, primarily to then be used by
#' [terminal_stocks()] and [terminal_fisheries()].
#'
#' @param fram_db Fram database object
#' @param species "COHO" or "CHINOOK". Optional, defaults to the database species. Provide this only
#'   if fram_db connects to a database with both Chinook and Coho information. And try to avoid that
#'   -- those databases are sketchy to work with.
#' @param suppress_label_warning Suppress warning about fishery and stock labels in the absence of a "stock_version" handling in the TAAETRS tables? Primarily here to silence that message during unit testing. Logical, defaults to FALSE.
#'
#' @returns tibble of TAAETRSList or TAAETRSListChinook tables translated to long form. `$taa_name` and `taa_num` identify the "TAA" group, `$stock_label` and `$stock_id` identify the FRAM stock, `$terminal_time_steps` and `$terminal_months` give the time periods that this stock is terminal, and `$fishery_label` and `$fishery_id` identify the fishery for which the stock is terminal.
#'
#' @export
#' @seealso [terminal_stocks()], [terminal_fisheries()]
#' @examples \dontrun{fram_db |> parse_terminal_info()}
terminal_info <- function(fram_db, species = NULL, suppress_label_warning = FALSE){

  validate_fram_db(fram_db, db_type = "full")

  if(!is.null(species)){
    species <- standardize_species(species)
  } else {
    species_present <- fram_database_species(fram_db)
    if(length(species_present) == 1){
      species = species_present
    }
  }

  if(species == "CHINOOK"){

    table_name = "TAAETRSListChinook"

  } else if(species == "COHO") {

    table_name = "TAAETRSList"

  }

  db_tables <- get_tables(fram_db)

  if(! table_name %in% db_tables){
    fram_abort("{.emph {table_name}} must be in FRAM database!")
  }

  timesteps <-  fetch_table(fram_db, "TimeStep") |>
    dplyr::mutate(timestep_start = gsub(" - .*", "", .data$time_step_title),
                  timestep_end = gsub(".* - ", "", .data$time_step_title)
    ) |>
    dplyr::rename(time_step = "time_step_id")

  tab <- fram_db |>
    fetch_table_(table_name) |>
    dplyr::select("taa_num",
                  "taa_stk_list",
                  "taa_fish_list", "taa_time_step1", "taa_time_step2",
                  "taa_type", "taa_name")

  if(! suppress_label_warning){
    cli::cli_alert_danger("Until FRAM has been updated to support the inclusion of `stock_version` to TAAETRS table, take fishery and stock labels from this function with a grain of salt.")
  }

  tab |>
    dplyr::mutate(taa_stk_list = stringr::str_split(.data$taa_stk_list, ","),
                  taa_fish_list = stringr::str_split(.data$taa_fish_list, ",")) |>
    tidyr::unnest("taa_stk_list") |>
    tidyr::unnest("taa_fish_list") |>
    dplyr::rename(stock_id = "taa_stk_list",
                  fishery_id = "taa_fish_list") |>
    dplyr::mutate(stock_id = as.numeric(.data$stock_id),
                  fishery_id = as.numeric(.data$fishery_id)) |>
    dplyr::filter(.data$fishery_id != 0) |>
    dplyr::mutate(terminal_time_steps = glue::glue("{taa_time_step1}-{taa_time_step2}")) |>
    dplyr::left_join(timesteps |>
                       dplyr::filter(.data$species == .env$species) |>
                       dplyr::select(taa_time_step1 = "time_step", "timestep_start"),
                     by = "taa_time_step1") |>
    dplyr::left_join(timesteps |>
                       dplyr::filter(.data$species == .env$species) |>
                       dplyr::select(taa_time_step2 = "time_step", "timestep_end"),
                     by = "taa_time_step2") |>
    dplyr::mutate(terminal_months = glue::glue("{timestep_start}-{timestep_end}")) |>
    framrosetta::label_fisheries(species = species) |>
    framrosetta::label_stocks(species = species) |>
    dplyr::select("taa_name", "taa_num", "stock_label", "stock_id", "terminal_time_steps", "terminal_months", "fishery_label", "fishery_id")
}

#' `r lifecycle::badge("experimental")` List terminal stock information
#'
#' For each TAA group, lists the associated FRAM stocks and timesteps. Intended to support working
#' with bios for QAQC.
#'
#' @inheritParams terminal_info
#'
#' @return Tibble of taa stocks and timesteps
#' @export
#'
#' @examples \dontrun{fram_db |> terminal_stocks()}
terminal_stocks <- function(fram_db, species = NULL, suppress_label_warning = FALSE){
  validate_fram_db(fram_db, db_type = "full")
  terminal_info(fram_db, species = species, suppress_label_warning = suppress_label_warning) |>
    dplyr::select("taa_name", "stock_label", "terminal_months", "stock_id", "terminal_time_steps") |>
    dplyr::distinct()
}

#' `r lifecycle::badge("experimental")`
#' List terminal stock information
#'
#' For each TAA, lists the associated fisheries. Intended to support working with bios for QAQC.
#'
#' @inheritParams terminal_info
#'
#' @returns Tibble of taa fisheries
#' @export
#'
#' @examples \dontrun{fram_db |> terminal_fisheries()}
terminal_fisheries <- function(fram_db, species = NULL, suppress_label_warning = FALSE){
  validate_fram_db(fram_db, db_type = "full")
  terminal_info(fram_db, species = species, suppress_label_warning = suppress_label_warning) |>
    dplyr::select("taa_name", "fishery_label", "fishery_id") |>
    dplyr::distinct()
}
