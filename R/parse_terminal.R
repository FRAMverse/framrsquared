#' `r lifecycle::badge("experimental")`
#' Parse TAAETRS table
#'
#' Terminal run information used by FRAM is stored in the TAAETRSList and (soon) the TAAETRSListChinook tables, but stored in a way that is not very human readable. `parse_terminal_info()` translates this to human-readable form, primarily to then be used by [terminal_stocks()] and [terminal_fisheries()].
#'
#' @param fram_db Fram database object
#' @param old_table_name Logical, defaults to TRUE. We intend to change the FRAM table from TAAETRSList to TAAETRSListChinook to avoid confusion. When working with a database where that hasn't been done, leave this argument to TRUE.
#' @param species "COHO" or "CHINOOK". Optional, defaults to the database species. Provide this only if fram_db connects to a database with both Chinook and Coho information. And try to avoid that -- those databases are sketchy to work with.
#'
#' @return tibble of TAAETRSList or TAAETRSListChinook tables translated to long form.
#' @export
#' @seealso [terminal_stocks()], [terminal_fisheries()]
#' @examples \dontrun{fram_db |> parse_terminal_info()}
terminal_info <- function(fram_db, old_table_name = TRUE, species = NULL){

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

    if(old_table_name){
      table_name = "TAAETRSList"
    } else {
      table_name = "TAAETRSListChinook"
    }

    timesteps <-  framrosetta::timestep_chinook_fram |>
      dplyr::mutate(timestep_start = gsub(" - .*", "", .data$time_step_title),
                    timestep_end = gsub(".* - ", "", .data$time_step_title)
      ) |>
      dplyr::rename(time_step = .data$time_step_id)

    tab <- fram_db |>
      fetch_table_(table_name) |> # currently, this table does not exist
      dplyr::select("taa_num",
                    "taa_stk_list",
                    "taa_fish_list", "taa_time_step1", "taa_time_step2",
                    "taa_type", "taa_name")

  } else if(species == "COHO") {

    timesteps <-  framrosetta::timestep_coho_fram |>
      dplyr::mutate(timestep_start = gsub(" - .*", "", .data$time_step_title),
                    timestep_end = gsub(".* - ", "", .data$time_step_title)
      ) |>
      dplyr::rename(time_step = .data$time_step_id)

    tab <- fram_db |>
      fetch_table_("TAAETRSList") |>
      dplyr::select("taa_num",
                    "taa_stk_list",
                    "taa_fish_list", "taa_time_step1", "taa_time_step2",
                    "taa_type", "taa_name")


  }

  tab |>
    dplyr::mutate(taa_stk_list = stringr::str_split(.data$taa_stk_list, ","),
                  taa_fish_list = stringr::str_split(.data$taa_fish_list, ",")) |>
    tidyr::unnest(.data$taa_stk_list) |>
    tidyr::unnest(.data$taa_fish_list) |>
    dplyr::rename(stock_id = .data$taa_stk_list,
                  fishery_id = .data$taa_fish_list) |>
    dplyr::mutate(stock_id = as.numeric(.data$stock_id),
                  fishery_id = as.numeric(.data$fishery_id)) |>
    dplyr::filter(.data$fishery_id != 0) |>
    dplyr::mutate(terminal_time_steps = glue::glue("{taa_time_step1}-{taa_time_step2}")) |>
    dplyr::left_join(timesteps |>
                       dplyr::select(taa_time_step1 = .data$time_step, .data$timestep_start),
                     by = "taa_time_step1") |>
    dplyr::left_join(timesteps |>
                       dplyr::select(taa_time_step2 = .data$time_step, .data$timestep_end),
                     by = "taa_time_step2") |>
    dplyr::mutate(terminal_months = glue::glue("{timestep_start}-{timestep_end}")) |>
    framrosetta::label_fisheries(species = species) |>
    framrosetta::label_stocks(species = species) |>
    dplyr::select("taa_name", "taa_num", "stock_label", "stock_id", "terminal_time_steps", "terminal_months", "fishery_label", "fishery_id")
}

#' `r lifecycle::badge("experimental")`
#' List terminal stock information
#'
#' For each TAA, lists the associated FRAM stocks and timesteps.
#'
#' @inheritParams terminal_info
#'
#' @return taa of taa stocks and timesteps
#' @export
#'
#' @examples \dontrun{fram_db |> terminal_stocks()}
terminal_stocks <- function(fram_db, species = NULL){
  validate_fram_db(fram_db, db_type = "full")
  terminal_info(fram_db, species = species) |>
    dplyr::select("taa_name", "stock_label", "terminal_months", "stock_id", "terminal_time_steps") |>
    dplyr::distinct()
}

#' `r lifecycle::badge("experimental")`
#' List terminal stock information
#'
#' For each TAA, lists the associated fisheries
#'
#' @inheritParams terminal_info
#'
#' @return tibble of taa fisheries
#' @export
#'
#' @examples \dontrun{fram_db |> terminal_fisheries()}
terminal_fisheries <- function(fram_db, species = NULL){
  validate_fram_db(fram_db, db_type = "full")
  terminal_info(fram_db, species = species) |>
    dplyr::select("taa_name", "fishery_label", "fishery_id") |>
    dplyr::distinct()
}
