#' `r lifecycle::badge("experimental")`
#' Parse TAAETRS table
#'
#' Terminal run information used by FRAM is stored in the TAAETRSList and (soon) the TAAETRSListChinook tables, but stored in a way that is not very human readable. `parse_terminal_info()` translates this to human-readable form, primarily to then be used by terminal_stocks() and terminal_fisheries().
#'
#' @param fram_db Fram database object
#' @param species "COHO" or "CHINOOK". Optional, defaults to the database species. Provide this only if fram_db connects to a database with both Chinook and Coho information.
#'
#' @return tibble of TAAETRSList or TAAETRSListChinook tables translated to long form.
#' @export
#' @examples \dontrun{fram_db |> parse_terminal_info()}
parse_terminal_info <- function(fram_db, species = NULL){

  validate_fram_db(fram_db)

  if(!is.null(species)){
    species <- standardize_species(species)
  }

  species_present <- fram_database_species(fram_db)

  if(length(species_present) == 1){
    species = species_present
  }

  if(species == "CHINOOK"){
    timesteps <-  framrosetta::timestep_chinook_fram |>
      dplyr::mutate(timestep_start = gsub(" - .*", "", .data$time_step_title),
                    timestep_end = gsub(".* - ", "", .data$time_step_title)
      ) |>
      dplyr::rename(time_step = .data$time_step_id)

    tab <- fram_db |>
      fetch_table("TAAETRSListChinook") |> # currently, this table does not exist
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
      fetch_table("TAAETRSList") |>
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
    framrosetta::label_fisheries(species = species) |>
    framrosetta::label_stocks(species = species) |>
    dplyr::mutate(terminal_time_steps = glue::glue("{taa_time_step1}-{taa_time_step2}")) |>
    dplyr::left_join(timesteps |>
                       dplyr::select(taa_time_step1 = .data$time_step, .data$timestep_start),
                     by = "taa_time_step1") |>
    dplyr::left_join(timesteps |>
                       dplyr::select(taa_time_step2 = .data$time_step, .data$timestep_end),
                     by = "taa_time_step2") |>
    dplyr::mutate(terminal_months = glue::glue("{timestep_start}-{timestep_end}")) |>
    dplyr::select("taa_name", "taa_num", "stock_label", "stock_id", "terminal_time_steps", "terminal_months", "fishery_label", "fishery_id")
}

#' `r lifecycle::badge("experimental")`
#' List terminal stock information
#'
#' For each TAA, lists the associated FRAM stocks and timesteps.
#'
#' @inheritParams parse_terminal_info
#'
#' @return taa of taa stocks and timesteps
#' @export
#'
#' @examples \dontrun{fram_db |> terminal_stocks()}
terminal_stocks <- function(fram_db, species = NULL){
  parse_terminal_info(fram_db, species) |>
    dplyr::select("taa_name", "stock_label", "terminal_months", "stock_id", "terminal_time_steps") |>
    dplyr::distinct()
}

#' `r lifecycle::badge("experimental")`
#' List terminal stock information
#'
#' For each TAA, lists the associated fisheries
#'
#' @inheritParams parse_terminal_info
#'
#' @return tibble of taa fisheries
#' @export
#'
#' @examples \dontrun{fram_db |> terminal_fisheries()}
terminal_fisheries <- function(fram_db, species = NULL){
  parse_terminal_info(fram_db, species) |>
    dplyr::select("taa_name", "fishery_label", "fishery_id") |>
    dplyr::distinct()
}


# taa_parsed = tab |>
#   mutate(taa_stk_list = str_split(taa_stk_list, ","),
#          taa_fish_list = str_split(taa_fish_list, ",")) |>
#   unnest(taa_stk_list) |>
#   unnest(taa_fish_list) |>
#   rename(stock_id = taa_stk_list,
#          fishery_id = taa_fish_list) |>
#   mutate(across(c(stock_id, fishery_id), ~as.numeric(.x))) |>
#   left_join(framrosetta::stock_coho_fram |>
#               select(stock_id, stock_long_name),
#             by = "stock_id") |>
#   left_join(framrosetta::fishery_coho_fram |>
#               select(fishery_id, fishery_title),
#             by = "fishery_id") |>
#   mutate(terminal_time_steps = glue("{taa_time_step1}-{taa_time_step2}")) |>
#   left_join(timesteps_coho |>
#               select(taa_time_step1 = time_step,
#                      timestep_start), by = "taa_time_step1") |>
#   left_join(timesteps_coho |>
#               select(taa_time_step2 = time_step,
#                      timestep_end), by = "taa_time_step2") |>
#   mutate(terminal_months = glue("{timestep_start}-{timestep_end}")) |>
#   select(taa_name, taa_num, stock_long_name, stock_id, terminal_time_steps, terminal_months, fishery_title, fishery_id)
#
# taa_stocks_summary = taa_parsed |>
#   select(taa_name, stock_long_name, terminal_months, stock_id, terminal_time_steps) |>
#   distinct()
#
# taa_fishery_summary = taa_parsed |>
#   select(taa_name, fishery_title, fishery_id) |>
#   distinct()
#
# write_csv(taa_parsed, "coho_taa_parsed.csv")
# write_csv(taa_stocks_summary, "coho_taa_stocks.csv")
# write_csv(taa_fishery_summary, "coho_taa_fishery.csv")
