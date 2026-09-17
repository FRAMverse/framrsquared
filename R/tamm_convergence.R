## helper function to parse the "TAA" lines of the FramCheck.txt file.
parse_taa_line <- function(x){
  trs_type = dplyr::if_else(stringr::str_detect(x, "^TAAQuota"),
                            "TAA",
                            "ETRS")
  x <- gsub("(TAAQuota-)|(TAA-ETRS-)", "", x)
  fishery_name  <-  gsub(" [0-9][0-9][0-9] .*", "", x)
  remainder <- gsub(paste0(fishery_name, " "), "", x)
  remainder_parsed <- as.numeric(stringr::str_split(remainder, " +")[[1]])

  res <- data.frame(
    fishery_name = fishery_name,
    ## the following are base on reading FRAM Source Code,
    ## `FramCalcs.vb`, loop starting with "Scale Terminal Fisheries using Terminal Runsizes
    fishery_id = remainder_parsed[1],
    trs_type = trs_type,
    time_step = remainder_parsed[2],
    quota = remainder_parsed[3],
    target_local = dplyr::if_else(length(remainder_parsed) == 4,
                                  remainder_parsed[4],
                                  NA_real_)
  )

  return(res)
}

## helper function to get the species from a FramCheck.txt file
##  NOTE: this is based on specific debug choices defined in FRAM source code,
##  in which Chinook gets some additional info printed. This is stable in theory,
##  but as this is arbitrary, changes to FRAM source code could render this useless.
get_framcheck_species <- function(filepath){

  validate_path(filepath)

  raw <- readLines(filepath)

  ## check that pattern matches Coho check file
  ref_lines = c("=================== CompCatch Results ========================",
                "--------Stk Age Fsh TSp LandCatch Cohort     BaseExpRt FishScl StkFshI LegalProp",
                "=================== CompCatch Results ========================")

  if(all(raw[3:5] == ref_lines)){
    species = "COHO"
  } else {
    species = "CHINOOK"
  }
  return(species)
}

## helper fun to get TAMM name
get_framcheck_tamm <- function(filepath){
  validate_path(filepath)

  raw <- readLines(filepath)
  tamm_raw <- stringr::str_subset(raw, "^Tamm Input File =")
  return(basename(tamm_raw))
}


#' Parse FramCheck.txt file for TAMM iteration information
#'
#' `r lifecycle::badge("experimental")`
#'
#' Reads a `FramCheck.txt` file and extracts TAMM iteration data for
#' terminal run sizes (TRS) and TAA/ETRS harvest quotas. Currently only
#' implemented for Coho.
#'
#' In the output, `$trs` dataframe shows the TRS abundances based on the groups of `TAAETRSList` table in the FRAM db, with `$trs_id` mapping to the `TaaNum` column of `TAAETRSList`. The `total` column is the total abundance in fish. The `$taa_etrs` dataframe shows the quotas for each of the fisheries that are modeled through TAMM iterations, for each of the two "terminal" timesteps (4 and 5). Fisheries with a TAA-based havest rate will have `NA` for the `$target_local` column. For ETRS-based harvest rate fisheries, `target_local` is an intermediate value in the calculation that gets printed for diagnostic purposes. (DEV NOTE: to view the code for these, search for "TAA-ETRS-").
#'
#' @param filepath Path to a `FramCheck.txt` file.
#'
#' @returns A named list with elements:
#'   \describe{
#'     \item{species}{Character string identifying the species (`"COHO"` or `"CHINOOK"`).}
#'     \item{tamm_name}{Basename of the associated TAMM input file.}
#'     \item{trs}{Data frame of terminal run size values per TRS ID and TAMM iteration.}
#'     \item{taa_etrs}{Data frame of TAA/ETRS quota values per fishery and TAMM iteration.}
#'   }
#'
#' @export
#' @family tamm convergence
#'
#' @examples
#' \dontrun{
#' parse_fram_check("path/to/FramCheck.txt")
#' }
parse_fram_check <- function(filepath){
  validate_path(filepath)

  raw <- readLines(filepath)

  species = get_framcheck_species(filepath)
  tamm_name = get_framcheck_tamm(filepath)

  if(!species == "COHO"){
    fram_abort("`parse_fram_check()` is not currently implemented for Chinook FramCheck.Txt files!")
  }

  start_inds <- which(stringr::str_detect(raw, " TAMM Iteration"))
  end_inds <- which(stringr::str_detect(raw, "=================== CompCatch Results"))


  trs_final_ls = list()
  taa_final_ls = list()
  for(i in 1:length(start_inds)) {

    cur_start = start_inds[i]

    ## find first end_ind that's after the current start ind
    cur_end = min(end_inds[end_inds - cur_start > 0])

    cur_iteration = readr::parse_number(raw[cur_start])
    cur_dat <- raw[(cur_start+1):(cur_end-1)]

    cur_trs_ls = cur_dat[stringr::str_detect(cur_dat, "^TRS#=")] |>
      stringr::str_split(pattern = " ")

    cur_trs_dat <- data.frame(do.call(rbind, cur_trs_ls))
    names(cur_trs_dat) = c("trs_raw", "escape_raw", "total_raw")
    cur_trs_dat <- cur_trs_dat |>
      dplyr::mutate(tamm_iteration = .env$cur_iteration,
                    trs_id = readr::parse_number(.data$trs_raw),
                    escapement = readr::parse_number(.data$escape_raw),
                    total = readr::parse_number(.data$total_raw),
      ) |>
      dplyr::select("tamm_iteration", "trs_id", "escapement", "total")

    trs_final_ls[[i]] = cur_trs_dat

    ## taa lines
    cur_taa_dat <- cur_dat[stringr::str_detect(cur_dat, "^TAA")]
    taa_final_ls[[i]] <-  purrr::map_df(cur_taa_dat, parse_taa_line) |>
      dplyr::mutate(tamm_iteration = .env$cur_iteration, .before = "fishery_name")

  }
  trs_final_df <- do.call(rbind, trs_final_ls)
  taa_final_df <- do.call(rbind, taa_final_ls)
  return(list(species = species,
              tamm_name = tamm_name,
              trs = trs_final_df,
              taa_etrs = taa_final_df))
}



#' Check TAMM iteration convergence
#'
#' `r lifecycle::badge("experimental")`
#'
#' Evaluates how much terminal run sizes (TRS) and TAA/ETRS quotas changed
#' between the final two TAMM iterations in a `FramCheck.txt` file. Optionally
#' prints a summary to the console. Currently only implemented for Coho.
#'
#' For both of the ouput objects, the "diffs" are based on final two iterations, with `$change_in_fish` and `$change_in_quota` giving the number in raw fish. `relative_change` is calculated as the difference in the final two counts (TRS or abundance) divided by their average. `trs_diff$trs_id` corresponds to the `TaaNum` column of the `TAAETRSList` table in the FRAM database.
#'
#' @param filepath Path to a `FramCheck.txt` file.
#' @param threshold_fish Numeric scalar. Minimum absolute change in fish (or
#'   quota) between the last two iterations to flag as a potential convergence
#'   problem. Defaults to `20`.
#' @param quiet Logical. If `TRUE`, suppresses console output. Defaults to
#'   `FALSE`.
#'
#' @returns Invisibly, a named list with elements:
#'   \describe{
#'     \item{trs_diff}{Data frame of per-TRS change and relative_change between the final two iterations.}
#'     \item{taa_etrs_diff}{Data frame of per-fishery quota change and relative_change between the final two iterations.}
#'   }
#'
#' @export
#' @family tamm convergence
#'
#' @examples
#' \dontrun{
#' check_tamm_convergence("path/to/FramCheck.txt", threshold_fish = 10)
#' }
check_tamm_convergence <- function(filepath,
                                   threshold_fish = 20, #threshold for messaging in cli
                                   quiet = FALSE){ #print to cli?

  validate_path(filepath)
  validate_numeric(threshold_fish, n = 1)
  validate_flag(quiet)

  species <-  get_framcheck_species(filepath)
  tamm_name <- get_framcheck_tamm(filepath)

  if(!species == "COHO"){
    fram_abort("`check_tamm_convergence()` is not currently implemented for Chinook FramCheck.Txt files!")
  }

  if(!quiet){
    cli::cli_h1("Checking TAMM iteration convergence of {.file {filepath}}")
    cli::cli_alert_info("Associated with tamm {.file {tamm_name}}")
  }

  trs_info <-parse_fram_check(filepath)

  max_iter = max(trs_info$trs$tamm_iteration)

  trs_final_diff <- trs_info$trs |>
    dplyr::filter(.data$tamm_iteration %in% (max_iter-1):max_iter) |>
    dplyr::summarize(change_in_fish = diff(.data$total),
                     relative_change = diff(.data$total)/mean(.data$total), .by = c("trs_id")) |>
    dplyr::mutate(relative_change = dplyr::if_else(.data$change_in_fish == 0,
                                         0,
                                         .data$relative_change)) |>
    dplyr::arrange(dplyr::desc(abs(.data$change_in_fish)))

  trs_problems <- trs_final_diff |>
    dplyr::filter(abs(.data$change_in_fish) >= threshold_fish)

  taa_final_diff <- trs_info$taa_etrs |>
    dplyr::filter(.data$tamm_iteration %in% (max_iter-1):max_iter) |>
    dplyr::summarize(change_in_harvest = diff(.data$quota),
                     relative_change = diff(.data$quota)/mean(.data$quota),
                     .by = c("fishery_name", "fishery_id", "time_step", "trs_type")) |>
    dplyr::mutate(relative_change = dplyr::if_else(.data$change_in_harvest == 0,
                                         0,
                                         .data$relative_change)) |>
    dplyr::arrange(dplyr::desc(abs(.data$change_in_harvest)))

  taa_problems <- taa_final_diff |>
    dplyr::filter(abs(.data$change_in_harvest) >= threshold_fish)

  if(!quiet){

    cli::cli_h2("How much did Terminal Run Sizes change between penultimate and ultimate iterations?")
    if(nrow(trs_problems)>0){
      cli::cli_alert_warning("{nrow(trs_problems)} terminal run sizes changed by at least {threshold_fish} fish between the final two iterations!!")
      print(trs_problems)
    }

    cli::cli_h2("How much did Harvest Rate based harvests change between penultimate and ultimate iterations?")
    if(nrow(trs_problems)>0){
      cli::cli_alert_warning("{nrow(taa_problems)} quotas changed by at least {threshold_fish} fish between the final two iterations!!")
      print(taa_problems)
    }
  }

  return(invisible(list(trs_diff = trs_final_diff,
                        taa_etrs_diff = taa_final_diff)))
}


#' Plot TAMM convergence for terminal run sizes
#'
#' `r lifecycle::badge("experimental")`
#'
#' Plots terminal run size (TRS) values across TAMM iterations for the `n`
#' worst-converging TRS IDs, as measured by the relative change
#' between the final two iterations. Currently only implemented for Coho.
#'
#' @param filepath Path to a `FramCheck.txt` file.
#' @param n Integer scalar. Number of worst-converging TRS IDs to display.
#'   Defaults to `5`.
#' @param split Logical. If `TRUE`, each TRS ID is shown in its own facet with
#'   a free y-axis scale. Defaults to `FALSE`.
#'
#' @returns A `ggplot` object.
#'
#' @export
#' @family tamm convergence
#'
#' @examples
#' \dontrun{
#' plot_tamm_convergence_trs("path/to/FramCheck.txt", n = 8, split = TRUE)
#' }
plot_tamm_convergence_trs <- function(filepath, n = 5, split = FALSE){

  validate_path(filepath)
  validate_numeric(n, n = 1)
  validate_flag(split)

  tamm_name = get_framcheck_tamm(filepath)

  diffs = check_tamm_convergence(filepath, quiet = TRUE)


  trs_to_plot <- diffs$trs_diff |>
    dplyr::arrange(abs(.data$relative_change)) |>
    utils::tail(n) |>
    dplyr::pull("trs_id")

  tamm_info <- parse_fram_check(filepath)

  trs_dat <- tamm_info$trs |>
    dplyr::filter(.data$trs_id %in% .env$trs_to_plot)

  gp <- trs_dat |>
    dplyr::mutate(trs_id = as.factor(.data$trs_id)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$tamm_iteration,
                                 y = .data$total,
                                 group = .data$trs_id))+
    ggplot2::geom_path(ggplot2::aes(col = .data$trs_id))+
    ggplot2::geom_point(ggplot2::aes(fill = .data$trs_id))+
    ggplot2::theme_bw(base_size = 15)+
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.8)))+
    ggplot2::scale_y_continuous(labels = scales::label_comma())+
    ggplot2::labs(x = "TAMM iteration",
                  y = "TRS Size",
                  title = glue::glue("Terminal Run Sizes, worst {n} convergence cases"),
                  subtitle = glue::glue("Run associated with tamm {tamm_name}"),
                  col = "TaaNum",
                  fill = "TaaNum")

  if(split){
    gp = gp +
      ggplot2::facet_wrap(.~.data$trs_id, scales = "free_y")+
      ggplot2::theme(legend.position = "none")
  }

  return(gp)

}

#' Plot TAMM convergence for TAA/ETRS harvest quotas
#'
#' `r lifecycle::badge("experimental")`
#'
#' Plots TAA and ETRS quota values across TAMM iterations for the `n`
#' worst-converging fishery–time-step combinations, as measured by the relative
#' change between the final two iterations. Currently only implemented
#' for Coho.
#'
#' @param filepath Path to a `FramCheck.txt` file.
#' @param n Integer scalar. Number of worst-converging fishery–time-step
#'   combinations to display. Defaults to `5`.
#' @param split Logical. If `TRUE`, each fishery is shown in its own facet with
#'   a free y-axis scale. Defaults to `FALSE`.
#'
#' @returns A `ggplot` object.
#'
#' @export
#' @family tamm convergence
#'
#' @examples
#' \dontrun{
#' plot_tamm_convergence_taa("path/to/FramCheck.txt", n = 8, split = TRUE)
#' }
plot_tamm_convergence_taa <- function(filepath, n = 5, split = FALSE){

  validate_path(filepath)
  validate_numeric(n, n = 1)
  validate_flag(split)

  diffs = check_tamm_convergence(filepath, quiet = TRUE)

  tamm_name = get_framcheck_tamm(filepath)

  taa_to_plot <- diffs$taa_etrs_diff |>
    dplyr::arrange(abs(.data$relative_change)) |>
    utils::tail(n) |>
    dplyr::select("fishery_id", "time_step")

  tamm_info <- parse_fram_check(filepath)

  taa_dat <- tamm_info$taa_etrs |>
    dplyr::inner_join(taa_to_plot,
                      by = c("fishery_id", "time_step"))

  gp = taa_dat |>
    dplyr::mutate(fishery_label = glue::glue("{.data$fishery_name} (ID {.data$fishery_id}) TS {.data$time_step}")) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$tamm_iteration,
                                 y = .data$quota,
                                 group = .data$fishery_label))+
    ggplot2::geom_path(ggplot2::aes(col = .data$fishery_label))+
    ggplot2::geom_point(ggplot2::aes(fill = .data$fishery_label))+
    ggplot2::theme_bw(base_size = 15)+
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.8)))+
    ggplot2::scale_y_continuous(labels = scales::label_comma())+
    ggplot2::labs(x = "TAMM iteration",
                  y = "Quota Size",
                  title = glue::glue("HR-based quotas, worst {n} convergence cases"),
                  subtitle = glue::glue("Run associated with tamm {tamm_name}"),
                  col = "Fishery",
                  fill = "Fishery")

  if(split){
    gp = gp +
      ggplot2::facet_wrap(.~fishery_label, scales = "free_y")+
      ggplot2::theme(legend.position = "none")
  }

  return(gp)


}
