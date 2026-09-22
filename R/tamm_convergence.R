## helper function to parse the "TAA" lines of Coho FramCheck.txt file.
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
#' terminal run sizes (TRS) and TAA/ETRS harvest quotas. In the case of Chinook,
#'
#' For Coho, in the output, `$trs` dataframe shows the TRS abundances based on the groups of
#' `TAAETRSList` table in the FRAM db, with `$trs_id` mapping to the `TaaNum` column of `TAAETRSList`.
#'  The `total` column is the total abundance in fish. The `$taa_etrs` dataframe shows the quotas
#'  for each of the fisheries that are modeled through TAMM iterations, for each of the two
#'  "terminal" timesteps (4 and 5).
#'  Fisheries with a TAA-based havest rate will have `NA` for the `$target_local` column.
#'  For ETRS-based harvest rate fisheries, `target_local` is an intermediate value in the
#'  calculation that gets printed for diagnostic purposes. (DEV NOTE: to view the code for these, search for "TAA-ETRS-").
#'
#' For Chinook, FramCheck provides a less organized hodge-podge of dianostic and debug information.
#' Some of these have clear motivation (e.g., total number of iterations run), while others may reflect
#' one-time problem-solving/FRAM learning experiments that have accidentally been left in.
#' This function function faithfully parses the information, but we do not attempt to justify
#' why some information is included in the check file.
#'
#' @param filepath Path to a `FramCheck.txt` file.
#'
#' @returns A named list with elements.
#'
#' \strong{For Coho:}
#'   \describe{
#'     \item{species}{Character string identifying the species (`"COHO"` or `"CHINOOK"`).}
#'     \item{tamm_name}{Basename of the associated TAMM input file.}
#'     \item{trs}{Data frame of terminal run size values per TRS ID and TAMM iteration.}
#'     \item{taa_etrs}{Data frame of TAA/ETRS quota values per fishery and TAMM iteration.}
#'   }
#'
#'   \describe{
#' \strong{For Chinook:}
#'     \item{species}{Character string identifying the species (`"COHO"` or `"CHINOOK"`).}
#'     \item{tamm_name}{Basename of the associated TAMM input file.}
#'     \item{final_iteration_count}{Number presented in check file generated from FRAM. FRAM does NOT count the first iteration in this number, so a value here of 14 means that there were 15 total iterations}
#'     \item{time_step_sections}{Sections of FramCheck falling under the "Chinook TAMM Time Step..." headers. Includes tamm scalers, TRS, and convergence information}
#'     \item{comp_catch_df}{Contents of the "CompCatch Results" 4-line injections in the FramCheck file for cases when those occur during TAMM iteration. These appear to be debug printings of 7BCNT and 7BCTR total landed catch.}
#'     \item{hood_canal_nonzero_catch}{Section starting with "StkFish" at the end of the FramCheck file. This appears to be summary information for stock x timestep x fishery information for catch in HC Net.}
#'     \item{tamm_fishery_scaling}{Information under the "TAMM Iter= " lines. Includes fishery and tamm scalers}
#'     \item{negative_escapement}{Dataframe of all stock x ages with negative escapements after TAMM iterations were completed. Null if there were no stocks with negative escapement.}
#'     }
#'
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

  species = get_framcheck_species(filepath)

  if(species == "COHO"){
    out <- parse_fram_check_coho(filepath)

  } else if(species == "CHINOOK"){
    out <- parse_fram_check_chinook(filepath)
  }

  return(out)
}

## Chinook-specific parsing
parse_fram_check_chinook <- function(filepath){

  tamm_name = get_framcheck_tamm(filepath)
  raw <- readLines(filepath)
  species = get_framcheck_species(filepath)

  ## cut out negative escapements
  ind_neg_esc <- which(stringr::str_detect(raw, "Negative Escapements"))
  if(length(ind_neg_esc) == 0){
    neg_esc = NULL
  } else if(length(ind_neg_esc) == 1){
    neg_esc_lines <- raw[ (ind_neg_esc+1) : length(raw) ]
    raw = raw[1:(ind_neg_esc-1)]
    neg_esc_lines <- gsub(" *Stock=", "", neg_esc_lines)
    neg_esc = data.frame(stock = gsub(" *Age=.*$", "", neg_esc_lines))
    neg_esc_lines <- gsub(".*Age=", "", neg_esc_lines)
    neg_esc$age = as.numeric(gsub(" =.*", "", neg_esc_lines))
    neg_esc$escapement = as.numeric(gsub(".* = ", "", neg_esc_lines))
  } else {
    fram_abort("Detecting multiple `Negative Escapement` sections (Lines {ind_neg_esc}). Function not designed to handle this!")
  }

  ## handling the "run is done" sections
  ind_total_iterations <- which(stringr::str_detect(raw, "Total Number of TAMM Iterations ="))[1]
  final_section <- raw[ind_total_iterations:length(raw)] |>
    ## bug in current version of fram causes many duplicate lines with the same info
    unique()
  raw <- raw[1:(ind_total_iterations-1)]

  final_iteration_count <- as.numeric(gsub("Total Number of TAMM Iterations = ", "", final_section[1]))
  final_section <- final_section[-1]

  if(!all(stringr::str_detect(final_section, "^StkFish "))){
    fram_abort("Final `StkFish` section has unexpected strings!")
  }

  hc_nonzero_catch = gsub("StkFish ", "", final_section) |>
    stringr::str_split(",") |>
    do.call(rbind, args = _) |>
    as.data.frame() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_squish)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  ## based on `PrnLine = "StkFish "...` in FRAM source
  names(hc_nonzero_catch) = c("stock_id", "fishery_id", "landed_catch", "msf_landed_catch")

  ## Iterations information
  inds_ts3_header <- which(stringr::str_detect(raw, "^Chinook TAMM Time Step 3"))
  raw = raw[inds_ts3_header[1]:length(raw)]
  iteration_groups <- cumsum(stringr::str_detect(raw, "^Chinook TAMM Time Step 3"))
  iteration_ls <- split(raw, iteration_groups)


  ## helper function for parsing ts3
  ## embedding in ..._chinook function to avoid coho vs chinook confusion
  parse_ts_3 <- function(x){
    res <- data.frame(area = x[1],
                      time_step = x[2],
                      tamm_estimate = x[3],
                      tamm_catch = x[4],
                      tamm_scaler = x[5],
                      tamm_chinook_convergence = x[6],
                      tamm_terminal_run_size = x[7]) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
    return(res)
  }

  ## helper function for parsing ts2
  ## embedding in ..._chinook function to avoid coho vs chinook confusion
  parse_ts_2 <- function(x){
    x <- gsub("Area,TS,TammEstimate,TammCatch  ", "", x)
    res <- data.frame(area = as.numeric(substr(x, 1, 2)),
                      time_step = as.numeric(substr(x, 3, 5)),
                      tamm_estimate = as.numeric(substr(x, 6, 15)))
    # tamm_catch = x[4],
    # tamm_scaler = x[5],
    # tamm_chinook_convergence = x[6])
    remaining_numeric = stringr::str_split(substr(x, 16, nchar(x)), "\\s+")[[1]]
    #removing leading empty string
    remaining_numeric = remaining_numeric[!remaining_numeric == ""] |>
      as.numeric()
    res$tamm_catch = remaining_numeric[1]
    res$tamm_scaler = remaining_numeric[2]
    res$tamm_chinook_convergence = remaining_numeric[3]
    return(res)
  }

  parse_tamm_iter <- function(x){

    partially_separated <- stringr::str_split(x, " - TStep,QEff,qf,QScale = ")[[1]]

    res <- data.frame(fishery = partially_separated[1])

    remainder <- partially_separated[2] |>
      stringr::str_split(pattern = "\\s+") |>
      unlist() |>
      as.numeric()

    res$time_step = remainder[1]
    res$fishery_scaler = remainder[2]
    res$fishery_flag = remainder[3]
    res$tamm_scaler = remainder[4]

    return(res)
  }

  ts_3_parsed_list <- list()
  ts_2_parsed_list <- list()
  comp_catch_ls <- list()
  tamm_iter_ls <- list()

  for(i in 1:length(iteration_ls)){
    ## handle "Chinook TAMM Time Step 3"
    chunk_end = which(stringr::str_detect(iteration_ls[[i]], "Chinook TAMM Time Step 2"))
    chunk_ts_3 <- iteration_ls[[i]][2:(chunk_end - 1)]
    chunk_ts_3 <- gsub("Area,TS,TammEstimate,TammCatch +", "", chunk_ts_3)
    ts_3_parsed_list[[i]] <- stringr::str_split(chunk_ts_3, "\\s+") |>
      purrr::map_df(.f = parse_ts_3) |>
      dplyr::mutate(tamm_iteration = i)

    ## handle "Chinook TAMM Time Step 2"
    chunk_ts_2 <- iteration_ls[[i]][(chunk_end+1):length(iteration_ls[[i]])]
    ## remove stuff at the end
    if(any(stringr::str_detect(chunk_ts_2, "--- TAMM Iter="))){

      ind_cut <- which(stringr::str_detect(chunk_ts_2, "--- TAMM Iter="))

      iter_line <- chunk_ts_2[ind_cut]
      tamm_iter_number <- stringr::str_extract(iter_line, "(?<=Iter=\\s?)\\d+") |>
        as.integer()

      if(tamm_iter_number != i){
        fram_abort("Parser iteration count ({i}) and iteration count printed in file ({tamm_iter_number}) do not match!")
      }

      chunk_ts_2 <- chunk_ts_2[1:(ind_cut-1)]
    }
    ts_2_parsed_list[[i]] <- purrr::map_df(chunk_ts_2, .f = parse_ts_2) |>
      dplyr::mutate(tamm_iteration = i)

    ## Handle the "TAMM Iter=" section
    chunk_tamm_iter <- iteration_ls[[i]][(chunk_end+1):length(iteration_ls[[i]])]
    tamm_iter_start <- which(stringr::str_detect(chunk_tamm_iter, "--- TAMM Iter="))

    if(length(tamm_iter_start) == 1){
      chunk_tamm_iter = chunk_tamm_iter[-(1:tamm_iter_start)]

      ## parse the compcatch interjections
      inds_comp_catch <- which(stringr::str_detect(chunk_tamm_iter, "=================== CompCatch Results"))

      inds_comp_info <- c(inds_comp_catch + 2, inds_comp_catch + 3)

      comp_info <- chunk_tamm_iter[inds_comp_info] |>
        stringr::str_split(pattern = "\\s+") |>
        purrr::map_df(\(x) {
          data.frame(
            fishery = x[1],
            time_step = as.numeric(x[2]),
            total_landed_catch = as.numeric(x[3])
          )
        }) |>
        dplyr::mutate(tamm_iteration = i)
      comp_catch_ls[[i]] <- comp_info

      ## remove the compcatch interjections - starting index -> starting index + 3
      inds_to_remove <- purrr::map(inds_comp_catch, \(x) {x : (x+3)}) |>
        unlist()

      ## parsing the TAMM iter information
      chunk_tamm_iter = chunk_tamm_iter[-inds_to_remove]

      tamm_iter_ls[[i]] <- purrr::map_df(chunk_tamm_iter,
                                         parse_tamm_iter) |>
        dplyr::mutate(tamm_iteration = i)


    }



    ## handle Chinook TAMM Time STep 2, normal cases
    ## handle Chinook TAMM Time Step 2, areas 13 and 14
    ## HAndle "TAMM Iter=" section (if present)
    ##  Within that, extract CompCatch results statements
  }

  ts_3_df <- do.call(rbind, ts_3_parsed_list)
  ts_2_df <- do.call(rbind, ts_2_parsed_list)
  ts_df <- dplyr::bind_rows(ts_3_df, ts_2_df)

  comp_catch_df <- do.call(rbind, comp_catch_ls)

  tamm_iter_df <- do.call(rbind, tamm_iter_ls)

  return(list(
    species = species,
    tamm_name = tamm_name,
    final_iteration_count = final_iteration_count,
    time_step_sections = ts_df,
    comp_catch_df = comp_catch_df,
    hood_canal_nonzero_catch = hc_nonzero_catch,
    tamm_fishery_scaling = tamm_iter_df,
    negative_escapement = neg_esc
  ))
}

## Coho-specific parsing
parse_fram_check_coho <- function(filepath){

  tamm_name = get_framcheck_tamm(filepath)
  raw <- readLines(filepath)
  species = get_framcheck_species(filepath)


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
#' Evaluates whether TAMM iterations appear to have converged. For Coho, checks how much terminal run sizes (TRS) and TAA/ETRS quotas changed
#' between the final two TAMM iterations in a `FramCheck.txt` file. For Chinook, compares total iteration count with (expected) FRAM maximum and flags any stocks with negative escapements. Prints informative messages to console; for Coho, messages can optionally be surpressed.
#'
#' For Coho, outputs "diffs" based on final two iterations, with `$change_in_fish` and `$change_in_quota` giving the number in raw fish. `relative_change` is calculated as the difference in the final two counts (TRS or abundance) divided by their average. `trs_diff$trs_id` corresponds to the `TaaNum` column of the `TAAETRSList` table in the FRAM database.
#'
#' For Chinook, the observed number of TAMM iterations is compared against the hard-coded maximum of 15 (FRAM version 2.24). In the event that
#' you are using an experimental version of FRAM or future versions of FRAM with a different maximum number of Chinook TAMM iterations,
#' you will need to evaluate the number of TAMM iterations with this in mind.
#'
#' @param filepath Path to a `FramCheck.txt` file.
#' @param threshold_fish Numeric scalar. Only used for Coho. Minimum absolute change in fish (or
#'   quota) between the last two iterations to flag as a potential convergence
#'   problem. Defaults to `20`.
#' @param quiet Logical. Only used for Coho. If `TRUE`, suppresses console output. Defaults to
#'   `FALSE`.
#'
#' @returns For Coho, Invisibly, a named list with elements:
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
#'
check_tamm_convergence <- function(filepath,
                                   threshold_fish = 20, #threshold for messaging in cli
                                   quiet = FALSE){

  validate_path(filepath)
  validate_numeric(threshold_fish, n = 1)
  validate_flag(quiet)

  species <-  get_framcheck_species(filepath)

  if(species == "COHO"){
    out <- check_tamm_convergence_coho(filepath = filepath,
                                       threshold_fish = threshold_fish,
                                       quiet = quiet)
  } else if (species == "CHINOOK"){
    out <- check_tamm_convergence_chinook(filepath = filepath)
  }

  return(invisible(out))
}

check_tamm_convergence_chinook <- function(filepath, fram_iter_max = 15){

  tamm_name <- get_framcheck_tamm(filepath)

  cli::cli_h1("Checking TAMM iteration convergence of {.file {filepath}}")
  cli::cli_alert_info("Associated with tamm {.file {tamm_name}}")

  trs_info <-parse_fram_check(filepath)
  iter_count <- trs_info$final_iteration_count+1

  cat("\n")
  if(iter_count < fram_iter_max){
    cli::cli_alert_success("FRAM stopped after {iter_count} TAMM iterations (FRAM max is {fram_iter_max}).")
  } else {
    cli::cli_alert_danger("FRAM did not stop until after {iter_count} TAMM iterations, the maximum allowed! TAMM may not have converged!")
  }

  if(!is.null(trs_info$negative_escapement)){
    cat("\n")
    cli::cli_alert_danger("The following stocks had negative escapement after iterating!")
    print(trs_info$negative_escapement)
  }


}

check_tamm_convergence_coho <- function(filepath,
                                        threshold_fish = 20, #threshold for messaging in cli
                                        quiet = FALSE){ #print to cli?


  tamm_name <- get_framcheck_tamm(filepath)

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

  return(list(trs_diff = trs_final_diff,
              taa_etrs_diff = taa_final_diff))
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
