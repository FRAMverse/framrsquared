## Creating and storing data objects for internal use
## Because it's INTERNAL, stored as a single .rda file
##   so use_data should only be called once in package.

# this imports the groups for the stock composition chart

coho_stock_comp_lut <- readr::read_csv("data-raw/coho_lut.csv") |>
  janitor::clean_names() |>
  dplyr::select(stock_id, stock_group)


# this imports the data for the SONCC lookup table

fishery_coho_soncc <- readxl::read_excel("data-raw/SONCC_fishery_lookup.xlsx") |>
  janitor::clean_names() |>
  dplyr::select(fishery_id, region, gear, area, factor) |>
  dplyr::filter(!is.na(.data$factor)) |>
  dplyr::arrange(factor)

coho_stock_marlene <- readr::read_csv("data-raw/fram_coho_stocks.csv") |>
  janitor::clean_names() |>
  dplyr::select(stock_id, stock_type = type)

usethis::use_data(coho_stock_comp_lut,
  coho_stock_marlene,
  fishery_coho_soncc,
  overwrite = TRUE, internal = TRUE
)
